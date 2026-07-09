package slayer

import scala.annotation.targetName

import zio.*
import zio.test.*

/** Thorough coverage of call recording (`stubbedTraced` / `enableTracing`).
  *
  * Recording happens at invoke time on `callStubbed` / `callStubbedOrElse`. Results are returned unchanged.
  */
object TracingSpec extends ZIOSpecDefault:

  // ----- fixtures ----------------------------------------------------------------------------------------------------

  trait Repo:
    def now: Long
    def find(id: Int): String
    def load(id: Int): UIO[String]
    def failLoad(id: Int): IO[String, String]
    def merge(a: Int, b: String): Int
    def composed(a: Int)(b: String)(c: Boolean): String
    def lookup(k: Int)(using s: String): Int
    def get[A](id: Int): UIO[A]

  trait Greeter:
    def name: String
    def hello: String = s"hello, $name"

  trait Overloaded:
    def find(id: Int): String
    def find(name: String): String

  trait Renamed:
    @targetName("bang") def !(i: Int): Int

  trait Base:
    def fromBase(i: Int): String

  trait Child extends Base:
    def fromChild(s: String): String

  trait SiblingA:
    def ping: String

  trait SiblingB:
    def pong: Int

  // ----- helpers -----------------------------------------------------------------------------------------------------

  def callsOf[S: Tag]: URIO[Stubbed[S], Chunk[Call]] =
    ZIO.serviceWith[Stubbed[S]](_.calls)

  // ----- suites ------------------------------------------------------------------------------------------------------

  def spec = suite("TracingSpec — call log")(
    suite("quiet vs traced")(
      test("stubbed is quiet — isTracing false and no calls") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))("x")
            store <- ZIO.service[Stubbed[Repo]]
            _     <- ZIO.serviceWith[Repo](_.find(1))
          yield (store.isTracing, store.calls)
        for tup <- program.provide(stubbed[Repo])
        yield assertTrue(!tup._1, tup._2.isEmpty)
      },
      test("stubbedTraced enables tracing") {
        for store <- ZIO.service[Stubbed[Repo]].provide(stubbedTraced[Repo])
        yield assertTrue(store.isTracing)
      },
      test("enableTracing can be turned on after construction on a quiet layer") {
        val program =
          for
            store <- ZIO.service[Stubbed[Repo]]
            _     <- stub[Repo](_.find(slayer.any[Int]))("x")
            _     <- ZIO.serviceWith[Repo](_.find(1))
            before = store.calls
            _      = store.enableTracing()
            mid    = store.isTracing
            _     <- ZIO.serviceWith[Repo](_.find(2))
            after  = store.calls
          yield (before, mid, after)
        for tup <- program.provide(stubbed[Repo])
        yield assertTrue(
          tup._1.isEmpty,
          tup._2,
          tup._3 == Chunk(Call(MethodId("find", List("Int")), List(2))),
        )
      },
      test("enableTracing is idempotent — still records after second call") {
        val program =
          for
            store <- ZIO.service[Stubbed[Repo]]
            _      = store.enableTracing()
            _      = store.enableTracing()
            _     <- stub[Repo](_.now)(1L)
            _     <- ZIO.serviceWith[Repo](_.now)
            _     <- ZIO.serviceWith[Repo](_.now)
          yield store.calls
        for calls <- program.provide(stubbed[Repo])
        yield assertTrue(calls.size == 2, calls.forall(_.id == MethodId("now", Nil)))
      },
    ),
    suite("Call structure and MethodId")(
      test("records method id and args in call order (oldest first)") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))("x")
            _     <- ZIO.serviceWith[Repo](_.find(7))
            _     <- ZIO.serviceWith[Repo](_.find(8))
            _     <- ZIO.serviceWith[Repo](_.find(9))
            calls <- callsOf[Repo]
          yield calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          calls.size == 3,
          calls(0) == Call(MethodId("find", List("Int")), List(7)),
          calls(1) == Call(MethodId("find", List("Int")), List(8)),
          calls(2) == Call(MethodId("find", List("Int")), List(9)),
        )
      },
      test("0-arg method records empty args") {
        val program =
          for
            _     <- stub[Repo](_.now)(42L)
            _     <- ZIO.serviceWith[Repo](_.now)
            calls <- callsOf[Repo]
          yield calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield assertTrue(calls == Chunk(Call(MethodId("now", Nil), Nil)))
      },
      test("multi-arg single list — flat args in declaration order") {
        val program =
          for
            _     <- stub[Repo](_.merge(slayer.any[Int], slayer.any[String]))(99)
            _     <- ZIO.serviceWith[Repo](_.merge(1, "a"))
            calls <- callsOf[Repo]
          yield calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield assertTrue(calls == Chunk(Call(MethodId("merge", List("Int", "String")), List(1, "a"))))
      },
      test("curried multi-list method — flat args across lists") {
        val program =
          for
            _     <- stub[Repo](_.composed(slayer.any)(slayer.any)(slayer.any))("ok")
            _     <- ZIO.serviceWith[Repo](_.composed(1)("x")(true))
            calls <- callsOf[Repo]
          yield calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          calls == Chunk(Call(MethodId("composed", List("Int", "String", "Boolean")), List(1, "x", true)))
        )
      },
      test("using clause is dropped from recorded args (and from MethodId)") {
        given String = "ctx"
        val program  =
          for
            _     <- stub[Repo](_.lookup(slayer.any))(99)
            _     <- ZIO.serviceWith[Repo](_.lookup(7))
            calls <- callsOf[Repo]
          yield calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          // only the value param `k` — not the using String
          calls == Chunk(Call(MethodId("lookup", List("Int")), List(7)))
        )
      },
      test("generic method records erased MethodId and value args") {
        val program =
          for
            _     <- stub[Repo](_.get[String](slayer.any))(ZIO.succeed("hi"))
            _     <- ZIO.serviceWithZIO[Repo](_.get[String](42))
            calls <- callsOf[Repo]
          yield calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          calls.size == 1,
          calls(0).id.jvmName == "get",
          calls(0).args == List(42),
        )
      },
      test("plain overloads record distinct MethodIds") {
        val program =
          for
            _     <- stub[Overloaded](_.find(slayer.any[Int]))("by-int")
            _     <- stub[Overloaded](_.find(slayer.any[String]))("by-string")
            _     <- ZIO.serviceWith[Overloaded](_.find(1))
            _     <- ZIO.serviceWith[Overloaded](_.find("x"))
            calls <- callsOf[Overloaded]
          yield calls
        for calls <- program.provide(stubbedTraced[Overloaded])
        yield assertTrue(
          calls.size == 2,
          calls(0) == Call(MethodId("find", List("Int")), List(1)),
          calls(1) == Call(MethodId("find", List("String")), List("x")),
        )
      },
      test("@targetName — recorded jvmName is the target name") {
        val program =
          for
            _     <- stub[Renamed](_.!(slayer.any[Int]))(99)
            _     <- ZIO.serviceWith[Renamed](_.!(3))
            calls <- callsOf[Renamed]
          yield calls
        for calls <- program.provide(stubbedTraced[Renamed])
        yield assertTrue(calls == Chunk(Call(MethodId("bang", List("Int")), List(3))))
      },
      test("inherited parent methods are recorded with the same MethodId shape") {
        val program =
          for
            _     <- stub[Child](_.fromBase(slayer.any[Int]))("b")
            _     <- stub[Child](_.fromChild(slayer.any[String]))("c")
            _     <- ZIO.serviceWith[Child](_.fromBase(1))
            _     <- ZIO.serviceWith[Child](_.fromChild("z"))
            calls <- callsOf[Child]
          yield calls
        for calls <- program.provide(stubbedTraced[Child])
        yield assertTrue(
          calls == Chunk(
            Call(MethodId("fromBase", List("Int")), List(1)),
            Call(MethodId("fromChild", List("String")), List("z")),
          )
        )
      },
    ),
    suite("callsFor / clearCalls / snapshots")(
      test("callsFor filters by MethodId") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))("x")
            _     <- stub[Repo](_.merge(slayer.any[Int], slayer.any[String]))(0)
            _     <- ZIO.serviceWith[Repo](_.find(1))
            _     <- ZIO.serviceWith[Repo](_.merge(2, "b"))
            _     <- ZIO.serviceWith[Repo](_.find(3))
            store <- ZIO.service[Stubbed[Repo]]
          yield (
            store.callsFor(MethodId("find", List("Int"))),
            store.callsFor(MethodId("merge", List("Int", "String"))),
            store.callsFor(MethodId("missing", Nil)),
          )
        for tup <- program.provide(stubbedTraced[Repo])
        yield
          val (finds, merges, missing) = tup
          assertTrue(
            finds.map(_.args) == Chunk(List(1), List(3)),
            merges.map(_.args) == Chunk(List(2, "b")),
            missing.isEmpty,
          )
      },
      test("callsFor distinguishes overloads that share a source name") {
        val program =
          for
            _     <- stub[Overloaded](_.find(slayer.any[Int]))("i")
            _     <- stub[Overloaded](_.find(slayer.any[String]))("s")
            _     <- ZIO.serviceWith[Overloaded](_.find(1))
            _     <- ZIO.serviceWith[Overloaded](_.find("a"))
            _     <- ZIO.serviceWith[Overloaded](_.find(2))
            store <- ZIO.service[Stubbed[Overloaded]]
          yield (
            store.callsFor(MethodId("find", List("Int"))),
            store.callsFor(MethodId("find", List("String"))),
          )
        for tup <- program.provide(stubbedTraced[Overloaded])
        yield assertTrue(
          tup._1.map(_.args) == Chunk(List(1), List(2)),
          tup._2.map(_.args) == Chunk(List("a")),
        )
      },
      test("clearCalls empties the log but keeps stubs") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))("still-there")
            _     <- ZIO.serviceWith[Repo](_.find(1))
            store <- ZIO.service[Stubbed[Repo]]
            _      = store.clearCalls()
            after  = store.calls
            again <- ZIO.serviceWith[Repo](_.find(2))
            more   = store.calls
          yield (after, again, more)
        for tup <- program.provide(stubbedTraced[Repo])
        yield
          val (after, again, more) = tup
          assertTrue(
            after.isEmpty,
            again == "still-there",
            more == Chunk(Call(MethodId("find", List("Int")), List(2))),
          )
      },
      test("clearCalls is safe when already empty") {
        val program =
          for
            store <- ZIO.service[Stubbed[Repo]]
            _      = store.clearCalls()
            _      = store.clearCalls()
          yield store.calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield assertTrue(calls.isEmpty)
      },
      test("calls snapshot is independent of later mutations") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))("x")
            _     <- ZIO.serviceWith[Repo](_.find(1))
            store <- ZIO.service[Stubbed[Repo]]
            snap   = store.calls
            _      = store.clearCalls()
            _     <- ZIO.serviceWith[Repo](_.find(2))
          yield (snap, store.calls)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          tup._1 == Chunk(Call(MethodId("find", List("Int")), List(1))),
          tup._2 == Chunk(Call(MethodId("find", List("Int")), List(2))),
        )
      },
    ),
    suite("fall-through, NoStub, handlers")(
      test("fall-through concrete method is recorded; nested abstract read is recorded too") {
        val program =
          for
            _     <- stub[Greeter](_.name)("ada")
            got   <- ZIO.serviceWith[Greeter](_.hello)
            calls <- callsOf[Greeter]
          yield (got, calls.map(c => c.id.jvmName -> c.args))
        for tup <- program.provide(stubbedTraced[Greeter])
        yield assertTrue(
          tup._1 == "hello, ada",
          // order: hello first (SUT call), then name (from trait body)
          tup._2 == Chunk("hello" -> Nil, "name" -> Nil),
        )
      },
      test("overriding a concrete method records only that method (no nested super path)") {
        val program =
          for
            _     <- stub[Greeter](_.name)("ada")
            _     <- stub[Greeter](_.hello)("howdy")
            got   <- ZIO.serviceWith[Greeter](_.hello)
            calls <- callsOf[Greeter]
          yield (got, calls)
        for tup <- program.provide(stubbedTraced[Greeter])
        yield assertTrue(
          tup._1 == "howdy",
          tup._2 == Chunk(Call(MethodId("hello", Nil), Nil)),
        )
      },
      test("NoStub still records the invoke before throwing") {
        val program =
          for
            store <- ZIO.service[Stubbed[Repo]]
            exit  <- ZIO.serviceWith[Repo](_.find(99)).exit
            calls  = store.calls
          yield (exit, calls)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          tup._1.causeOption.exists(_.dieOption.exists(_.isInstanceOf[SlayerError.NoStub])),
          tup._2 == Chunk(Call(MethodId("find", List("Int")), List(99))),
        )
      },
      test("function-handler stubs still record args") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))((id: Int) => s"id=$id")
            a     <- ZIO.serviceWith[Repo](_.find(3))
            b     <- ZIO.serviceWith[Repo](_.find(4))
            calls <- callsOf[Repo]
          yield (a, b, calls)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          tup._1 == "id=3",
          tup._2 == "id=4",
          tup._3 == Chunk(
            Call(MethodId("find", List("Int")), List(3)),
            Call(MethodId("find", List("Int")), List(4)),
          ),
        )
      },
      test("re-stub mid-test — later invokes use new stub; all calls still logged") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))("first")
            a     <- ZIO.serviceWith[Repo](_.find(1))
            _     <- stub[Repo](_.find(slayer.any[Int]))("second")
            b     <- ZIO.serviceWith[Repo](_.find(2))
            calls <- callsOf[Repo]
          yield (a, b, calls)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          tup._1 == "first",
          tup._2 == "second",
          tup._3.size == 2,
          tup._3.map(_.args) == Chunk(List(1), List(2)),
        )
      },
    ),
    suite("isolation")(
      test("two traced service layers do not share call logs") {
        val program =
          for
            _  <- stub[SiblingA](_.ping)("a")
            _  <- stub[SiblingB](_.pong)(1)
            _  <- ZIO.serviceWith[SiblingA](_.ping)
            _  <- ZIO.serviceWith[SiblingB](_.pong)
            ca <- callsOf[SiblingA]
            cb <- callsOf[SiblingB]
          yield (ca, cb)
        for tup <- program.provide(stubbedTraced[SiblingA], stubbedTraced[SiblingB])
        yield assertTrue(
          tup._1 == Chunk(Call(MethodId("ping", Nil), Nil)),
          tup._2 == Chunk(Call(MethodId("pong", Nil), Nil)),
        )
      },
      test("quiet and traced layers for different services coexist") {
        val program =
          for
            _  <- stub[SiblingA](_.ping)("a")
            _  <- stub[SiblingB](_.pong)(1)
            _  <- ZIO.serviceWith[SiblingA](_.ping)
            _  <- ZIO.serviceWith[SiblingB](_.pong)
            ca <- ZIO.serviceWith[Stubbed[SiblingA]](s => (s.isTracing, s.calls))
            cb <- ZIO.serviceWith[Stubbed[SiblingB]](s => (s.isTracing, s.calls))
          yield (ca, cb)
        for tup <- program.provide(stubbed[SiblingA], stubbedTraced[SiblingB])
        yield assertTrue(
          !tup._1._1 && tup._1._2.isEmpty,
          tup._2._1 && tup._2._2 == Chunk(Call(MethodId("pong", Nil), Nil)),
        )
      },
    ),
    suite("concurrency")(
      test("parallel invokes all appear in the call log") {
        val program =
          for
            _     <- stub[Repo](_.find(slayer.any[Int]))((id: Int) => s"$id")
            _     <- ZIO.foreachParDiscard(1 to 50) { i =>
              ZIO.serviceWith[Repo](_.find(i))
            }
            calls <- callsOf[Repo]
          yield calls
        for calls <- program.provide(stubbedTraced[Repo])
        yield
          val args = calls.map(_.args.head.asInstanceOf[Int]).toSet
          assertTrue(
            calls.size == 50,
            args == (1 to 50).toSet,
            calls.forall(_.id == MethodId("find", List("Int"))),
          )
      },
    ),
    suite("effect stubs under tracing")(
      test("effectful value stub returns correct result and is recorded") {
        val program =
          for
            _     <- stub[Repo](_.load(slayer.any[Int]))(ZIO.succeed("ok"))
            out   <- ZIO.serviceWithZIO[Repo](_.load(5))
            calls <- callsOf[Repo]
          yield (out, calls)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          tup._1 == "ok",
          tup._2 == Chunk(Call(MethodId("load", List("Int")), List(5))),
        )
      },
      test("effectful handler stub returns correct result under tracing") {
        val program =
          for
            _   <- stub[Repo](_.load(slayer.any[Int]))((id: Int) => ZIO.succeed(s"id=$id"))
            out <- ZIO.serviceWithZIO[Repo](_.load(11))
          yield out
        for out <- program.provide(stubbedTraced[Repo])
        yield assertTrue(out == "id=11")
      },
      test("returned effect is not forced at callStubbed — only when run") {
        var ran = false
        val program =
          for
            _      <- stub[Repo](_.load(slayer.any[Int]))(ZIO.succeed { ran = true; "x" })
            effect <- ZIO.serviceWith[Repo](_.load(1))
            mid     = ran
            out    <- effect
          yield (mid, out, ran)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(tup._1 == false, tup._2 == "x", tup._3 == true)
      },
      test("failing ZIO still fails; invoke is recorded") {
        val program =
          for
            _     <- stub[Repo](_.failLoad(slayer.any[Int]))(ZIO.fail("boom"))
            exit  <- ZIO.serviceWithZIO[Repo](_.failLoad(1)).exit
            calls <- callsOf[Repo]
          yield (exit, calls)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(
          tup._1 == Exit.fail("boom"),
          tup._2 == Chunk(Call(MethodId("failLoad", List("Int")), List(1))),
        )
      },
      test("pure (non-ZIO) return is unchanged under tracing") {
        val program =
          for
            _   <- stub[Repo](_.find(slayer.any[Int]))("pure")
            out <- ZIO.serviceWith[Repo](_.find(0))
          yield out
        for out <- program.provide(stubbedTraced[Repo])
        yield assertTrue(out == "pure")
      },
      test("generic effect method preserves success value") {
        val program =
          for
            _   <- stub[Repo](_.get[Int](slayer.any))(ZIO.succeed(123))
            out <- ZIO.serviceWithZIO[Repo](_.get[Int](9))
          yield out
        for out <- program.provide(stubbedTraced[Repo])
        yield assertTrue(out == 123)
      },
      test("same returned effect can be run more than once") {
        val program =
          for
            _      <- stub[Repo](_.load(slayer.any[Int]))(ZIO.succeed("once"))
            effect <- ZIO.serviceWith[Repo](_.load(1))
            a      <- effect
            b      <- effect
          yield (a, b)
        for tup <- program.provide(stubbedTraced[Repo])
        yield assertTrue(tup == ("once", "once"))
      },
    ),
  )
end TracingSpec
