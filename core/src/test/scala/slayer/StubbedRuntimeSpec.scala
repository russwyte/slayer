package slayer

import zio.*
import zio.test.*

object StubbedRuntimeSpec extends ZIOSpecDefault:

  /** A bare service used to materialize a `Stubbed[A]` without any macro involvement. */
  trait NoOp
  final class NoOpStubbed extends Stubbed[NoOp]

  def spec = suite("StubbedRuntimeSpec — M0 runtime, no macros")(
    suite("MethodId")(
      test("equal when jvmName and erasedParams match") {
        assertTrue(MethodId("foo", List("Int", "String")) == MethodId("foo", List("Int", "String")))
      },
      test("not equal when jvmName differs") {
        assertTrue(MethodId("foo", Nil) != MethodId("bar", Nil))
      },
      test("not equal when erasedParams differ") {
        assertTrue(MethodId("foo", List("Int")) != MethodId("foo", List("String")))
      },
      test("no-arg method has empty erasedParams") {
        val id = MethodId("now", Nil)
        assertTrue(id.erasedParams.isEmpty) && assertTrue(id == MethodId("now", Nil))
      },
      test("erased order matters") {
        assertTrue(MethodId("f", List("Int", "String")) != MethodId("f", List("String", "Int")))
      },
    ),
    suite("StubResult.Value")(
      test("call ignores args, returns stored value") {
        val r = StubResult.Value(42)
        assertTrue(r.call(Array.empty) == 42) &&
        assertTrue(r.call(Array("ignored", 1, 2)) == 42)
      },
      test("can store null") {
        val r = StubResult.Value(null)
        assertTrue(r.call(Array.empty) == null)
      },
      test("can store Unit") {
        val r = StubResult.Value(())
        assertTrue(r.call(Array.empty) == (()))
      },
      test("returns ZIO effect without running it") {
        var ran              = false
        val effect: UIO[Int] = ZIO.succeed { ran = true; 7 }
        val r                = StubResult.Value(effect)
        // Calling the stub returns the effect, but does not execute it.
        val got = r.call(Array.empty)
        assertTrue(!ran) &&
        assertTrue(got.asInstanceOf[UIO[Int]] eq effect)
      },
    ),
    suite("StubResult.Func")(
      test("arity 0 — empty array, returns constant") {
        val r = StubResult.Func(_ => "zero")
        assertTrue(r.call(Array.empty) == "zero")
      },
      test("arity 1 — passes through") {
        val r = StubResult.Func(args => args(0).asInstanceOf[Int] + 1)
        assertTrue(r.call(Array[Any](41)) == 42)
      },
      test("arity 5 — heterogeneous types") {
        val r = StubResult.Func { args =>
          val i = args(0).asInstanceOf[Int]
          val s = args(1).asInstanceOf[String]
          val b = args(2).asInstanceOf[Boolean]
          val d = args(3).asInstanceOf[Double]
          val l = args(4).asInstanceOf[Long]
          s"$i|$s|$b|$d|$l"
        }
        assertTrue(r.call(Array[Any](1, "x", true, 2.5, 9L)) == "1|x|true|2.5|9")
      },
      test("arity 12 — proves no ceiling at the runtime layer") {
        val r = StubResult.Func { args =>
          (0 until 12).map(i => args(i).asInstanceOf[Int]).sum
        }
        val got = r.call(Array[Any](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
        assertTrue(got == (1 to 12).sum)
      },
      test("arity 25 — past Function22, still fine for Array adapter") {
        val r = StubResult.Func { args =>
          (0 until 25).map(i => args(i).asInstanceOf[Int]).sum
        }
        val got = r.call(Array.tabulate(25)(_ + 1).map(_.asInstanceOf[Any]))
        assertTrue(got == (1 to 25).sum)
      },
    ),
    suite("Stubbed.callStubbed")(
      test("throws NoStub when key missing") {
        val s      = NoOpStubbed()
        val id     = MethodId("missing", Nil)
        val caught =
          try
            s.callStubbed[Int](id, Array.empty); None
          catch case e: SlayerError.NoStub => Some(e)
        assertTrue(caught.isDefined) &&
        assertTrue(caught.get.id == id)
      },
      test("returns value after insertValueImpl") {
        val s  = NoOpStubbed()
        val id = MethodId("foo", Nil)
        for _ <- s.insertValueImpl(id, "hello")
        yield assertTrue(s.callStubbed[String](id, Array.empty) == "hello")
      },
      test("returns func result after insertFuncImpl") {
        val s  = NoOpStubbed()
        val id = MethodId("add", List("Int", "Int"))
        for _ <- s.insertFuncImpl(id, args => args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int])
        yield assertTrue(s.callStubbed[Int](id, Array[Any](2, 3)) == 5)
      },
      test("second insert wins (last-write semantics)") {
        val s  = NoOpStubbed()
        val id = MethodId("foo", Nil)
        for
          _ <- s.insertValueImpl(id, 1)
          _ <- s.insertValueImpl(id, 2)
        yield assertTrue(s.callStubbed[Int](id, Array.empty) == 2)
      },
      test("Func can replace Value (and vice versa)") {
        val s  = NoOpStubbed()
        val id = MethodId("foo", Nil)
        for
          _ <- s.insertValueImpl(id, 1)
          v1 = s.callStubbed[Int](id, Array.empty)
          _ <- s.insertFuncImpl(id, _ => 99)
          v2 = s.callStubbed[Int](id, Array.empty)
        yield assertTrue(v1 == 1) && assertTrue(v2 == 99)
      },
      test("thread-safety smoke: 100 parallel inserts and reads do not lose writes") {
        val s = NoOpStubbed()
        for
          _ <- ZIO.foreachParDiscard(1 to 100) { i =>
            s.insertValueImpl(MethodId(s"m$i", Nil), i)
          }
          got <- ZIO.foreachPar((1 to 100).toList) { i =>
            ZIO.attempt(s.callStubbed[Int](MethodId(s"m$i", Nil), Array.empty))
          }
        yield assertTrue(got.sorted == (1 to 100).toList)
      },
    ),
    suite("Stubbed companion object — ZIO accessors")(
      test("insertValue routes through the service layer") {
        val id                                = MethodId("foo", Nil)
        val program: URIO[Stubbed[NoOp], Int] =
          for
            _ <- Stubbed.insertValue[NoOp, Int](id, 7)
            s <- ZIO.service[Stubbed[NoOp]]
          yield s.callStubbed[Int](id, Array.empty)

        for got <- program.provide(ZLayer.succeed[Stubbed[NoOp]](NoOpStubbed()))
        yield assertTrue(got == 7)
      },
      test("insertFunc routes through the service layer") {
        val id                                = MethodId("add", List("Int"))
        val program: URIO[Stubbed[NoOp], Int] =
          for
            _ <- Stubbed.insertFunc[NoOp](id, args => args(0).asInstanceOf[Int] * 10)
            s <- ZIO.service[Stubbed[NoOp]]
          yield s.callStubbed[Int](id, Array[Any](4))

        for got <- program.provide(ZLayer.succeed[Stubbed[NoOp]](NoOpStubbed()))
        yield assertTrue(got == 40)
      },
    ),
    suite("SlayerError")(
      test("NoStub message includes the id") {
        val id  = MethodId("foo", List("Int"))
        val err = SlayerError.NoStub(id)
        assertTrue(err.getMessage.contains("foo")) && assertTrue(err.getMessage.contains("Int"))
      }
    ),
    suite("tracing (runtime)")(
      test("quiet by default — no calls recorded") {
        val s  = NoOpStubbed()
        val id = MethodId("foo", Nil)
        for _ <- s.insertValueImpl(id, 1)
        yield
          val _ = s.callStubbed[Int](id, Array.empty)
          assertTrue(!s.isTracing, s.calls.isEmpty)
      },
      test("enableTracing records Call with args") {
        val s  = NoOpStubbed()
        val id = MethodId("add", List("Int", "Int"))
        s.enableTracing()
        for _ <- s.insertFuncImpl(id, args => args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int])
        yield
          val got = s.callStubbed[Int](id, Array[Any](2, 3))
          assertTrue(
            got == 5,
            s.calls == Chunk(Call(id, List(2, 3))),
          )
      },
      test("callStubbedOrElse records even on fall-through") {
        val s  = NoOpStubbed()
        val id = MethodId("x", Nil)
        s.enableTracing()
        val got = s.callStubbedOrElse[String](id, Array.empty, "fallback")
        assertTrue(got == "fallback", s.calls == Chunk(Call(id, Nil)))
      },
      test("callStubbedOrElse records when stub is present (not only fall-through)") {
        val s  = NoOpStubbed()
        val id = MethodId("x", Nil)
        s.enableTracing()
        for _ <- s.insertValueImpl(id, "stubbed")
        yield
          val got = s.callStubbedOrElse[String](id, Array[Any](1), "fallback")
          assertTrue(
            got == "stubbed",
            s.calls == Chunk(Call(id, List(1))),
          )
      },
      test("NoStub still records the invoke before throwing") {
        val s  = NoOpStubbed()
        val id = MethodId("missing", List("Int"))
        s.enableTracing()
        val caught =
          try
            s.callStubbed[Int](id, Array[Any](7)); None
          catch case e: Exception => Some(e)
        assertTrue(
          caught.exists(_.isInstanceOf[SlayerError.NoStub]),
          s.calls == Chunk(Call(id, List(7))),
        )
      },
      test("clearCalls does not remove stubs") {
        val s  = NoOpStubbed()
        val id = MethodId("foo", Nil)
        s.enableTracing()
        for _ <- s.insertValueImpl(id, 42)
        yield
          val _ = s.callStubbed[Int](id, Array.empty)
          s.clearCalls()
          assertTrue(s.calls.isEmpty, s.callStubbed[Int](id, Array.empty) == 42)
      },
      test("callsFor filters; empty for unknown id") {
        val s   = NoOpStubbed()
        val idA = MethodId("a", Nil)
        val idB = MethodId("b", List("Int"))
        s.enableTracing()
        for
          _ <- s.insertValueImpl(idA, 1)
          _ <- s.insertFuncImpl(idB, args => args(0))
        yield
          val _ = s.callStubbed[Int](idA, Array.empty)
          val _ = s.callStubbed[Int](idB, Array[Any](9))
          val _ = s.callStubbed[Int](idA, Array.empty)
          assertTrue(
            s.callsFor(idA).size == 2,
            s.callsFor(idB).map(_.args) == Chunk(List(9)),
            s.callsFor(MethodId("nope", Nil)).isEmpty,
          )
        end for
      },
      test("ZIO result is returned as-is under tracing (not forced at call time)") {
        val s  = NoOpStubbed()
        val id = MethodId("load", Nil)
        s.enableTracing()
        var ran              = false
        val raw: UIO[String] = ZIO.succeed { ran = true; "ok" }
        for _ <- s.insertValueImpl(id, raw)
        yield
          val effect = s.callStubbed[UIO[String]](id, Array.empty)
          val mid    = ran
          assertTrue(
            !mid,
            effect eq raw, // identity — no debug wrap
            s.calls == Chunk(Call(id, Nil)),
          )
      },
      test("failing ZIO still fails under tracing; invoke is recorded") {
        val s  = NoOpStubbed()
        val id = MethodId("fail", Nil)
        s.enableTracing()
        val raw: IO[String, Int] = ZIO.fail("nope")
        for
          _    <- s.insertValueImpl(id, raw)
          exit <- s.callStubbed[IO[String, Int]](id, Array.empty).exit
        yield assertTrue(exit == Exit.fail("nope"), s.calls.size == 1)
      },
      test("pure result is identity under tracing") {
        val s  = NoOpStubbed()
        val id = MethodId("n", Nil)
        s.enableTracing()
        for _ <- s.insertValueImpl(id, 7)
        yield assertTrue(s.callStubbed[Int](id, Array.empty) == 7)
      },
      test("parallel callStubbed appends without losing entries") {
        val s  = NoOpStubbed()
        val id = MethodId("f", List("Int"))
        s.enableTracing()
        for
          _ <- s.insertFuncImpl(id, args => args(0))
          _ <- ZIO.foreachParDiscard(1 to 100) { i =>
            ZIO.succeed(s.callStubbed[Int](id, Array[Any](i)))
          }
        yield
          val args = s.calls.map(_.args.head.asInstanceOf[Int]).toSet
          assertTrue(s.calls.size == 100, args == (1 to 100).toSet)
      },
      test("Call equality is structural") {
        assertTrue(
          Call(MethodId("f", List("Int")), List(1)) == Call(MethodId("f", List("Int")), List(1)),
          Call(MethodId("f", List("Int")), List(1)) != Call(MethodId("f", List("Int")), List(2)),
          Call(MethodId("f", Nil), Nil) != Call(MethodId("g", Nil), Nil),
        )
      },
    ),
  )
end StubbedRuntimeSpec
