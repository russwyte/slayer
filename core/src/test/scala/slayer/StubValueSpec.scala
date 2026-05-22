package slayer

import zio.*
import zio.test.*

/** M2 unit tests: drive the value-result branch of `stub`.
  *
  * The synthesized service class doesn't exist yet (M4), so we materialize a `Stubbed[Service]` directly via a
  * hand-rolled minimal subtype and feed it into the layer that `stub` produces. After the effect runs, we verify the
  * stub landed in the map by calling `callStubbed` with the same `MethodId` we expect the macro to have generated.
  */
object StubValueSpec extends ZIOSpecDefault:

  // ----- Service traits ----------------------------------------------------------------------------------------------

  trait Repo:
    def now: Long
    def find(id: Int): Option[String]
    def merge(a: Int, b: Int, c: Int): Int
    def composed(a: Int)(b: String)(c: Boolean): String
    def get[A](id: Int): UIO[A]
    def label: String

  /** Bare runtime store used as the test layer. The macro routes through `Stubbed.insertValue`, which calls
    * `insertValueImpl` on whatever instance the layer provides — so we just need any `Stubbed[Repo]`.
    */
  final class RepoStubbed extends Stubbed[Repo]

  // ----- Helpers -----------------------------------------------------------------------------------------------------

  private def runStub[A](program: URIO[Stubbed[Repo], A]): UIO[(A, RepoStubbed)] =
    val store = RepoStubbed()
    program.provide(ZLayer.succeed[Stubbed[Repo]](store)).map(a => (a, store))

  def spec = suite("StubValueSpec — M2 stub macro, value branch")(
    test("0-arg method") {
      for (_, store) <- runStub(stub[Repo](_.now)(123L))
      yield assertTrue(store.callStubbed[Long](MethodId("now", Nil), Array.empty) == 123L)
    },
    test("1-arg method") {
      for (_, store) <- runStub(stub[Repo](_.find(slayer.any))(Some("hello")))
      yield assertTrue(
        store.callStubbed[Option[String]](MethodId("find", List("Int")), Array[Any](42)) == Some("hello")
      )
    },
    test("3-arg single-list method") {
      for (_, store) <- runStub(stub[Repo](_.merge(slayer.any, slayer.any, slayer.any))(7))
      yield assertTrue(
        store.callStubbed[Int](
          MethodId("merge", List("Int", "Int", "Int")),
          Array[Any](1, 2, 3),
        ) == 7
      )
    },
    test("curried 3-list method — value insert ignores list shape") {
      for (_, store) <- runStub(stub[Repo](_.composed(slayer.any)(slayer.any)(slayer.any))("hi"))
      yield assertTrue(
        store.callStubbed[String](
          MethodId("composed", List("Int", "String", "Boolean")),
          Array.empty,
        ) == "hi"
      )
    },
    test("generic method — selector pins type arg, ZIO effect stored as the value") {
      val effect: UIO[String] = ZIO.succeed("x")
      for
        (_, store) <- runStub(stub[Repo](_.get[String](slayer.any))(effect))
        got        <- store.callStubbed[UIO[String]](MethodId("get", List("Int")), Array[Any](7))
      yield assertTrue(got == "x")
    },
    test("non-effect (pure) return") {
      for (_, store) <- runStub(stub[Repo](_.label)("LBL"))
      yield assertTrue(store.callStubbed[String](MethodId("label", Nil), Array.empty) == "LBL")
    },
    test("second insert wins") {
      val s                                  = RepoStubbed()
      val program: URIO[Stubbed[Repo], Unit] =
        for
          _ <- stub[Repo](_.now)(1L)
          _ <- stub[Repo](_.now)(2L)
        yield ()
      for _ <- program.provide(ZLayer.succeed[Stubbed[Repo]](s))
      yield assertTrue(s.callStubbed[Long](MethodId("now", Nil), Array.empty) == 2L)
    },
    test("compile error: result type doesn't match method return") {
      for r <- typeCheck("slayer.stub[slayer.StubValueSpec.Repo](_.now)(\"nope\")")
      yield assertTrue(r.left.exists(_.contains("does not match")))
    },
    test("compile error: selector doesn't reference a method") {
      for r <- typeCheck("slayer.stub[slayer.StubValueSpec.Repo](_ => 42)(99L)")
      yield assertTrue(r.left.exists(_.contains("must reference a method")))
    },
  )
end StubValueSpec
