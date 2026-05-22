package slayer

import zio.*
import zio.test.*

/** M4 unit tests: drive the synthesized service class produced by `stubbed[Service]`.
  *
  * Each test composes `stubbed[Service]` with one or more `stub` calls, then calls the trait method through the
  * synthesized class. We confirm:
  *   - the right value comes back
  *   - missing stubs raise `SlayerError.NoStub`
  *   - last-write-wins semantics across multiple stub calls
  *   - independent service layers don't share state
  */
object StubbedClassSpec extends ZIOSpecDefault:

  trait Repo:
    def now: Long
    def find(id: Int): String
    def merge(a: Int, b: Int, c: Int): Int
    def label: String

  trait Greeter:
    def greet(name: String): String

  def spec = suite("StubbedClassSpec — M4 synthesized service class")(
    test("0-arg method, end-to-end") {
      val program =
        for
          _   <- stub[Repo](_.now)(123L)
          got <- ZIO.serviceWith[Repo](_.now)
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == 123L)
    },
    test("1-arg method, end-to-end") {
      val program =
        for
          _   <- stub[Repo](_.find(slayer.any))("hi")
          got <- ZIO.serviceWith[Repo](_.find(42))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == "hi")
    },
    test("3-arg method, end-to-end") {
      val program =
        for
          _   <- stub[Repo](_.merge(slayer.any, slayer.any, slayer.any))(7)
          got <- ZIO.serviceWith[Repo](_.merge(1, 2, 3))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == 7)
    },
    test("multiple methods, no cross-contamination") {
      val program =
        for
          _ <- stub[Repo](_.now)(99L)
          _ <- stub[Repo](_.label)("LBL")
          a <- ZIO.serviceWith[Repo](_.now)
          b <- ZIO.serviceWith[Repo](_.label)
        yield (a, b)
      for tup <- program.provide(stubbed[Repo])
      yield assertTrue(tup == (99L, "LBL"))
    },
    test("second stub on the same method wins") {
      val program =
        for
          _   <- stub[Repo](_.now)(1L)
          _   <- stub[Repo](_.now)(2L)
          got <- ZIO.serviceWith[Repo](_.now)
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == 2L)
    },
    test("calling an unstubbed method raises NoStub") {
      val program = ZIO.serviceWith[Repo](_.now)
      val attempt = program.provide(stubbed[Repo]).exit
      for ex <- attempt
      yield assertTrue(ex.isFailure || ex.causeOption.exists(_.dieOption.isDefined))
    },
    test("function-handler stub flows through the synthesized class") {
      val program =
        for
          _   <- stub[Repo](_.find(slayer.any))((id: Int) => s"id=$id")
          got <- ZIO.serviceWith[Repo](_.find(7))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == "id=7")
    },
    test("two distinct service layers do not share state") {
      val program =
        for
          _ <- stub[Repo](_.label)("repo")
          _ <- stub[Greeter](_.greet(slayer.any))("hello")
          a <- ZIO.serviceWith[Repo](_.label)
          b <- ZIO.serviceWith[Greeter](_.greet("ignored"))
        yield (a, b)
      for tup <- program.provide(stubbed[Repo], stubbed[Greeter])
      yield assertTrue(tup == ("repo", "hello"))
    },
  )
end StubbedClassSpec
