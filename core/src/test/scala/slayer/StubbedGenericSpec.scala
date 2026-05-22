package slayer

import zio.*
import zio.test.*

/** M5 unit tests: drive the synthesized service class for the harder method shapes — curried parameter lists,
  * `using`/implicit clauses, and generic methods. The class shape is generated once for the whole trait, so the macro
  * has to handle all of these in a single pass.
  */
object StubbedGenericSpec extends ZIOSpecDefault:

  trait Repo:
    def composed(a: Int)(b: String)(c: Boolean): String
    def lookup(k: Int)(using s: String): Int
    def get[A](id: Int): UIO[A]

  def spec = suite("StubbedGenericSpec — M5 multi-list, implicits, generics")(
    test("curried 3-list method, end-to-end") {
      val program =
        for
          _   <- stub[Repo](_.composed(slayer.any)(slayer.any)(slayer.any))("ok")
          got <- ZIO.serviceWith[Repo](_.composed(1)("x")(true))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == "ok")
    },
    test("method with using clause — implicit dropped from key and array") {
      given String = "ctx"
      val program =
        for
          _   <- stub[Repo](_.lookup(slayer.any))(99)
          got <- ZIO.serviceWith[Repo](_.lookup(7))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == 99)
    },
    test("generic method, value stub at A := String") {
      val program =
        for
          _   <- stub[Repo](_.get[String](slayer.any))(ZIO.succeed("hi"))
          got <- ZIO.serviceWithZIO[Repo](_.get[String](7))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == "hi")
    },
    test("generic method, handler stub uses id") {
      val program =
        for
          _   <- stub[Repo](_.get[String](slayer.any))((id: Int) => ZIO.succeed(s"id=$id"))
          got <- ZIO.serviceWithZIO[Repo](_.get[String](42))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == "id=42")
    },
  )
end StubbedGenericSpec
