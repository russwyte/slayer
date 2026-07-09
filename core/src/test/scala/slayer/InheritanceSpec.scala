package slayer

import zio.*
import zio.test.*

/** Parent-trait / multi-level service hierarchies: `stubbed` must synthesize overrides for *inherited* methods, not
  * only members declared on the leaf type (`declaredMethods` alone is insufficient).
  */
object InheritanceSpec extends ZIOSpecDefault:

  trait Base:
    def find(id: Int): String
    def label: String

  trait Mid extends Base:
    def mid(x: Int): Int

  trait Repo extends Mid:
    def save(s: String): UIO[Unit]

  /** Parent provides a concrete default; leaf only adds abstract members. */
  trait GreeterBase:
    def name: String
    def hello: String = s"hello, $name"

  trait Greeter extends GreeterBase:
    def shout: String

  def spec = suite("InheritanceSpec — inherited service methods")(
    test("inherited abstract method from parent trait is stubbable end-to-end") {
      val program =
        for
          _   <- stub[Repo](_.find(slayer.any[Int]))("found")
          _   <- stub[Repo](_.label)("L")
          _   <- stub[Repo](_.mid(slayer.any[Int]))(9)
          _   <- stub[Repo](_.save(slayer.any[String]))(ZIO.unit)
          a   <- ZIO.serviceWith[Repo](_.find(1))
          b   <- ZIO.serviceWith[Repo](_.label)
          c   <- ZIO.serviceWith[Repo](_.mid(3))
          _   <- ZIO.serviceWithZIO[Repo](_.save("x"))
        yield (a, b, c)
      for tup <- program.provide(stubbed[Repo])
      yield assertTrue(tup == ("found", "L", 9))
    },
    test("leaf-only declaration still works when parents are empty of abstracts we need") {
      val program =
        for
          _   <- stub[Repo](_.find(slayer.any))("x")
          _   <- stub[Repo](_.label)("y")
          _   <- stub[Repo](_.mid(slayer.any))(0)
          _   <- stub[Repo](_.save(slayer.any))(ZIO.unit)
          got <- ZIO.serviceWith[Repo](_.find(0))
        yield got
      for got <- program.provide(stubbed[Repo])
      yield assertTrue(got == "x")
    },
    test("inherited concrete method falls through to parent impl") {
      val program =
        for
          _   <- stub[Greeter](_.name)("alice")
          _   <- stub[Greeter](_.shout)("HEY")
          got <- ZIO.serviceWith[Greeter](_.hello)
        yield got
      for got <- program.provide(stubbed[Greeter])
      yield assertTrue(got == "hello, alice")
    },
    test("inherited concrete method can still be overridden by stub") {
      val program =
        for
          _   <- stub[Greeter](_.name)("alice")
          _   <- stub[Greeter](_.shout)("HEY")
          _   <- stub[Greeter](_.hello)("howdy")
          got <- ZIO.serviceWith[Greeter](_.hello)
        yield got
      for got <- program.provide(stubbed[Greeter])
      yield assertTrue(got == "howdy")
    },
    test("unstubbed inherited abstract method raises NoStub") {
      val program =
        for
          _   <- stub[Repo](_.label)("only-label")
          // find/mid/save left unstubbed
          got <- ZIO.serviceWith[Repo](_.find(1)).exit
        yield got
      for ex <- program.provide(stubbed[Repo])
      yield assertTrue(ex.causeOption.exists(_.dieOption.exists(_.isInstanceOf[SlayerError.NoStub])))
    },
  )
end InheritanceSpec
