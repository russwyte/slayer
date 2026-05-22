package slayer

import zio.*
import zio.test.*

/** Concrete trait methods: slayer overrides every trait method on the synthesized class, but concrete methods fall
  * through to the trait's original impl when no stub is registered. Users can:
  *   - leave concrete methods alone and let the trait's impl run (with abstract dependencies stubbed),
  *   - or stub the concrete method directly to override its behavior in a test.
  *
  * Implementation: concrete-method overrides call `Stubbed.callStubbedOrElse(id, args, () => super.method(args))`.
  */
object ConcreteMethodSpec extends ZIOSpecDefault:

  trait Greeter:
    def name: String // abstract — must be stubbed
    def hello: String = s"hello, $name" // concrete — calls `name`

  trait WithMath:
    def base: Int // abstract
    def doubled: Int    = base * 2    // concrete, uses abstract `base`
    def quadrupled: Int = doubled * 2 // concrete, uses concrete `doubled`

  trait Echoer:
    def prefix: String // abstract
    def echo(s: String): String = s"$prefix$s" // concrete with args

  def spec = suite("ConcreteMethodSpec — concrete trait methods")(
    test("fall-through: concrete `hello` uses the trait's impl, which reads the stubbed abstract `name`") {
      val program =
        for
          _   <- stub[Greeter](_.name)("alice")
          got <- ZIO.serviceWith[Greeter](_.hello)
        yield got
      for got <- program.provide(stubbed[Greeter])
      yield assertTrue(got == "hello, alice")
    },
    test("override: stubbing a concrete method wins over its trait impl") {
      val program =
        for
          _   <- stub[Greeter](_.name)("alice")
          _   <- stub[Greeter](_.hello)("howdy from stub")
          got <- ZIO.serviceWith[Greeter](_.hello)
        yield got
      for got <- program.provide(stubbed[Greeter])
      yield assertTrue(got == "howdy from stub")
    },
    test("chained concretes cascade through stubbed abstract") {
      val program =
        for
          _ <- stub[WithMath](_.base)(7)
          a <- ZIO.serviceWith[WithMath](_.base)
          b <- ZIO.serviceWith[WithMath](_.doubled)
          c <- ZIO.serviceWith[WithMath](_.quadrupled)
        yield (a, b, c)
      for tup <- program.provide(stubbed[WithMath])
      yield assertTrue(tup == (7, 14, 28))
    },
    test("stub middle concrete: `quadrupled` falls through and calls our stubbed `doubled`") {
      val program =
        for
          _ <- stub[WithMath](_.base)(1)
          _ <- stub[WithMath](_.doubled)(100)
          q <- ZIO.serviceWith[WithMath](_.quadrupled)
        yield q
      for q <- program.provide(stubbed[WithMath])
      yield assertTrue(q == 200)
    },
    test("concrete method with args: fall-through and stub-override both work") {
      val program =
        for
          _     <- stub[Echoer](_.prefix)(">> ")
          fallA <- ZIO.serviceWith[Echoer](_.echo("one"))
          _     <- stub[Echoer](_.echo(slayer.any[String])) { (s: String) => s"!$s!" }
          stubB <- ZIO.serviceWith[Echoer](_.echo("two"))
        yield (fallA, stubB)
      for tup <- program.provide(stubbed[Echoer])
      yield assertTrue(tup == (">> one", "!two!"))
    },
  )
end ConcreteMethodSpec
