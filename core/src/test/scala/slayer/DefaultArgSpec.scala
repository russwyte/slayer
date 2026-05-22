package slayer

import zio.*
import zio.test.*

/** Default-argument support — both stub site and call site agree on the same `MethodId` because the Scala typer fills
  * defaults *at both sites* before the macro runs. The only fix slayer needs is to filter compiler-synthesized
  * `<name>$default$<n>` getters out of the methods we synthesize overrides for; otherwise the JVM rejects the class
  * with "Duplicate method name". See `Macros.stubbedImpl` — `methods.filter(!Flags.Synthetic && !"$default$")`.
  */
object DefaultArgSpec extends ZIOSpecDefault:

  trait WithDefault:
    def f(a: Int, b: Int = 7): Int

  /** Multiple defaults at different positions — exercises subsets like (1,2,_), (1,_,3), and (_,_,_) via named args. */
  trait Multi:
    def g(a: Int, b: Int = 20, c: Int = 30, d: Int = 40): String

  /** Default in the middle, no default at the end — Scala 3 lets you skip a leading default if you name the trailing
    * required arg, e.g. `h(c = 5)` with `h(a: Int = 1, b: Int = 2, c: Int)`.
    */
  trait LeadingDefaults:
    def h(a: Int = 1, b: Int = 2, c: Int): String

  /** Curried with defaults across multiple parameter lists — uncommon but legal. */
  trait CurriedDefaults:
    def k(a: Int, b: Int = 99)(c: Int = 100, d: Int): String

  /** Q1 verification: if defaults are filled in by the typer, then a stub written with `_.f(any[Int])` (one arg) has
    * the same `erasedParams` as the actual call `service.f(1)` (which is filled to 2 args). If they match, slayer's
    * `MethodId` lookup will work. If they don't match, we'd need to compute `MethodId` differently for partial calls.
    */
  def spec = suite("DefaultArgSpec — default-argument support")(
    test("stub with default omitted, call with default omitted — typer fills both, MethodIds match") {
      val program =
        for
          _   <- stub[WithDefault](_.f(slayer.any[Int]))(99)
          got <- ZIO.serviceWith[WithDefault](_.f(1))
        yield got
      for got <- program.provide(stubbed[WithDefault])
      yield assertTrue(got == 99)
    },
    test("stub with both args explicit, call with both args explicit — sanity baseline") {
      val program =
        for
          _   <- stub[WithDefault](_.f(slayer.any[Int], slayer.any[Int]))(42)
          got <- ZIO.serviceWith[WithDefault](_.f(1, 2))
        yield got
      for got <- program.provide(stubbed[WithDefault])
      yield assertTrue(got == 42)
    },
    test("stub with default omitted, call with default filled explicitly — still dispatches") {
      val program =
        for
          _   <- stub[WithDefault](_.f(slayer.any[Int]))(11)
          got <- ZIO.serviceWith[WithDefault](_.f(1, 7))
        yield got
      for got <- program.provide(stubbed[WithDefault])
      yield assertTrue(got == 11)
    },
    test("multi-default: stub omits all defaults, call omits all defaults") {
      val program =
        for
          _   <- stub[Multi](_.g(slayer.any[Int]))("a-only")
          got <- ZIO.serviceWith[Multi](_.g(1))
        yield got
      for got <- program.provide(stubbed[Multi])
      yield assertTrue(got == "a-only")
    },
    test("multi-default: stub provides middle default, call provides middle default") {
      val program =
        for
          _   <- stub[Multi](_.g(slayer.any[Int], slayer.any[Int]))("a-b")
          got <- ZIO.serviceWith[Multi](_.g(1, 2))
        yield got
      for got <- program.provide(stubbed[Multi])
      yield assertTrue(got == "a-b")
    },
    test("multi-default: stub provides all, call provides all") {
      val program =
        for
          _   <- stub[Multi](_.g(slayer.any[Int], slayer.any[Int], slayer.any[Int], slayer.any[Int]))("all")
          got <- ZIO.serviceWith[Multi](_.g(1, 2, 3, 4))
        yield got
      for got <- program.provide(stubbed[Multi])
      yield assertTrue(got == "all")
    },
    test("multi-default: handler reads filled args — confirms default values flow into the array") {
      val program =
        for
          _ <- stub[Multi](_.g(slayer.any[Int])) { (a: Int, b: Int, c: Int, d: Int) =>
            s"$a/$b/$c/$d"
          }
          got <- ZIO.serviceWith[Multi](_.g(1))
        yield got
      for got <- program.provide(stubbed[Multi])
      yield assertTrue(got == "1/20/30/40") // defaults from the trait are filled at the call site
    },
    test("multi-default: handler reads partial fill — defaults fill only the omitted slots") {
      val program =
        for
          _ <- stub[Multi](_.g(slayer.any[Int], slayer.any[Int])) { (a: Int, b: Int, c: Int, d: Int) =>
            s"$a/$b/$c/$d"
          }
          got <- ZIO.serviceWith[Multi](_.g(1, 99))
        yield got
      for got <- program.provide(stubbed[Multi])
      yield assertTrue(got == "1/99/30/40")
    },
    test("multi-default: stub-side and call-side fill different subsets — they still match") {
      val program =
        for
          // Stub omits b,c,d (typer fills 20, 30, 40).
          _ <- stub[Multi](_.g(slayer.any[Int])) { (a: Int, b: Int, c: Int, d: Int) =>
            s"$a/$b/$c/$d"
          }
          // Call omits c,d (typer fills 30, 40), passes b explicitly.
          got <- ZIO.serviceWith[Multi](_.g(5, 6))
        yield got
      for got <- program.provide(stubbed[Multi])
      yield assertTrue(got == "5/6/30/40")
    },
    test("named arg skipping a leading default — `h(c = 5)` with two leading defaults") {
      val program =
        for
          _ <- stub[LeadingDefaults](_.h(c = slayer.any[Int])) { (a: Int, b: Int, c: Int) =>
            s"$a/$b/$c"
          }
          got <- ZIO.serviceWith[LeadingDefaults](_.h(c = 5))
        yield got
      for got <- program.provide(stubbed[LeadingDefaults])
      yield assertTrue(got == "1/2/5") // both leading defaults filled
    },
    test("curried with defaults across lists — full call works") {
      val program =
        for
          _ <- stub[CurriedDefaults](_.k(slayer.any[Int], slayer.any[Int])(slayer.any[Int], slayer.any[Int])) {
            (a: Int, b: Int, c: Int, d: Int) => s"$a/$b/$c/$d"
          }
          got <- ZIO.serviceWith[CurriedDefaults](_.k(1, 2)(3, 4))
        yield got
      for got <- program.provide(stubbed[CurriedDefaults])
      yield assertTrue(got == "1/2/3/4")
    },
    test("curried with defaults — call omits the default in each list, stub also omits both") {
      val program =
        for
          _ <- stub[CurriedDefaults](_.k(slayer.any[Int])(d = slayer.any[Int])) { (a: Int, b: Int, c: Int, d: Int) =>
            s"$a/$b/$c/$d"
          }
          got <- ZIO.serviceWith[CurriedDefaults](_.k(1)(d = 4))
        yield got
      for got <- program.provide(stubbed[CurriedDefaults])
      yield assertTrue(got == "1/99/100/4")
    },
  )
end DefaultArgSpec
