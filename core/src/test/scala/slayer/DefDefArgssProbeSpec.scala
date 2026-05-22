package slayer

import zio.*
import zio.test.*

/** First-principles probe: when we synthesize a class member for a method on a service trait via `Symbol.newMethod` +
  * `DefDef`, what does the `argss` callback parameter actually contain? This is the question we have to answer to
  * implement M5 (multi-list / using / generic methods) in `stubbedImpl` correctly — instead of guessing, capture the
  * shape and let the test be the source of truth.
  *
  * We probe a simple non-generic method, a curried method, a `using` method, and a generic method. Each test asserts
  * the *exact* shape we observe; if a future Scala upgrade changes the shape, these tests fail loudly and tell us what
  * to fix.
  */
object DefDefArgssProbeSpec extends ZIOSpecDefault:

  trait Repo:
    def simple(i: Int): String
    def curried(a: Int)(b: String): Boolean
    def withUsing(k: Int)(using s: String): Int
    def generic[A](id: Int): UIO[A]

  def spec = suite("DefDefArgssProbeSpec — DefDef argss shape")(
    test("simple 1-arg method") {
      val p = _root_.slayer.Macros.probeDefDefArgss[Repo]("simple")
      assertTrue(
        p.methodName == "simple",
        p.listCount == 1,
        p.perListSizes == List(1),
      )
    },
    test("curried 2-list method") {
      val p = _root_.slayer.Macros.probeDefDefArgss[Repo]("curried")
      assertTrue(
        p.listCount == 2,
        p.perListSizes == List(1, 1),
      )
    },
    test("method with `using` clause") {
      val p = _root_.slayer.Macros.probeDefDefArgss[Repo]("withUsing")
      println(s"USING PROBE: $p")
      assertTrue(
        p.listCount == 2,
        p.perListSizes == List(1, 1),
      )
    },
    test("generic method (one type param + one value list)") {
      val p = _root_.slayer.Macros.probeDefDefArgss[Repo]("generic")
      println(s"""GENERIC PROBE
                 |  termRefRaw = ${p.methodTermRefRaw}
                 |  typeTreeTpes = ${p.typeTreeTpes}
                 |  valueIdentTpes = ${p.valueIdentTpes}
                 |  retShow = ${p.methodReturnTypeShow}
                 |""".stripMargin)
      assertTrue(
        p.methodName == "generic",
        p.listCount == 2,
        p.perListSizes == List(1, 1),
        p.perListElementClasses(0) == List("TypeTree"),
        p.perListElementClasses(1) == List("Ident"),
      )
    },
  )
end DefDefArgssProbeSpec
