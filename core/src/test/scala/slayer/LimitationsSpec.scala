package slayer

import zio.test.*

/** Pin the README's "Limitations" claims so they don't drift.
  *
  * Each test asserts the actual behavior we document — language-level rejection or slayer-level rejection — using
  * `typeCheck` to inspect the compiler's error message.
  */
object LimitationsSpec extends ZIOSpecDefault:

  def spec = suite("LimitationsSpec — pin documented limitations")(
    test("erasure-collision overloads are rejected by Scala at the trait-declaration site") {
      for res <- typeCheck("""
        trait BadOverloads:
          def f(xs: List[Int]): Int
          def f(xs: List[String]): Int
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          // Scala's message mentions same erasure / clash.
          msg.contains("erasure") || msg.contains("double definition") || msg.contains("clash"),
        )
    },
    test("class with concrete methods is accepted as a service type (concrete methods fall through)") {
      for res <- typeCheck("""
        import slayer.*
        class ClassService { def m(i: Int): Int = i + 1 }
        val layer = stubbed[ClassService]
      """)
      yield assertTrue(res.isRight)
    },
    test("class with a final method is rejected — slayer can't override it") {
      for res <- typeCheck("""
        import slayer.*
        class WithFinal:
          final def m(i: Int): Int = i
        val layer = stubbed[WithFinal]
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          msg.contains("overriding"),
        )
    },
    test("final class is rejected — slayer can't subclass it") {
      for res <- typeCheck("""
        import slayer.*
        final class Sealed { def m(i: Int): Int = i }
        val layer = stubbed[Sealed]
      """)
      yield assertTrue(res.isLeft)
    },
    test("abstract val is rejected — use def instead") {
      for res <- typeCheck("""
        import slayer.*
        trait WithVal { val x: Int }
        val layer = stubbed[WithVal]
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(res.isLeft, msg.contains("abstract val"), msg.contains("def"))
    },
  )
end LimitationsSpec
