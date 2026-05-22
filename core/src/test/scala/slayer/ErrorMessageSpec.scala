package slayer

import zio.*
import zio.test.*

/** M7 unit tests: pin compile-time error messages produced by `stub` and `stubbed`.
  *
  * Each test uses zio-test's `typeCheck` to compile a snippet and asserts the error message contains the substrings we
  * promise users. We match on substrings (not exact strings) so cosmetic tweaks don't break the contract — but each
  * substring is something a user would search for when diagnosing the failure.
  */
object ErrorMessageSpec extends ZIOSpecDefault:

  trait Svc:
    def find(id: Int): String
    def label: String
    def merge(a: Int, b: Int): String

  def spec = suite("ErrorMessageSpec — M7 compile-error contracts")(
    test("result type doesn't match method return type — names both types") {
      for res <- typeCheck("""
        import slayer.*
        stub[ErrorMessageSpec.Svc](_.find(any[Int]))(42)
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          msg.contains("find"),
          msg.contains("String"),
          msg.contains("Int"),
        )
    },
    test("selector isn't a method — message identifies the problem") {
      for res <- typeCheck("""
        import slayer.*
        stub[ErrorMessageSpec.Svc](_ => 42)("x")
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          msg.contains("selector"),
          msg.contains("method"),
        )
    },
    test("handler arity doesn't match method arity — names both arities") {
      for res <- typeCheck("""
        import slayer.*
        stub[ErrorMessageSpec.Svc](_.merge(any[Int], any[Int]))((a: Int) => "x")
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          msg.contains("arity"),
          msg.contains("merge"),
        )
    },
    test("handler param type incompatible — message names mismatched types") {
      for res <- typeCheck("""
        import slayer.*
        stub[ErrorMessageSpec.Svc](_.find(any[Int]))((s: String) => "x")
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          msg.contains("parameter"),
          msg.contains("Int"),
          msg.contains("String"),
        )
    },
    test("handler return type incompatible — message names both") {
      for res <- typeCheck("""
        import slayer.*
        stub[ErrorMessageSpec.Svc](_.find(any[Int]))((i: Int) => 42)
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          msg.contains("return"),
          msg.contains("String"),
          msg.contains("Int"),
        )
    },
  )
end ErrorMessageSpec
