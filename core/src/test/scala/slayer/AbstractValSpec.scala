package slayer

import zio.*
import zio.test.*

/** Abstract `val` members cannot be stubbed: a stable val body would run at layer construction (before `stub`), and
  * Scala rejects both `lazy val` and `def` as completions of a non-lazy abstract val. Prefer `def` on service traits.
  * Concrete vals still inherit without override.
  */
object AbstractValSpec extends ZIOSpecDefault:

  trait Config:
    val host: String
    val port: Int

  /** Concrete val — inherits; not stubbed. */
  trait WithConcreteVal:
    val fixed: String = "from-trait"
    def label: String

  def spec = suite("AbstractValSpec — val members")(
    test("abstract val is rejected with a clear stubbed error") {
      for res <- typeCheck("""
        import slayer.*
        val layer = stubbed[AbstractValSpec.Config]
      """)
      yield
        val msg = res.swap.getOrElse("")
        assertTrue(
          res.isLeft,
          msg.contains("abstract val"),
          msg.contains("host") || msg.contains("port"),
          msg.contains("def"),
        )
    },
    test("concrete val inherits from trait (not stubbed)") {
      val program =
        for
          _   <- stub[WithConcreteVal](_.label)("L")
          f   <- ZIO.serviceWith[WithConcreteVal](_.fixed)
          lbl <- ZIO.serviceWith[WithConcreteVal](_.label)
        yield (f, lbl)
      for tup <- program.provide(stubbed[WithConcreteVal])
      yield assertTrue(tup == ("from-trait", "L"))
    },
  )
end AbstractValSpec
