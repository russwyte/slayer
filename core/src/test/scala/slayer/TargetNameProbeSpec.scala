package slayer

import scala.annotation.targetName
import zio.test.*

/** First-principles probe (resolved): when a trait declares an abstract method renamed via `@targetName`, what does
  * Scala 3 require of an overriding class?
  *
  *   - Hypothesis A: the overrider must repeat the `@targetName` annotation on the source name. **CONFIRMED.**
  *   - Hypothesis B: the overrider can use the JVM name directly without the annotation. **REJECTED** —
  *     "class ByJvmName needs to be abstract, since def !(i: Int): Int in trait Renamed is not defined".
  *
  * Implication for slayer: the synthesized class must (a) declare the override under the *source* name and (b) carry
  * the `@targetName` annotation. We achieve (b) via `experimental.addTargetNameAnnotation`, which reaches the dotty
  * `SymDenotation.addAnnotation` API through a `QuotesImpl` cast. End-to-end coverage of this lives in
  * [[OverloadSpec]] ("@targetName rename — synthesized override carries propagated annotation").
  */
object TargetNameProbeSpec extends ZIOSpecDefault:

  trait Renamed:
    @targetName("bang") def !(i: Int): Int

  // Hypothesis A: name the override `!` and repeat the @targetName annotation. Works.
  final class ByAnnotated extends Renamed:
    @targetName("bang") def !(i: Int): Int = i + 2

  def spec = suite("TargetNameProbeSpec — Scala override rules for @targetName")(
    test("hypothesis A: source name + repeated annotation overrides successfully") {
      val a: Renamed = ByAnnotated()
      assertTrue(a.!(1) == 3)
    }
  )
end TargetNameProbeSpec
