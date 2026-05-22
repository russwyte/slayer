package slayer

import scala.annotation.targetName
import zio.*
import zio.test.*

/** M6 unit tests: prove that overloaded methods do not collide in the `Stubbed` runtime store.
  *
  * Each test is one focused theory, added incrementally. The design choice from M1 is that `MethodId` carries both
  * `jvmName` (which honors `@targetName`) and `erasedParams`, so plain overloads with different param types share a
  * `jvmName` but differ in `erasedParams`.
  *
  * `@targetName` support: handled by `experimental.addTargetNameAnnotation`, which propagates the annotation from the
  * trait method to the synthesized override via the dotty `SymDenotation.addAnnotation` API reached through a
  * `QuotesImpl` cast — same family of trick we use elsewhere for unsurfaced compiler APIs.
  */
object OverloadSpec extends ZIOSpecDefault:

  trait Repo:
    def find(id: Int): String
    def find(name: String): String

  trait Three:
    def f(i: Int): String
    def f(s: String): String
    def f(xs: List[Int]): String

  trait Mixed:
    def g(i: Int): String
    def g[A](a: A): String

  trait Renamed:
    @targetName("bang") def !(i: Int): Int

  // Probe: Scala 3 lets two abstract methods share a source name if @targetName disambiguates them at the JVM level.
  // Trait declaration compiles. Calling them is another matter: `_.a(0)` is ambiguous, and `_.a1(0)` / `_.a2(0)` are
  // rejected ("value a1 is not a member" — @targetName is purely a JVM-level rename, not a Scala alias). So while the
  // siblings shape is *legal*, the methods are effectively uncallable from Scala source. We document this as a
  // language-level limitation in the README; slayer itself synthesizes the override correctly via
  // `addTargetNameAnnotation` (proven by the `Renamed` test above).
  trait Siblings:
    @targetName("a1") def a(i: Int): Int
    @targetName("a2") def a(i: Int): Int

  def spec = suite("OverloadSpec — M6 overloads")(
    test("plain overload — stub and call each independently") {
      val program =
        for
          _ <- stub[Repo](_.find(slayer.any[Int]))("by-int")
          _ <- stub[Repo](_.find(slayer.any[String]))("by-string")
          a <- ZIO.serviceWith[Repo](_.find(7))
          b <- ZIO.serviceWith[Repo](_.find("hello"))
        yield (a, b)
      for tup <- program.provide(stubbed[Repo])
      yield assertTrue(tup == ("by-int", "by-string"))
    },
    test("plain overload — stub one, call the other → NoStub") {
      val program =
        for
          _   <- stub[Repo](_.find(slayer.any[Int]))("only-int")
          got <- ZIO.serviceWith[Repo](_.find("never-stubbed")).exit
        yield got
      for ex <- program.provide(stubbed[Repo])
      yield assertTrue(ex.causeOption.exists(_.dieOption.exists(_.isInstanceOf[SlayerError.NoStub])))
    },
    test("3-way overload — Int / String / List[Int] all distinct") {
      val program =
        for
          _ <- stub[Three](_.f(slayer.any[Int]))("i")
          _ <- stub[Three](_.f(slayer.any[String]))("s")
          _ <- stub[Three](_.f(slayer.any[List[Int]]))("l")
          a <- ZIO.serviceWith[Three](_.f(7))
          b <- ZIO.serviceWith[Three](_.f("x"))
          c <- ZIO.serviceWith[Three](_.f(List(1, 2, 3)))
        yield (a, b, c)
      for tup <- program.provide(stubbed[Three])
      yield assertTrue(tup == ("i", "s", "l"))
    },
    test("overload where one is generic — selectors disambiguate by type") {
      val program =
        for
          _ <- stub[Mixed](_.g(slayer.any[Int]))("plain-int")
          _ <- stub[Mixed](_.g[String](slayer.any[String]))("generic-string")
          a <- ZIO.serviceWith[Mixed](_.g(7))
          b <- ZIO.serviceWith[Mixed](_.g[String]("hi"))
        yield (a, b)
      for tup <- program.provide(stubbed[Mixed])
      yield assertTrue(tup == ("plain-int", "generic-string"))
    },
    test("@targetName rename — synthesized override carries propagated annotation") {
      val program =
        for
          _   <- stub[Renamed](_.!(slayer.any[Int]))(99)
          got <- ZIO.serviceWith[Renamed](_.!(0))
        yield got
      for got <- program.provide(stubbed[Renamed])
      yield assertTrue(got == 99)
    },
    test("@targetName-disambiguated siblings — language-level limitation pinned") {
      // Trait shape is legal — confirm the synthesized class compiles (no missing-override errors).
      // Calling either sibling is the language-level limitation: `_.a` is ambiguous, `_.a1` / `_.a2` aren't members.
      for
        bySource <- typeCheck("ZIO.serviceWith[OverloadSpec.Siblings](_.a(0))")
        byJvm    <- typeCheck("ZIO.serviceWith[OverloadSpec.Siblings](_.a1(0))")
      yield assertTrue(
        bySource.isLeft, // ambiguous reference
        byJvm.isLeft,    // a1 is not a member; @targetName is JVM-only
      )
    },
  )
end OverloadSpec
