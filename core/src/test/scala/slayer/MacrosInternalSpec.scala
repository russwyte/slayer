package slayer

import scala.annotation.targetName
import zio.test.*

/** M1 unit tests: drive `peelMethod`, `flatten`, `methodIdOf` in isolation through the probe macros, before any of the
  * higher-level `stub`/`stubbed` macros exist.
  */
object MacrosInternalSpec extends ZIOSpecDefault:

  import slayer.Macros.*

  // ----- Service traits used purely as targets for selectors. ---------------------------------------------------------

  trait Plain:
    def noArg: Int
    def oneArg(i: Int): Int
    def threeArgs(a: Int, b: String, c: Boolean): Int
    def twoLists(a: Int)(b: String): Int
    def threeLists(a: Int)(b: String)(c: Boolean): Int
  end Plain

  trait Generic:
    def gen0[A]: A
    def gen1[A](a: A): A
    def gen2[A, B](a: A, b: B): (A, B)
    def withTag[A](id: Int)(using ev: scala.reflect.ClassTag[A]): A

  trait Overloaded:
    def f(i: Int): Int
    def f(s: String): String
    def f(xs: List[Int]): Int

    @targetName("a1") def a(i: Int): Int
    @targetName("a2") def a(i: Int): Int

    @targetName("bang") def renamed(i: Int): Int
  end Overloaded

  trait WithImplicit:
    def lookup(i: Int)(using s: String): Int
    def both[A](a: A)(using s: String): A

  // `using` parameters in selector lambdas need a `given` of the right type in scope so the call typechecks.
  // The actual value is irrelevant — `slayer.any` is never evaluated; the macro reads only the lambda's tree.
  given String = ""

  def spec = suite("MacrosInternalSpec — M1 reflection helpers")(
    suite("peelMethod")(
      test("no-args selector") {
        val p = probePeel[Plain](_.noArg)
        assertTrue(p.foundMethod, p.methodName.contains("noArg"), p.typeArgs.isEmpty)
      },
      test("single-arg selector") {
        val p = probePeel[Plain](_.oneArg(slayer.any))
        assertTrue(p.foundMethod, p.methodName.contains("oneArg"))
      },
      test("multi-arg single-list selector") {
        val p = probePeel[Plain](_.threeArgs(slayer.any, slayer.any, slayer.any))
        assertTrue(p.foundMethod, p.methodName.contains("threeArgs"))
      },
      test("two parameter lists") {
        val p = probePeel[Plain](_.twoLists(slayer.any)(slayer.any))
        assertTrue(p.foundMethod, p.methodName.contains("twoLists"))
      },
      test("three parameter lists") {
        val p = probePeel[Plain](_.threeLists(slayer.any)(slayer.any)(slayer.any))
        assertTrue(p.foundMethod, p.methodName.contains("threeLists"))
      },
      test("type-app, no value args") {
        val p = probePeel[Generic](_.gen0[String])
        assertTrue(
          p.foundMethod,
          p.methodName.contains("gen0"),
          p.typeArgs.exists(_.contains("String")),
        )
      },
      test("type-app + value args") {
        val p = probePeel[Generic](_.gen2[String, Int](slayer.any, slayer.any))
        assertTrue(
          p.foundMethod,
          p.methodName.contains("gen2"),
          p.typeArgs.size == 2,
          p.typeArgs.exists(_.contains("String")),
          p.typeArgs.exists(_.contains("Int")),
        )
      },
      test("plain overload — typer picks the right symbol") {
        // Both selectors share source name 'f' but `peelMethod` returns the resolved overload symbol.
        val pInt = probePeel[Overloaded](_.f(slayer.any[Int]))
        val pStr = probePeel[Overloaded](_.f(slayer.any[String]))
        assertTrue(pInt.foundMethod, pInt.methodName.contains("f"))
        && assertTrue(pStr.foundMethod, pStr.methodName.contains("f"))
      },
      test("@targetName rename — methodName uses targetName, not source name") {
        // Source name is `renamed`, JVM/targetName is `bang`. The selector must use the source name.
        val p = probePeel[Overloaded](_.renamed(slayer.any))
        assertTrue(p.foundMethod, p.methodName.contains("bang"))
      },
    ),
    suite("flatten")(
      test("no params") {
        val f = probeFlatten[Plain](_.noArg)
        assertTrue(f.typeParams.isEmpty, f.valueParams.isEmpty, f.returnType.contains("Int"))
      },
      test("single list, multiple args") {
        val f = probeFlatten[Plain](_.threeArgs(slayer.any, slayer.any, slayer.any))
        assertTrue(
          f.typeParams.isEmpty,
          f.valueParams.size == 3,
          f.valueParams.forall(!_._2),
        )
      },
      test("curried 2 lists") {
        val f = probeFlatten[Plain](_.twoLists(slayer.any)(slayer.any))
        assertTrue(f.valueParams.size == 2, f.valueParams.forall(!_._2))
      },
      test("curried 3 lists") {
        val f = probeFlatten[Plain](_.threeLists(slayer.any)(slayer.any)(slayer.any))
        assertTrue(f.valueParams.size == 3)
      },
      test("generic, no value args") {
        val f = probeFlatten[Generic](_.gen0[String])
        assertTrue(f.typeParams.size == 1, f.valueParams.isEmpty)
      },
      test("generic + value list") {
        val f = probeFlatten[Generic](_.gen1[String](slayer.any))
        assertTrue(f.typeParams.size == 1, f.valueParams.size == 1)
      },
      test("generic with using clause — flag is set on the using list") {
        val f = probeFlatten[Generic](_.withTag[String](slayer.any))
        // 1 explicit value param, 1 implicit ClassTag
        assertTrue(f.typeParams.size == 1)
        && assertTrue(f.valueParams.size == 2)
        && assertTrue(f.valueParams.count(_._2) == 1)
        && assertTrue(f.valueParams.count(!_._2) == 1)
      },
      test("Scala-2-style implicit also flagged") {
        val f = probeFlatten[WithImplicit](_.lookup(slayer.any))
        assertTrue(f.valueParams.size == 2, f.valueParams.count(_._2) == 1)
      },
    ),
    suite("methodIdOf")(
      test("no-arg method has empty erasedParams") {
        val id = probeMethodId[Plain](_.noArg)
        assertTrue(id.jvmName == "noArg", id.erasedParams.isEmpty)
      },
      test("plain overloads — same jvmName, different erasedParams") {
        val idInt = probeMethodId[Overloaded](_.f(slayer.any[Int]))
        val idStr = probeMethodId[Overloaded](_.f(slayer.any[String]))
        val idLst = probeMethodId[Overloaded](_.f(slayer.any[List[Int]]))
        assertTrue(idInt.jvmName == "f", idStr.jvmName == "f", idLst.jvmName == "f")
        && assertTrue(idInt != idStr, idInt != idLst, idStr != idLst)
        && assertTrue(idInt.erasedParams != idStr.erasedParams)
      },
      test("@targetName siblings — different jvmName, same erasedParams") {
        // Source name `a` is ambiguous between the two siblings, so we can't reference them via a selector lambda.
        // Walk all declared methods of the trait instead and find the two with jvmName a1 / a2.
        val all  = probeMethodIdsByName[Overloaded]
        val ida1 = all.collectFirst { case (_, id) if id.jvmName == "a1" => id }
        val ida2 = all.collectFirst { case (_, id) if id.jvmName == "a2" => id }
        assertTrue(ida1.isDefined, ida2.isDefined)
        && assertTrue(ida1 != ida2)
        && assertTrue(ida1.get.erasedParams == ida2.get.erasedParams)
      },
      test("@targetName rename — jvmName is the targetName, not the source") {
        // Source name `renamed`, targetName `bang`. Selector references the source name.
        val id = probeMethodId[Overloaded](_.renamed(slayer.any))
        assertTrue(id.jvmName == "bang")
      },
      test("idempotence — probing the same method twice gives equal ids") {
        val a = probeMethodId[Plain](_.threeArgs(slayer.any, slayer.any, slayer.any))
        val b = probeMethodId[Plain](_.threeArgs(slayer.any, slayer.any, slayer.any))
        assertTrue(a == b)
      },
      test("type-parameter param erases to Object") {
        val id = probeMethodId[Generic](_.gen1[String](slayer.any))
        assertTrue(id.jvmName == "gen1", id.erasedParams == List("Object"))
      },
      test("using clause is dropped from erasedParams") {
        val id = probeMethodId[WithImplicit](_.lookup(slayer.any))
        assertTrue(id.jvmName == "lookup", id.erasedParams == List("Int"))
      },
      test("curried lists are concatenated in order") {
        val id = probeMethodId[Plain](_.threeLists(slayer.any)(slayer.any)(slayer.any))
        assertTrue(id.jvmName == "threeLists")
        && assertTrue(id.erasedParams.size == 3)
        && assertTrue(id.erasedParams == List("Int", "String", "Boolean"))
      },
    ),
  )
end MacrosInternalSpec
