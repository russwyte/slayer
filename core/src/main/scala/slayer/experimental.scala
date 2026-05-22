package slayer.experimental

import scala.quoted.*

/** Bypass for `@experimental` on `Symbol.newClass` and `ClassDef.apply` in Scala 3.8.x.
  *
  * The public `quotes.reflect.Symbol.newClass` and `quotes.reflect.ClassDef.apply` overloads we need are flagged
  * `@experimental`, which would force every downstream user of slayer to enable `-experimental`. The compiler-internal
  * `scala.quoted.runtime.impl.QuotesImpl.reflect` exposes the same methods *without* the annotation.
  *
  * We cast the user-facing `Quotes` to `QuotesImpl`, do the call there, and cast the result back. From the call site's
  * perspective there is no experimental API in sight, so neither slayer nor its users need the flag.
  *
  * This is a known Scala 3 macro-community pattern (stubby uses it too); revisit when Scala promotes these APIs.
  */

def newClassSym(using
    Quotes
)(
    parent: quotes.reflect.Symbol,
    name: String,
    parents: List[quotes.reflect.TypeRepr],
    decls: quotes.reflect.Symbol => List[quotes.reflect.Symbol],
    selfType: Option[quotes.reflect.TypeRepr],
): quotes.reflect.Symbol =
  val iq = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl]
  val ir = iq.reflect

  val iparent   = parent.asInstanceOf[ir.Symbol]
  val iparents  = parents.asInstanceOf[List[ir.TypeRepr]]
  val idecls    = decls.asInstanceOf[ir.Symbol => List[ir.Symbol]]
  val iselfType = selfType.asInstanceOf[Option[ir.TypeRepr]]

  val isym = ir.Symbol.newClass(iparent, name, iparents, idecls, iselfType)

  isym.asInstanceOf[quotes.reflect.Symbol]
end newClassSym

def ClassDef(using
    Quotes
)(
    cls: quotes.reflect.Symbol,
    parents: List[quotes.reflect.Tree],
    body: List[quotes.reflect.Statement],
): quotes.reflect.ClassDef =
  val iq = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl]
  val ir = iq.reflect

  val icls     = cls.asInstanceOf[ir.Symbol]
  val iparents = parents.asInstanceOf[List[ir.Tree]]
  val ibody    = body.asInstanceOf[List[ir.Statement]]

  val idef = ir.ClassDef(icls, iparents, ibody)

  idef.asInstanceOf[quotes.reflect.ClassDef]
end ClassDef

/** Attach an `@targetName(name)` annotation to the given (synthesized) symbol.
  *
  * The `Quotes.reflect.Symbol` API has no surface for adding annotations to a synthesized symbol — `newMethod` and
  * `newClass` produce annotation-free symbols, and the reflect API exposes only read-side accessors (`hasAnnotation`,
  * `getAnnotation`, `annotations`). Without an annotation, an override of an abstract `@targetName`-renamed method is
  * rejected by the Scala compiler ("misses a target name annotation"), which would make `stubbed[Service]` fail for any
  * service whose abstract methods carry `@targetName`.
  *
  * The compiler-internal `dotty.tools.dotc.core.SymDenotations.SymDenotation` does expose `addAnnotation`, and the
  * `QuotesImpl` already in scope at macro expansion gives us the `Context` we need. We build the annotation tree (`new
  * targetName("bang")`) and attach it to the synthesized symbol's denotation directly. This is the same family of "cast
  * through QuotesImpl to reach an unsurfaced compiler API" trick we already use for `Symbol.newClass` /
  * `ClassDef.apply` — see top of file.
  *
  * Revisit when (if) Scala promotes a public `Symbol.addAnnotation` API.
  */
def addTargetNameAnnotation(using Quotes)(sym: quotes.reflect.Symbol, name: String): Unit =
  import dotty.tools.dotc.ast.tpd
  import dotty.tools.dotc.core.Annotations
  import dotty.tools.dotc.core.Constants.Constant
  import dotty.tools.dotc.core.Contexts.Context
  import dotty.tools.dotc.core.Symbols
  import dotty.tools.dotc.util.Spans.NoSpan

  val iq                            = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl]
  given ctx: Context                = iq.ctx
  val dottySym: Symbols.Symbol      = sym.asInstanceOf[Symbols.Symbol]
  val targetNameCls: Symbols.Symbol = Symbols.requiredClass("scala.annotation.targetName")
  val argTree: tpd.Tree             = tpd.Literal(Constant(name))
  val annot: Annotations.Annotation =
    Annotations.Annotation(targetNameCls.asClass, argTree :: Nil, NoSpan)
  dottySym.denot.addAnnotation(annot)
end addTargetNameAnnotation
