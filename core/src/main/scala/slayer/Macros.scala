package slayer

import scala.quoted.*

/** Compile-time machinery for slayer.
  *
  * This object grows in milestones (see plan): M1 is just the reflection helpers (`peelMethod`, `flatten`,
  * `methodIdOf`) and a probe object that lets tests exercise them in isolation. M2+ adds `stubImpl` and `stubbedImpl`.
  */
private[slayer] object Macros:

  // -------------------------------------------------------------------------------------------------------------------
  // peelMethod
  // -------------------------------------------------------------------------------------------------------------------

  /** Walk a selector-lambda body to find the underlying method `Select` and any type arguments applied to it.
    *
    *   - Strips outer `Apply` / `TypeApply` chains so multi-parameter-list calls (`_.foo(a)(b)(c)`) and type-applied
    *     calls (`_.foo[String]`) both resolve to the same root.
    *   - Descends through `Inlined` and `Block` wrappers introduced by `inline def` expansion.
    *   - The returned `Symbol` is the typer-resolved overload (the typer has already chosen, so plain Scala overloads
    *     are disambiguated by the `Select`'s symbol). For `@targetName`-disambiguated overloads, the annotation must be
    *     read explicitly; see `jvmNameOf`.
    *
    * Returns the *innermost* type-argument list encountered. For `_.foo[String](42)`, the `Apply(TypeApply(...))`
    * structure puts the type args on the inner `TypeApply`, so we collect them on the way down.
    */
  def peelMethod(using Quotes)(t: quotes.reflect.Term): Option[(quotes.reflect.Symbol, List[quotes.reflect.TypeRepr])] =
    import quotes.reflect.*

    def loop(term: Term, accTypeArgs: List[TypeRepr]): Option[(Symbol, List[TypeRepr])] =
      term match
        case Apply(inner, _)         => loop(inner, accTypeArgs)
        case TypeApply(inner, targs) =>
          // Inner-most TypeApply wins (a method generic in two scopes is rare; we capture the closest layer).
          loop(inner, if accTypeArgs.isEmpty then targs.map(_.tpe) else accTypeArgs)
        case sel: Select if sel.symbol.flags.is(Flags.Method) =>
          Some(sel.symbol -> accTypeArgs)
        // A Scala 3 lambda after `underlyingArgument` is desugared to:
        //   Block(List(DefDef("$anonfun", ..., rhs)), Closure(Ident("$anonfun"), _))
        // We recurse into the DefDef's rhs to find the method selection.
        case Block(List(d: DefDef), _: Closure) =>
          d.rhs.flatMap(loop(_, accTypeArgs))
        case Block(_, expr)      => loop(expr, accTypeArgs)
        case Inlined(_, _, expr) => loop(expr, accTypeArgs)
        case Lambda(_, body)     => loop(body, accTypeArgs)
        case Typed(expr, _)      => loop(expr, accTypeArgs)
        case _                   => None
      end match
    end loop

    loop(t, Nil)
  end peelMethod

  // -------------------------------------------------------------------------------------------------------------------
  // flatten
  // -------------------------------------------------------------------------------------------------------------------

  /** Decompose a method's type into:
    *
    *   - type-parameter names (from the outermost `PolyType`, if any),
    *   - the flat list of value parameter types paired with an "is contextual" flag (one entry per parameter, across
    *     all parameter lists). The flag is `true` for both Scala 3 `using` (contextual) and Scala 2 `(implicit ...)`
    *     parameter lists.
    *   - the final return type.
    *
    * Contextual / implicit parameter lists are kept in the returned list so callers see the full shape, but flagged so
    * they can be dropped before going into the runtime args array.
    *
    * Note: `PolyType` does not expose type-parameter `Symbol`s through the `Quotes` API; we return `paramNames`.
    */
  def flatten(using
      Quotes
  )(
      repr: quotes.reflect.TypeRepr
  ): (List[String], List[(quotes.reflect.TypeRepr, Boolean)], quotes.reflect.TypeRepr) =
    import quotes.reflect.*

    repr.widenTermRefByName match
      case pt: PolyType =>
        val (_, params, ret) = flatten(pt.resType)
        (pt.paramNames, params, ret)
      case mt: MethodType =>
        val (typeParams, params, ret) = flatten(mt.resType)
        val isCtx                     = mt.isImplicit || mt.isContextual
        val thisList                  = mt.paramTypes.map(_ -> isCtx)
        (typeParams, thisList ++ params, ret)
      case other => (Nil, Nil, other)
    end match
  end flatten

  // -------------------------------------------------------------------------------------------------------------------
  // methodIdOf
  // -------------------------------------------------------------------------------------------------------------------

  /** Build a `MethodId` literal at compile time from a method symbol.
    *
    *   - `jvmName` is read via `jvmNameOf`, which honors `@targetName` (the annotation, not `Symbol.name`). Two methods
    *     with the same source name but different targetNames produce different `jvmName`s.
    *   - `erasedParams` is the per-value-parameter erased type names, in declaration order, *excluding* implicit
    *     (`using`) parameters. Plain Scala overloads are disambiguated here: `def f(i: Int)` and `def f(s: String)`
    *     share `jvmName = "f"` but differ in `erasedParams`.
    */
  def methodIdOf(using Quotes)(sym: quotes.reflect.Symbol): Expr[MethodId] =
    val jvmName = jvmNameOf(sym)
    val erased  = flatten(sym.termRef)._2.collect { case (tpe, false) => erasedTypeName(tpe) }
    '{ MethodId(${ Expr(jvmName) }, ${ Expr(erased) }) }

  /** Resolve the JVM-visible name of a method symbol.
    *
    * `Symbol.name` returns the Scala source name. To honor `@targetName`, we look for the annotation explicitly and
    * read its single string argument. This is what the JVM bytecode carries, so two `@targetName`-disambiguated
    * siblings end up with distinct `jvmName`s here even though their `Symbol.name` collides.
    */
  private def jvmNameOf(using Quotes)(sym: quotes.reflect.Symbol): String =
    import quotes.reflect.*
    val targetNameSym = Symbol.requiredClass("scala.annotation.targetName")
    sym.getAnnotation(targetNameSym) match
      case Some(Apply(_, List(Literal(StringConstant(name))))) => name
      case _                                                   => sym.name
  end jvmNameOf

  /** Best-effort erased name for a `TypeRepr`. Used as a string key for plain-overload disambiguation.
    *
    * We deliberately avoid relying on JVM-level `Class.getName` because the macro must work for unloaded classes,
    * including the service trait being stubbed. `dealias.show` is stable across the same compiler version and is
    * sufficient for keying — what matters is that two distinct overloads of the same source name produce distinct
    * strings, not that the string match the JVM erasure exactly.
    *
    * For type-parameter references that erase to `Object`, we explicitly emit `"Object"` so two overloads
    * `def f[A](a: A)` and `def f(a: AnyRef)` collide as the JVM would — surfacing the limitation rather than masking
    * it.
    */
  private def erasedTypeName(using Quotes)(tpe: quotes.reflect.TypeRepr): String =
    import quotes.reflect.*

    val widened = tpe.dealias.widen
    widened match
      // A method's PolyType-bound parameter appears as a ParamRef pointing at the binding PolyType.
      case _: ParamRef => "Object"
      // Fall-through for `def f[A](a: A)` shapes where the param surfaces as a TypeRef to a Flags.Param symbol.
      case ref: TypeRef if ref.typeSymbol.flags.is(Flags.Param) => "Object"
      case _                                                    => widened.show(using Printer.TypeReprShortCode)
    end match
  end erasedTypeName

  // -------------------------------------------------------------------------------------------------------------------
  // stubImpl — value-result branch (M2) and function-handler branch (M3)
  // -------------------------------------------------------------------------------------------------------------------

  /** Macro implementation for `slayer.stub`.
    *
    * Two branches:
    *
    *   - **Value branch**: if `result` typechecks as the method's final return type, emit `Stubbed.insertValue`.
    *   - **Function branch (M3)**: if `result` is a chain of `FunctionN[..., R]` whose flattened parameter list matches
    *     the method's flattened *non-implicit* value parameters and whose final return matches the method return, emit
    *     an `Array[Any] => Any` adapter that captures the handler in a closure and applies it.
    *
    * Handler shape need not mirror method shape — a curried 2-list method can be stubbed with `(a, b) => r` (tupled) or
    * `a => b => r` (curried). The unrolling reads from the *handler*'s function chain and pulls args off the array in
    * declaration order.
    *
    * The selector lambda's *typed* tree carries any type-argument substitutions the user wrote (`_.get[String](...)`),
    * so the return-type check is done against the lambda body's `.tpe`, not the un-substituted symbol's `termRef`.
    */
  def stubImpl[Service: Type](
      select: Expr[Service => Any],
      result: Expr[Any],
  )(using Quotes): Expr[zio.URIO[Stubbed[Service], Unit]] =
    import quotes.reflect.*

    val tree = select.asTerm.underlyingArgument
    peelMethod(tree) match
      case None =>
        report.errorAndAbort(
          s"stub: selector must reference a method on ${TypeRepr.of[Service].show}, but got: ${select.show}"
        )
      case Some((sym, _)) =>
        val expectedReturn = selectorReturnType(tree)
        val resultTpe      = result.asTerm.tpe.widen

        val tagExpr = Expr.summon[zio.Tag[Service]].getOrElse {
          report.errorAndAbort(
            s"stub: could not summon zio.Tag[${TypeRepr.of[Service].show}] required by Stubbed insertion"
          )
        }
        val id = methodIdOf(sym)

        if resultTpe <:< expectedReturn then
          // Value branch — store as-is. We tag the stored value with the method's return type so the runtime cast in
          // `Stubbed.callStubbed` succeeds.
          expectedReturn.asType match
            case '[r] =>
              val typedV = '{ $result.asInstanceOf[r] }
              '{ Stubbed.insertValue[Service, r]($id, $typedV)(using $tagExpr) }
        else
          // Function branch — try to interpret `result` as a Function* whose flat params match the method's flat
          // non-implicit params.
          peelFunctionParams(resultTpe) match
            case Some((handlerParams, handlerReturn)) =>
              val methodValueParams = selectorValueParamTypes(tree, sym)

              if !(handlerReturn <:< expectedReturn) then
                report.errorAndAbort(
                  s"""stub: handler return type does not match method return type.
                     |  method:   ${sym.name}
                     |  expected: ${expectedReturn.show}
                     |  got:      ${handlerReturn.show}""".stripMargin
                )

              if handlerParams.size != methodValueParams.size then
                report.errorAndAbort(
                  s"""stub: handler arity does not match method arity.
                     |  method:   ${sym.name} expects ${methodValueParams.size} value parameter(s)
                     |  handler:  ${handlerParams.size} parameter(s)""".stripMargin
                )

              handlerParams.zip(methodValueParams).zipWithIndex.foreach { case ((hp, mp), i) =>
                if !(mp <:< hp) then
                  report.errorAndAbort(
                    s"""stub: handler parameter ${i} type does not accept method parameter type.
                       |  method param:  ${mp.show}
                       |  handler param: ${hp.show}""".stripMargin
                  )
              }

              val adapter = buildAdapter(result, resultTpe)
              '{ Stubbed.insertFunc[Service]($id, $adapter)(using $tagExpr) }

            case None =>
              report.errorAndAbort(
                s"""stub: result type does not match method return type, and is not a function handler.
                   |  method:   ${sym.name}
                   |  expected: ${expectedReturn.show}
                   |  got:      ${resultTpe.show}""".stripMargin
              )
          end match
        end if
    end match
  end stubImpl

  /** Peel a chain of `Function1`/`Function2`/.../`FunctionN` types into a flat parameter list and the final return.
    *
    * Examples:
    *
    *   - `(A, B) => R` → `Some((List(A, B), R))`
    *   - `A => B => R` → `Some((List(A, B), R))`
    *   - `(A, B) => C => R` → `Some((List(A, B, C), R))`
    *   - `Int` → `None`
    *
    * A `scala.FunctionN` `AppliedType` has `args = [a1, ..., an, r]`. We split off the last one as the return and
    * recurse into it; any non-`FunctionN` type terminates the recursion.
    */
  private def peelFunctionParams(using
      Quotes
  )(tpe: quotes.reflect.TypeRepr): Option[(List[quotes.reflect.TypeRepr], quotes.reflect.TypeRepr)] =
    import quotes.reflect.*

    def loop(t: TypeRepr, acc: List[TypeRepr]): (List[TypeRepr], TypeRepr) =
      t.dealias match
        case AppliedType(tycon, args) if isFunctionTycon(tycon) && args.nonEmpty =>
          val ps  = args.init
          val ret = args.last
          loop(ret, acc ++ ps)
        case other => (acc, other)
      end match
    end loop

    val (params, ret) = loop(tpe, Nil)
    if params.isEmpty then None else Some((params, ret))
  end peelFunctionParams

  /** Recognise `scala.FunctionN` type constructors. Checks the type symbol's full name to avoid hard-coding arity. */
  private def isFunctionTycon(using Quotes)(tycon: quotes.reflect.TypeRepr): Boolean =
    val name = tycon.typeSymbol.fullName
    name.startsWith("scala.Function") && {
      val suffix = name.stripPrefix("scala.Function")
      suffix.nonEmpty && suffix.forall(_.isDigit)
    }
  end isFunctionTycon

  /** Extract the method's *substituted* value-parameter types from the typed selector tree.
    *
    * Walking the tree's `Apply` chain (after stripping outer `Inlined`/`Block`/`Closure` wrappers) gives us the typed
    * arguments — their `.tpe` is the method's substituted parameter type. This is how we get `Int` (not `A`) for
    * `_.gen1[Int](any)`. Implicit/`using` parameters are dropped here, matching what we drop in
    * `MethodId.erasedParams`.
    */
  private def selectorValueParamTypes(using
      Quotes
  )(tree: quotes.reflect.Term, sym: quotes.reflect.Symbol): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    def lambdaBody(t: Term): Term =
      t match
        case Block(List(d: DefDef), _: Closure) => d.rhs.getOrElse(t)
        case Block(_, expr)                     => lambdaBody(expr)
        case Inlined(_, _, expr)                => lambdaBody(expr)
        case Lambda(_, body)                    => lambdaBody(body)
        case Typed(expr, _)                     => lambdaBody(expr)
        case other                              => other
      end match
    end lambdaBody

    def applyArgs(t: Term, acc: List[List[Term]]): List[List[Term]] =
      t match
        case Apply(inner, args)  => applyArgs(inner, args :: acc)
        case TypeApply(inner, _) => applyArgs(inner, acc)
        case _                   => acc
      end match
    end applyArgs

    val body                       = lambdaBody(tree)
    val argLists: List[List[Term]] = applyArgs(body, Nil)

    if argLists.isEmpty then
      // No-arg method; nothing to flatten from the tree. Fall back to `flatten(sym.termRef)` minus ctx params.
      flatten(sym.termRef)._2.collect { case (t, false) => t }
    else
      // The typer preserves implicit lists as their own `Apply` layers; `flatten(sym.termRef)` tells us which lists are
      // implicit. We pair the captured argument lists with the symbol's parameter-list flags and drop implicits.
      val flagsByList = parameterListFlags(sym.termRef)
      argLists
        .zip(flagsByList)
        .collect { case (args, false) =>
          args.map(_.tpe.widen)
        }
        .flatten
    end if
  end selectorValueParamTypes

  /** For each parameter list of a method type, return whether that list is contextual/implicit. The list order matches
    * the method's declaration order (poly type-param lists are skipped — they don't show up as `Apply` layers).
    */
  private def parameterListFlags(using Quotes)(repr: quotes.reflect.TypeRepr): List[Boolean] =
    import quotes.reflect.*
    repr.widenTermRefByName match
      case pt: PolyType   => parameterListFlags(pt.resType)
      case mt: MethodType => (mt.isImplicit || mt.isContextual) :: parameterListFlags(mt.resType)
      case _              => Nil
  end parameterListFlags

  /** Build the `Array[Any] => Any` adapter that applies the user's handler to a flat array of arguments.
    *
    * For arity N, regardless of whether the handler is tupled or curried, we capture the handler in a quote and use
    * `applyHandler` (a small set of inlined helpers) to do the per-arity application. We choose the helper at compile
    * time based on the handler's *type chain* — `peelFunctionParams` already gave us the flat count; the closure that
    * captures `$result` keeps its original (curried or tupled) shape, and we apply it accordingly.
    *
    * For up to arity 22 we can rely on `Function*.apply` being directly callable by spelling out the call as a tree;
    * past that, the handler value cannot exist as a Scala 3 function literal anyway, so we only need to support the
    * shapes the user can actually write.
    */
  private def buildAdapter(using
      Quotes
  )(
      result: Expr[Any],
      resultTpe: quotes.reflect.TypeRepr,
  ): Expr[Array[Any] => Any] =
    import quotes.reflect.*

    val mt        = MethodType(List("args"))(_ => List(TypeRepr.of[Array[Any]]), _ => TypeRepr.of[Any])
    val lambdaSym = Symbol.newMethod(Symbol.spliceOwner, "$slayerAdapter", mt)
    val rhsFn: List[List[Tree]] => Option[Term] = {
      case List(List(argsParam: Term)) => Some(applyHandlerChain(result.asTerm, resultTpe, argsParam))
      case other                       => report.errorAndAbort(s"unexpected adapter param shape: $other")
    }
    val defdef  = DefDef(lambdaSym, rhsFn)
    val closure = Closure(Ref(lambdaSym), Some(TypeRepr.of[Array[Any] => Any]))
    Block(List(defdef), closure).asExprOf[Array[Any] => Any]
  end buildAdapter

  /** Emit a Term that walks the handler's `Function*` chain, pulling args off the `args: Array[Any]` parameter and
    * casting each to the handler's declared param type. Recurses through nested `FunctionN` returns (curried handlers).
    */
  private def applyHandlerChain(using
      Quotes
  )(
      handler: quotes.reflect.Term,
      handlerTpe: quotes.reflect.TypeRepr,
      argsRef: quotes.reflect.Term,
  ): quotes.reflect.Term =
    import quotes.reflect.*

    def loop(currentFn: Term, currentTpe: TypeRepr, offset: Int): Term =
      currentTpe.dealias match
        case AppliedType(tycon, args) if isFunctionTycon(tycon) && args.nonEmpty =>
          val paramTpes = args.init
          val retTpe    = args.last
          val applyArgs = paramTpes.zipWithIndex.map { case (pt, i) =>
            val idx     = offset + i
            val rawElem = Apply(Select.unique(argsRef, "apply"), List(Literal(IntConstant(idx))))
            castTo(rawElem, pt)
          }
          val applied = Apply(Select.unique(currentFn, "apply"), applyArgs)
          loop(applied, retTpe, offset + paramTpes.size)
        case _ => currentFn
      end match
    end loop

    loop(handler, handlerTpe, 0)
  end applyHandlerChain

  /** Cast a term `t` to type `target`, emitting `t.asInstanceOf[target]`. Boxes through `Any` first so primitive casts
    * go through Scala's runtime unboxing.
    */
  private def castTo(using Quotes)(t: quotes.reflect.Term, target: quotes.reflect.TypeRepr): quotes.reflect.Term =
    import quotes.reflect.*
    target.asType match
      case '[a] => '{ ${ t.asExprOf[Any] }.asInstanceOf[a] }.asTerm

  /** Walk the selector lambda's tree to find the type of the body — i.e. the method's return type *after* the typer has
    * substituted any user-supplied type arguments.
    *
    * For `_.get[String](any)`, peeking at `sym.termRef.resType` would give `UIO[A]` (un-substituted). We want
    * `UIO[String]`, which lives on the body of the lambda. The body is whatever sits inside the
    * `Block(DefDef, Closure)` (after `underlyingArgument`); for fallback we also handle bare `Inlined`/`Block`/`Lambda`
    * wrappers.
    */
  private def selectorReturnType(using Quotes)(t: quotes.reflect.Term): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    def body(term: Term): Term =
      term match
        case Block(List(d: DefDef), _: Closure) => d.rhs.getOrElse(term)
        case Block(_, expr)                     => body(expr)
        case Inlined(_, _, expr)                => body(expr)
        case Lambda(_, b)                       => body(b)
        case Typed(expr, _)                     => body(expr)
        case other                              => other
      end match
    end body

    body(t).tpe.widen
  end selectorReturnType

  // -------------------------------------------------------------------------------------------------------------------
  // stubbedImpl — synthesized service class (M4)
  // -------------------------------------------------------------------------------------------------------------------

  /** Macro implementation for `slayer.stubbed`. Synthesizes a class extending `Service` and `Stubbed[Service]`. Every
    * declared method on `Service` is implemented by packing its arguments into `Array[Any]` and calling
    * `this.callStubbed[ReturnType](id, args)`.
    *
    * **M4 scope:** simple methods only — single value parameter list, no type parameters, no contextual/`using`
    * parameters. Curried lists, implicits, and generics land in M5.
    *
    * The class itself uses `Symbol.newClass` / `ClassDef`, both of which are `@experimental` on Scala 3.8.x. We route
    * those calls through `slayer.experimental` (which casts to `QuotesImpl`) so neither slayer nor downstream users
    * need the `-experimental` flag.
    */
  def stubbedImpl[Service: Type](using Quotes): Expr[zio.ULayer[Service & Stubbed[Service]]] =
    import quotes.reflect.*

    val serviceRepr = TypeRepr.of[Service]
    val serviceSym  = serviceRepr.typeSymbol
    // Synthesize overrides for both abstract and concrete trait methods, but skip:
    //   - `<name>$default$<n>` getters: synthetic, inherited from the trait at the JVM level — overriding them would
    //     produce duplicate-method JVM errors.
    //   - any other Synthetic-flagged member: e.g. case-class accessors on data traits, equality probes, etc.
    // Abstract methods get a `callStubbed` body (throws NoStub when missing). Concrete methods get a
    // `callStubbedOrElse` body that falls through to the trait's impl via super.<method>(...) when no stub is set.
    val methods = serviceSym.declaredMethods.filter { m =>
      !m.flags.is(Flags.Synthetic) && !m.name.contains("$default$")
    }

    val tagExpr = Expr.summon[zio.Tag[Service]].getOrElse {
      report.errorAndAbort(
        s"stubbed: could not summon zio.Tag[${serviceRepr.show}] required to build the layer"
      )
    }

    val parents = List(TypeRepr.of[Object], serviceRepr, TypeRepr.of[Stubbed[Service]])

    val targetNameSym = Symbol.requiredClass("scala.annotation.targetName")

    /** Read the source-level `@targetName(...)` argument on `sym`, if present. */
    def declaredTargetName(sym: Symbol): Option[String] =
      sym.getAnnotation(targetNameSym).flatMap {
        case Apply(_, List(Literal(StringConstant(s)))) => Some(s)
        case _                                          => None
      }

    def decls(cls: Symbol): List[Symbol] =
      methods.map { m =>
        // Concrete trait methods need `Flags.Override` on the synthesized symbol — otherwise the compiler rejects the
        // class with "method needs `override` modifier". Abstract methods (Deferred) need no flag.
        val flags  = if m.flags.is(Flags.Deferred) then Flags.EmptyFlags else Flags.Override
        val newSym = Symbol.newMethod(cls, m.name, serviceRepr.memberType(m), flags, Symbol.noSymbol)
        // If the trait method carries `@targetName("foo")`, the override must too — otherwise the compiler rejects
        // the override with "misses a target name annotation". Quotes' newMethod gives no way to attach annotations,
        // so we reach into the compiler-internal SymDenotation via `experimental.addTargetNameAnnotation`.
        declaredTargetName(m).foreach(name => experimental.addTargetNameAnnotation(newSym, name))
        newSym
      }

    val cls = experimental.newClassSym(
      Symbol.spliceOwner,
      s"${serviceSym.name}Stubbed",
      parents,
      decls,
      selfType = None,
    )

    val thisExpr = This(cls).asExprOf[Service & Stubbed[Service]]

    // Build one DefDef per declared method on the synthesized class. Abstract trait methods → callStubbed (throws
    // NoStub). Concrete trait methods → callStubbedOrElse with a super-call fallback so the trait's impl runs when
    // no stub is registered.
    val body: List[Statement] = cls.declaredMethods.map { method =>
      val methodIdExpr = methodIdOf(method)
      val listFlags    = parameterListFlags(method.termRef)
      // Look up the trait method this override targets — needed for the super-call when the trait method is concrete.
      val traitMethod = methods.find(_.name == method.name).getOrElse(method)
      val isConcrete  = !traitMethod.flags.is(Flags.Deferred)

      DefDef(
        method,
        argss =>
          Some {
            val (typeArgLists, valueArgLists) = argss.partition {
              case Nil                => false
              case (_: TypeTree) :: _ => true
              case _                  => false
            }
            val typeArgs: List[TypeRepr] =
              typeArgLists.flatten.collect { case tt: TypeTree => tt.tpe }

            def finalReturnOf(t: TypeRepr): TypeRepr = t match
              case mt: MethodType => finalReturnOf(mt.resType)
              case pt: PolyType   =>
                if typeArgs.isEmpty then finalReturnOf(pt.resType)
                else finalReturnOf(pt.appliedTo(typeArgs))
              case other => other
            val finalReturn = finalReturnOf(method.termRef.widenTermRefByName)

            val keptLists =
              if valueArgLists.size == listFlags.size then valueArgLists.zip(listFlags).collect { case (l, false) => l }
              else valueArgLists
            val termArgs = keptLists.flatten.collect { case t: Term => t.asExprOf[Any] }
            val arr      = '{ Array[Any](${ Varargs(termArgs) }*) }

            finalReturn.asType match
              case '[r] =>
                if isConcrete then
                  // Build `super.<method>[typeArgs](args0)(args1)...` — passes ALL value lists (including using/implicit)
                  // through to the trait so the original method semantics are preserved on fallback.
                  val superSelect: Term  = Select(Super(This(cls), None), traitMethod)
                  val withTypeArgs: Term =
                    if typeArgs.isEmpty then superSelect
                    else TypeApply(superSelect, typeArgs.map(t => TypeTree.of(using t.asType)))
                  val superCall: Term =
                    valueArgLists.foldLeft(withTypeArgs) { (acc, list) =>
                      Apply(acc, list.collect { case t: Term => t })
                    }
                  val fallback = superCall.asExprOf[r]
                  '{ $thisExpr.callStubbedOrElse[r]($methodIdExpr, $arr, $fallback) }.asTerm
                else '{ $thisExpr.callStubbed[r]($methodIdExpr, $arr) }.asTerm
            end match
          },
      )
    }

    val classDef = experimental.ClassDef(
      cls,
      parents.map(p => TypeTree.of(using p.asType)),
      body,
    )

    val newInstance = Typed(
      Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil),
      TypeTree.of[Service & Stubbed[Service]],
    )

    val instanceExpr =
      Block(List(classDef), newInstance).asExprOf[Service & Stubbed[Service]]

    '{
      zio.ZLayer.fromZIOEnvironment {
        zio.ZIO.succeed {
          val s = $instanceExpr
          zio.ZEnvironment[Service, Stubbed[Service]](s, s)(using $tagExpr, summon[zio.Tag[Stubbed[Service]]])
        }
      }
    }
  end stubbedImpl

  // -------------------------------------------------------------------------------------------------------------------
  // Probe API — for use only by tests in src/test/scala/slayer/MacrosInternalSpec.scala
  // -------------------------------------------------------------------------------------------------------------------

  /** Compile-time-captured snapshot of `peelMethod`'s output for testing. */
  final case class PeelProbe(
      foundMethod: Boolean,
      methodName: Option[String],
      typeArgs: List[String],
  )

  /** Compile-time-captured snapshot of `flatten`'s output for testing. */
  final case class FlattenProbe(
      typeParams: List[String],
      valueParams: List[(String, Boolean)], // (typeName, isImplicit)
      returnType: String,
  )

  /** Probe `peelMethod` against a selector lambda. The macro picks the lambda apart and returns whatever the helper
    * found; if no method is found it returns `PeelProbe(false, None, Nil)`.
    */
  inline def probePeel[Service](inline sel: Service => Any): PeelProbe = ${ probePeelImpl[Service]('sel) }

  private def probePeelImpl[Service: Type](sel: Expr[Service => Any])(using Quotes): Expr[PeelProbe] =
    import quotes.reflect.*

    val tree = sel.asTerm.underlyingArgument
    peelMethod(tree) match
      case Some((sym, targs)) =>
        val name     = Expr(jvmNameOf(sym))
        val targStrs = Expr(targs.map(_.show))
        '{ PeelProbe(true, Some($name), $targStrs) }
      case None =>
        '{ PeelProbe(false, None, Nil) }
    end match
  end probePeelImpl

  /** Probe `flatten` against a method's type. Selects the method via `peelMethod`, then flattens its `termRef`. */
  inline def probeFlatten[Service](inline sel: Service => Any): FlattenProbe = ${ probeFlattenImpl[Service]('sel) }

  private def probeFlattenImpl[Service: Type](sel: Expr[Service => Any])(using Quotes): Expr[FlattenProbe] =
    import quotes.reflect.*

    val tree = sel.asTerm.underlyingArgument
    peelMethod(tree) match
      case Some((sym, _)) =>
        val (typeParams, valueParams, ret) = flatten(sym.termRef)
        val tps                            = Expr(typeParams)
        val vps                            = Expr(valueParams.map { case (t, isCtx) => (t.show, isCtx) })
        val r                              = Expr(ret.show)
        '{ FlattenProbe($tps, $vps, $r) }
      case None =>
        report.errorAndAbort(s"probeFlatten: could not find a method symbol in selector ${sel.show}")
    end match
  end probeFlattenImpl

  /** Probe `methodIdOf` against a method symbol resolved from a selector. */
  inline def probeMethodId[Service](inline sel: Service => Any): MethodId = ${ probeMethodIdImpl[Service]('sel) }

  private def probeMethodIdImpl[Service: Type](sel: Expr[Service => Any])(using Quotes): Expr[MethodId] =
    import quotes.reflect.*

    val tree = sel.asTerm.underlyingArgument
    peelMethod(tree) match
      case Some((sym, _)) => methodIdOf(sym)
      case None => report.errorAndAbort(s"probeMethodId: could not find a method symbol in selector ${sel.show}")
  end probeMethodIdImpl

  /** Build a `MethodId` for every declared method of a service trait, keyed by `Symbol.name` (which is the
    * targetName-rewritten JVM name). Used to test cases where `@targetName` siblings can't be referenced by source name
    * from a selector lambda.
    */
  inline def probeMethodIdsByName[Service]: List[(String, MethodId)] = ${ probeMethodIdsByNameImpl[Service] }

  private def probeMethodIdsByNameImpl[Service: Type](using Quotes): Expr[List[(String, MethodId)]] =
    import quotes.reflect.*

    val tpe                                   = TypeRepr.of[Service]
    val syms                                  = tpe.typeSymbol.declaredMethods
    val pairs: List[Expr[(String, MethodId)]] = syms.map { s =>
      val nameExpr = Expr(s.name)
      val idExpr   = methodIdOf(s)
      '{ ($nameExpr, $idExpr) }
    }
    Expr.ofList(pairs)
  end probeMethodIdsByNameImpl

  /** Compile-time-captured shape of `DefDef`'s `argss` parameter when synthesizing a method on a class for a given
    * service member. We synthesize a tiny class with one method (the named one), then in the body callback record what
    * `argss` looks like as a structural string. Used to drive M5 design from first-principles instead of guesswork.
    */
  final case class ArgssProbe(
      methodName: String,
      listCount: Int,
      perListSizes: List[Int],
      perListElementClasses: List[List[String]],
      perListElementShows: List[List[String]],
      methodReturnTypeShow: String,
      // Extras to understand generic-method substitution.
      methodTermRefRaw: String,    // m.termRef.show — the full polymorphic type
      typeTreeTpes: List[String],  // for any TypeTree elements in argss, their .tpe.show
      valueIdentTpes: List[String], // for any Term elements, their .tpe.show
  )

  inline def probeDefDefArgss[Service](methodName: String): ArgssProbe =
    ${ probeDefDefArgssImpl[Service]('methodName) }

  private def probeDefDefArgssImpl[Service: Type](methodName: Expr[String])(using Quotes): Expr[ArgssProbe] =
    import quotes.reflect.*

    val name        = methodName.valueOrAbort
    val serviceRepr = TypeRepr.of[Service]
    val serviceSym  = serviceRepr.typeSymbol
    val target      = serviceSym.declaredMethods
      .find(_.name == name)
      .getOrElse(report.errorAndAbort(s"probeDefDefArgss: no method $name on ${serviceRepr.show}"))

    var captured: Option[
      (Int, List[Int], List[List[String]], List[List[String]], String, String, List[String], List[String])
    ] = None

    def decls(cls: Symbol): List[Symbol] =
      List(Symbol.newMethod(cls, target.name, serviceRepr.memberType(target)))

    val cls = experimental.newClassSym(
      Symbol.spliceOwner,
      "ArgssProbeCls",
      List(TypeRepr.of[Object]),
      decls,
      selfType = None,
    )

    cls.declaredMethods.foreach { m =>
      DefDef(
        m,
        argss =>
          val classes = argss.map(_.map(_.getClass.getSimpleName))
          val shows   = argss.map(_.map {
            case t: Term      => t.tpe.show
            case tt: TypeTree => "TypeTree:" + tt.tpe.show
            case other        => other.toString
          })
          val ret = m.termRef.widenTermRefByName match
            case mt: MethodType => mt.resType.show
            case pt: PolyType   => pt.resType.show
            case other          => other.show
          val termRefRaw     = m.termRef.show
          val typeTreeTpes   = argss.flatten.collect { case tt: TypeTree => tt.tpe.show }
          val valueIdentTpes = argss.flatten.collect { case t: Term => t.tpe.show }
          captured = Some(
            (argss.size, argss.map(_.size), classes, shows, ret, termRefRaw, typeTreeTpes, valueIdentTpes)
          )
          // Return null literal of the appropriate type just to avoid further crashes; we only care about argss.
          Some(Literal(NullConstant())),
      )
    }

    val (count, sizes, classes, shows, ret, termRefRaw, typeTreeTpes, valueIdentTpes) =
      captured.getOrElse((0, Nil, Nil, Nil, "?", "?", Nil, Nil))
    '{
      ArgssProbe(
        ${ Expr(name) },
        ${ Expr(count) },
        ${ Expr(sizes) },
        ${ Expr(classes) },
        ${ Expr(shows) },
        ${ Expr(ret) },
        ${ Expr(termRefRaw) },
        ${ Expr(typeTreeTpes) },
        ${ Expr(valueIdentTpes) },
      )
    }
  end probeDefDefArgssImpl

end Macros
