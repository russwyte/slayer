package slayer

import zio.*

import java.util.concurrent.ConcurrentHashMap

/** Identity of a stubbed method.
  *
  *   - `jvmName` is the JVM-visible name. Where Scala uses `@targetName("a1")`, the JVM name is `a1`, so two methods
  *     that share a Scala source name but differ in `@targetName` produce distinct ids.
  *   - `erasedParams` is the list of erased parameter type names, flattened across all value parameter lists
  *     (implicit/`using` lists are excluded). This disambiguates plain Scala overloads that share `jvmName`.
  */
final case class MethodId(jvmName: String, erasedParams: List[String])

/** Stored stub: either a concrete value or an `Array[Any] => Any` adapter built by the macro. */
sealed trait StubResult:
  def call(args: Array[Any]): Any
object StubResult:
  final case class Value(value: Any) extends StubResult:
    def call(args: Array[Any]): Any = value
  final case class Func(f: Array[Any] => Any) extends StubResult:
    def call(args: Array[Any]): Any = f(args)

/** Runtime store backing a synthesized service implementation.
  *
  * One `Stubbed[Service]` is materialized per layer. The macro-synthesized service class extends both `Service` and
  * `Stubbed[Service]`; its method bodies all delegate to `callStubbed` after packing arguments into an `Array[Any]`.
  *
  * The map is a plain `ConcurrentHashMap` rather than a `Ref[Map[...]]` because reads from the synthesized class are
  * synchronous (a `ZIO` cannot be returned from an arbitrary trait method), so we need a thread-safe store with a
  * synchronous `get` API.
  */
trait Stubbed[A]:
  protected[slayer] val ref: ConcurrentHashMap[MethodId, StubResult] = new ConcurrentHashMap()

  protected[slayer] def insertValueImpl(id: MethodId, response: Any): UIO[Unit] =
    ZIO.succeed { ref.put(id, StubResult.Value(response)); () }

  protected[slayer] def insertFuncImpl(id: MethodId, f: Array[Any] => Any): UIO[Unit] =
    ZIO.succeed { ref.put(id, StubResult.Func(f)); () }

  /** Look up and invoke a stub. Throws `SlayerError.NoStub` synchronously when no stub is registered. */
  def callStubbed[R](id: MethodId, args: Array[Any]): R =
    val r = ref.get(id)
    if r == null then throw SlayerError.NoStub(id)
    else r.call(args).asInstanceOf[R]

  /** Look up a stub; if none is registered, evaluate `fallback` and return its result. Used by the synthesized class
    * for concrete trait methods, so that `_.hello` falls through to the trait's `hello` impl when no stub overrides it.
    * `fallback` is by-name so the macro can pass a `super.<method>(args)` expression directly without wrapping it in a
    * `() =>` lambda — which would fail with a "could not find proxy" error because the lambda body captures the
    * override method's parameter symbols.
    */
  def callStubbedOrElse[R](id: MethodId, args: Array[Any], fallback: => R): R =
    val r = ref.get(id)
    if r == null then fallback
    else r.call(args).asInstanceOf[R]
end Stubbed

object Stubbed:
  /** Insert a constant-value stub. Used by the value branch of `stub`. */
  def insertValue[Service: Tag, V](id: MethodId, response: V): URIO[Stubbed[Service], Unit] =
    ZIO.serviceWithZIO[Stubbed[Service]](_.insertValueImpl(id, response))

  /** Insert a function-adapter stub. Used by the function-handler branch of `stub`. */
  def insertFunc[Service: Tag](id: MethodId, f: Array[Any] => Any): URIO[Stubbed[Service], Unit] =
    ZIO.serviceWithZIO[Stubbed[Service]](_.insertFuncImpl(id, f))
end Stubbed
