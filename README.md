# slayer

N-arity, multi-param-list, generics-aware ZIO service stubbing for Scala 3.

`slayer` is a small library for stubbing service traits in ZIO tests. Given any service trait, you can synthesize a
`ZLayer` that satisfies it and configure per-method behavior from the test body. It is inspired by Kit Langton's
[stubby](https://github.com/kitlangton/stubby) and addresses four of its limitations:

1. **No arity ceiling** — stubby hardcoded `F1..F9`. slayer unrolls the user's handler into an `Array[Any] => Any`
   adapter, so any number of parameters works.
2. **Multi-parameter-list (curried) methods** are first-class for both selectors and handlers.
3. **Generic / type-parameterized methods** (`def get[A: Tag](id: Id): UIO[A]`) are supported. Dispatch is by name and
   erased value-parameter signature.
4. **Overloaded methods** — both plain Scala overloads (different param types, same source name) and
   `@targetName`-renamed methods are routed to distinct stubs.

## Install

```scala
libraryDependencies += "io.github.russwyte" %% "slayer" % "<version>" % Test
```

ZIO is declared `% "provided"` — bring your own (≥ 2.1.x). slayer requires Scala 3.8.x or later.

## Usage

```scala
import slayer.*
import zio.*
import zio.test.*

trait Repo:
  def find(id: Int): UIO[Option[String]]
  def upsert(id: Int, value: String): UIO[Unit]
  def get[A: Tag](id: Int): UIO[A]

object MySpec extends ZIOSpecDefault:
  def spec = suite("repo")(
    test("find returns the stubbed value") {
      val program =
        for
          _   <- stub[Repo](_.find(any[Int]))(ZIO.succeed(Some("hello")))
          out <- ZIO.serviceWithZIO[Repo](_.find(7))
        yield assertTrue(out == Some("hello"))
      program.provide(stubbed[Repo])
    },
    test("upsert handler can read its arguments") {
      val program =
        for
          ref <- Ref.make(Map.empty[Int, String])
          _   <- stub[Repo](_.upsert(any[Int], any[String])) { (id: Int, v: String) =>
                   ref.update(_ + (id -> v))
                 }
          _   <- ZIO.serviceWithZIO[Repo](_.upsert(1, "a"))
          _   <- ZIO.serviceWithZIO[Repo](_.upsert(2, "b"))
          got <- ref.get
        yield assertTrue(got == Map(1 -> "a", 2 -> "b"))
      program.provide(stubbed[Repo])
    },
  )
```

### Curried (multi-parameter-list) methods

slayer reads the *handler*'s function shape, not the method's. A curried 3-list method can be stubbed with either a
curried handler or a tupled one:

```scala
trait AuditLog:
  def record(orderId: OrderId)(actor: String)(event: String): UIO[Unit]

// Curried handler — mirrors the method shape:
stub[AuditLog](_.record(any[OrderId])(any[String])(any[String])) {
  (id: OrderId) => (actor: String) => (event: String) =>
    log.update((id, actor, event) :: _)
}

// Tupled handler — flat arg list, same method:
stub[AuditLog](_.record(any[OrderId])(any[String])(any[String])) {
  (id: OrderId, actor: String, event: String) => ZIO.unit
}
```

Both work end-to-end. See [`ExampleSpec`](core/src/test/scala/slayer/ExampleSpec.scala) for a full set of patterns
including failing effects, captured calls, branching handlers, and re-stubbing mid-test.

### Concrete (default) trait methods

If your trait already provides an implementation for a method, slayer falls through to that implementation by default
— you don't need to stub it. You can still stub it if you want to override the trait's logic in a test:

```scala
trait Greeter:
  def name: String                         // abstract — must be stubbed
  def hello: String = s"hello, $name"      // concrete — has a default impl

// Fall-through: trait's `hello` runs, reading the stubbed `name`.
stub[Greeter](_.name)("alice")
ZIO.serviceWith[Greeter](_.hello)          // → "hello, alice"

// Override: stubbing the concrete method wins over the trait's impl.
stub[Greeter](_.hello)("howdy from stub")
ZIO.serviceWith[Greeter](_.hello)          // → "howdy from stub"
```

Under the hood, every trait method gets an override on the synthesized class. Abstract methods route to `callStubbed`
(throws `NoStub` when missing). Concrete methods route to `callStubbedOrElse(...)(super.method(args))`, so a missing
stub falls through to the trait's original impl.

### Default arguments

Default arguments work transparently — at both the stub site and the call site, you can omit any args that have
defaults, and the Scala typer fills them in for you. The defaults are evaluated at the call site and flow into the
handler's arg list, so a handler can read them just like any other argument:

```scala
trait Multi:
  def g(a: Int, b: Int = 20, c: Int = 30, d: Int = 40): String

// Stub omits all defaults; handler sees the full filled-in arg list.
stub[Multi](_.g(any[Int])) { (a: Int, b: Int, c: Int, d: Int) =>
  s"$a/$b/$c/$d"
}
ZIO.serviceWith[Multi](_.g(1))         // → "1/20/30/40"
ZIO.serviceWith[Multi](_.g(1, 99))     // → "1/99/30/40" (b overridden, c/d defaulted)
```

Subsets work too: stub `_.g(any[Int], any[Int])` and the rest of the defaults still flow through. Named args that
skip leading defaults (e.g. `_.h(c = any[Int])` against `def h(a: Int = 1, b: Int = 2, c: Int)`) also work.

See [`DefaultArgSpec`](core/src/test/scala/slayer/DefaultArgSpec.scala) for the full coverage matrix including curried
methods with defaults across multiple parameter lists.

`stub[S](selector)(result)` — selector is a lambda picking a method; `result` is either a value of the method's return
type, or a function whose flat parameters match the method's flat value parameters.

`stubbed[S]` — a `ZLayer` that synthesizes a class implementing `S` (and exposes its `Stubbed[S]` store). Provide it
once per service per test.

`any[A]` — a placeholder used inside selectors. Never evaluated; only its type matters.

## How it works

`stub` is a macro that:
- peels the selector lambda to find the resolved method symbol (overloads have already been chosen by the typer);
- derives a `MethodId(jvmName, erasedParams)` from that symbol — `jvmName` honors `@targetName`, `erasedParams` keys
  plain overloads;
- type-checks the result against the method's return; if it's a function, walks the handler's `Function*` chain and
  emits an `Array[Any] => Any` adapter that casts each slot to the right type.

`stubbed[S]` is a macro that synthesizes a class extending `S & Stubbed[S]`. Each method packs its value arguments
(dropping `using` clauses) into an `Array[Any]` and calls `callStubbed` with the same `MethodId` the stub side
produces. Lookup is a single concurrent-map read.

## Limitations

**Slayer-level:**

- **Generic dispatch is by name + erased value-param types.** Different type-argument instantiations of the same
  method share a stub. For type-discriminated behavior, use a handler that branches on its arguments.
- **`final` classes** can't be used as the service type — slayer needs to synthesize a subclass.
- **`final` methods** on a class can't be stubbed (or even left to fall through) — slayer overrides every trait/class
  method, and `final` blocks the override. Either remove `final` or make the type a trait.

**Scala-language (surfaces before slayer runs, not slayer-specific):**

- **`@targetName`-disambiguated siblings** — two abstract methods with identical source signatures distinguished only
  by `@targetName`: `_.a(x)` is ambiguous, and `_.a1(x)` / `_.a2(x)` aren't callable members (`@targetName` is purely
  a JVM-level rename, not a Scala alias). The trait shape compiles, but no caller can dispatch deterministically.
- **Erasure-collision overloads** — e.g. `def f(xs: List[Int])` vs `def f(xs: List[String])`: Scala rejects the trait
  declaration itself with "have the same erasure". Add `@targetName` on one to disambiguate.

## Status

Pre-release. APIs may change. v0.1.0 will be the first published artifact.

## License

Apache-2.0.
