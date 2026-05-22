package slayer

import zio.*

/** Public API surface for slayer.
  *
  * `stub` registers a stub for one method of a service trait. The selector lambda picks the method; the second argument
  * either typechecks as the method's return type (constant-value branch) or as a function from the method's parameter
  * types to its return type (handler branch — implemented in M3).
  *
  * `stubbed` synthesizes a layer providing a `Service & Stubbed[Service]` whose method bodies look up `Stubbed`'s
  * `ConcurrentHashMap` (M4+).
  */
inline def stub[Service](inline select: Service => Any)(inline result: Any): URIO[Stubbed[Service], Unit] =
  ${ Macros.stubImpl[Service]('select, 'result) }

/** Synthesize a `ZLayer` providing a runtime stub for `Service`.
  *
  * The macro emits a class extending both `Service` (with every declared method delegating to `Stubbed.callStubbed`)
  * and `Stubbed[Service]` (so `stub` calls can store entries in the same instance). Use it at the top of a test:
  *
  * ```
  * val layer: ULayer[Repo & Stubbed[Repo]] = stubbed[Repo]
  * ```
  *
  * Then `stub[Repo](_.find(any))(...)` inside an effect provided with this layer registers a stub that the synthesized
  * class will return when `Repo.find` is called.
  */
inline def stubbed[Service]: ULayer[Service & Stubbed[Service]] =
  ${ Macros.stubbedImpl[Service] }
