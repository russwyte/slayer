package slayer

sealed abstract class SlayerError(msg: String) extends RuntimeException(msg)
object SlayerError:
  /** Thrown synchronously when an abstract (or otherwise non-fall-through) method is invoked with no registered stub.
    */
  final case class NoStub(id: MethodId) extends SlayerError(s"No stub registered for $id")

/** Typed placeholder used inside `stub` selector lambdas.
  *
  * `any[A]` is **not** an argument matcher (there is no `eq` / `argThat` / partial matching). It is never evaluated at
  * runtime; the macro extracts the method symbol from the lambda's tree without inspecting argument values. To branch
  * on call-site arguments, use a function handler and match inside it.
  */
def any[A]: A = null.asInstanceOf[A]
