package slayer

sealed abstract class SlayerError(msg: String) extends RuntimeException(msg)
object SlayerError:
  final case class NoStub(id: MethodId) extends SlayerError(s"No stub registered for $id")
  final case class WrongArity(id: MethodId, expected: Int, got: Int)
      extends SlayerError(s"Wrong arity for $id: expected $expected but got $got")

/** Argument matcher placeholder.
  *
  * `any[A]` is only a typed placeholder used inside `stub` selector lambdas. It is never evaluated at runtime; the
  * macro extracts the method symbol from the lambda's tree without inspecting argument values.
  */
def any[A]: A = null.asInstanceOf[A]
