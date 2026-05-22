package slayer

import zio.*
import zio.test.*

/** Showcase spec — demonstrates what slayer is for: writing focused tests against ZIO services without writing a fake
  * implementation by hand. Each test highlights a capability you'd otherwise miss with a hand-rolled stub.
  *
  * The "system under test" is a tiny `OrderService` that depends on three collaborators (`Inventory`, `Pricing`,
  * `Notifier`). The tests stub each collaborator independently — successes, failures, captured calls, multi-step
  * scenarios — and assert on `OrderService`'s behavior.
  */
object ExampleSpec extends ZIOSpecDefault:

  // ----- Domain ------------------------------------------------------------------------------------------------------

  final case class OrderId(value: Long)
  final case class Sku(value: String)
  final case class Money(cents: Long)

  enum InventoryError extends Throwable:
    case OutOfStock(sku: Sku)
    case Unknown(sku: Sku)

  enum PricingError extends Throwable:
    case NoPrice(sku: Sku)

  trait Inventory:
    def reserve(sku: Sku, qty: Int): IO[InventoryError, Unit]
    def release(sku: Sku, qty: Int): UIO[Unit]

  trait Pricing:
    def quote(sku: Sku): IO[PricingError, Money]

  trait Notifier:
    def notify(orderId: OrderId, message: String): UIO[Unit]

  /** A multi-parameter-list (curried) collaborator — the kind of API where you pass context first and data second.
    * stubby couldn't handle these for handler functions; slayer reads the *handler*'s function shape, so either tupled
    * or curried handlers are accepted.
    */
  trait AuditLog:
    def record(orderId: OrderId)(actor: String)(event: String): UIO[Unit]

  // ----- System under test -------------------------------------------------------------------------------------------

  final class OrderService(inv: Inventory, pricing: Pricing, notifier: Notifier):
    def place(orderId: OrderId, sku: Sku, qty: Int): IO[InventoryError | PricingError, Money] =
      for
        _    <- inv.reserve(sku, qty)
        unit <- pricing.quote(sku).onError(_ => inv.release(sku, qty))
        total = Money(unit.cents * qty)
        _ <- notifier.notify(orderId, s"placed ${sku.value} x$qty for ${total.cents}c")
      yield total

  // ----- Tests -------------------------------------------------------------------------------------------------------

  def spec = suite("ExampleSpec — slayer in action")(
    test("happy path — stub each collaborator with a successful effect") {
      val program =
        for
          _     <- stub[Inventory](_.reserve(slayer.any[Sku], slayer.any[Int]))(ZIO.unit)
          _     <- stub[Pricing](_.quote(slayer.any[Sku]))(ZIO.succeed(Money(250)))
          _     <- stub[Notifier](_.notify(slayer.any[OrderId], slayer.any[String]))(ZIO.unit)
          svc   <- ZIO.service[OrderService]
          total <- svc.place(OrderId(1), Sku("WIDGET"), 3)
        yield total
      for total <- program.provide(
          stubbed[Inventory],
          stubbed[Pricing],
          stubbed[Notifier],
          ZLayer.fromFunction(OrderService(_, _, _)),
        )
      yield assertTrue(total == Money(750))
    },

    test("failing effect — provide a Pricing failure and observe the order failing") {
      val program =
        for
          _      <- stub[Inventory](_.reserve(slayer.any[Sku], slayer.any[Int]))(ZIO.unit)
          _      <- stub[Inventory](_.release(slayer.any[Sku], slayer.any[Int]))(ZIO.unit)
          _      <- stub[Pricing](_.quote(slayer.any[Sku]))(ZIO.fail(PricingError.NoPrice(Sku("MISSING"))))
          _      <- stub[Notifier](_.notify(slayer.any[OrderId], slayer.any[String]))(ZIO.unit)
          svc    <- ZIO.service[OrderService]
          either <- svc.place(OrderId(2), Sku("MISSING"), 1).either
        yield either
      for either <- program.provide(
          stubbed[Inventory],
          stubbed[Pricing],
          stubbed[Notifier],
          ZLayer.fromFunction(OrderService(_, _, _)),
        )
      yield assertTrue(either == Left(PricingError.NoPrice(Sku("MISSING"))))
    },

    test("handlers can capture calls — verify Notifier was invoked with the expected message") {
      val program =
        for
          calls <- Ref.make(List.empty[(OrderId, String)])
          _     <- stub[Inventory](_.reserve(slayer.any[Sku], slayer.any[Int]))(ZIO.unit)
          _     <- stub[Pricing](_.quote(slayer.any[Sku]))(ZIO.succeed(Money(100)))
          _     <- stub[Notifier](_.notify(slayer.any[OrderId], slayer.any[String])) { (id: OrderId, msg: String) =>
            calls.update((id, msg) :: _)
          }
          svc  <- ZIO.service[OrderService]
          _    <- svc.place(OrderId(42), Sku("BOLT"), 5)
          seen <- calls.get
        yield seen
      for seen <- program.provide(
          stubbed[Inventory],
          stubbed[Pricing],
          stubbed[Notifier],
          ZLayer.fromFunction(OrderService(_, _, _)),
        )
      yield assertTrue(seen == List((OrderId(42), "placed BOLT x5 for 500c")))
    },

    test("handler branches on input — different SKUs return different prices") {
      val program =
        for
          _ <- stub[Inventory](_.reserve(slayer.any[Sku], slayer.any[Int]))(ZIO.unit)
          _ <- stub[Pricing](_.quote(slayer.any[Sku])) { (sku: Sku) =>
            sku.value match
              case "A" => ZIO.succeed(Money(100))
              case "B" => ZIO.succeed(Money(250))
              case _   => ZIO.fail(PricingError.NoPrice(sku))
          }
          _   <- stub[Notifier](_.notify(slayer.any[OrderId], slayer.any[String]))(ZIO.unit)
          svc <- ZIO.service[OrderService]
          a   <- svc.place(OrderId(1), Sku("A"), 2)
          b   <- svc.place(OrderId(2), Sku("B"), 1)
        yield (a, b)
      for tup <- program.provide(
          stubbed[Inventory],
          stubbed[Pricing],
          stubbed[Notifier],
          ZLayer.fromFunction(OrderService(_, _, _)),
        )
      yield assertTrue(tup == (Money(200), Money(250)))
    },

    test("rolling failures — Inventory fails first, Pricing never gets called") {
      val program =
        for
          callsToPricing <- Ref.make(0)
          _              <- stub[Inventory](_.reserve(slayer.any[Sku], slayer.any[Int]))(
            ZIO.fail(InventoryError.OutOfStock(Sku("WIDGET")))
          )
          _ <- stub[Pricing](_.quote(slayer.any[Sku])) { (_: Sku) =>
            callsToPricing.update(_ + 1) *> ZIO.succeed(Money(0))
          }
          _           <- stub[Notifier](_.notify(slayer.any[OrderId], slayer.any[String]))(ZIO.unit)
          svc         <- ZIO.service[OrderService]
          either      <- svc.place(OrderId(7), Sku("WIDGET"), 1).either
          pricingHits <- callsToPricing.get
        yield (either, pricingHits)
      for (either, pricingHits) <- program.provide(
          stubbed[Inventory],
          stubbed[Pricing],
          stubbed[Notifier],
          ZLayer.fromFunction(OrderService(_, _, _)),
        )
      yield assertTrue(
        either == Left(InventoryError.OutOfStock(Sku("WIDGET"))),
        pricingHits == 0, // Pricing was never reached
      )
    },

    test("re-stubbing mid-test — second stub replaces the first") {
      val program =
        for
          _     <- stub[Inventory](_.reserve(slayer.any[Sku], slayer.any[Int]))(ZIO.unit)
          _     <- stub[Pricing](_.quote(slayer.any[Sku]))(ZIO.succeed(Money(100)))
          _     <- stub[Notifier](_.notify(slayer.any[OrderId], slayer.any[String]))(ZIO.unit)
          svc   <- ZIO.service[OrderService]
          first <- svc.place(OrderId(1), Sku("X"), 1)
          // Now simulate a price change for the next call.
          _      <- stub[Pricing](_.quote(slayer.any[Sku]))(ZIO.succeed(Money(999)))
          second <- svc.place(OrderId(2), Sku("X"), 1)
        yield (first, second)
      for tup <- program.provide(
          stubbed[Inventory],
          stubbed[Pricing],
          stubbed[Notifier],
          ZLayer.fromFunction(OrderService(_, _, _)),
        )
      yield assertTrue(tup == (Money(100), Money(999)))
    },

    test("curried collaborator — stub a 3-list method with a curried handler that captures every list") {
      val program =
        for
          calls <- Ref.make(List.empty[(OrderId, String, String)])
          // The selector mirrors the curried call shape; the handler is curried as well, but slayer would also
          // accept a tupled `(id, actor, event) => ...` — the unrolling reads the *handler*'s function chain.
          _ <- stub[AuditLog](_.record(slayer.any[OrderId])(slayer.any[String])(slayer.any[String])) {
            (id: OrderId) => (actor: String) => (event: String) =>
              calls.update((id, actor, event) :: _)
          }
          _    <- ZIO.serviceWithZIO[AuditLog](_.record(OrderId(1))("alice")("placed"))
          _    <- ZIO.serviceWithZIO[AuditLog](_.record(OrderId(2))("bob")("cancelled"))
          seen <- calls.get
        yield seen.reverse
      for seen <- program.provide(stubbed[AuditLog])
      yield assertTrue(
        seen == List(
          (OrderId(1), "alice", "placed"),
          (OrderId(2), "bob", "cancelled"),
        )
      )
    },

    test("curried collaborator — tupled handler against a 3-list method (handler shape is independent)") {
      val program =
        for
          _ <- stub[AuditLog](_.record(slayer.any[OrderId])(slayer.any[String])(slayer.any[String])) {
            // Tupled handler against curried method: slayer flattens both sides to the same arg array.
            (id: OrderId, actor: String, event: String) =>
              ZIO.unit.unit *> ZIO.succeed(s"$id-$actor-$event").unit
          }
          _ <- ZIO.serviceWithZIO[AuditLog](_.record(OrderId(99))("carol")("shipped"))
        yield ()
      for _ <- program.provide(stubbed[AuditLog])
      yield assertTrue(true) // success = no NoStub, no ClassCastException
    },

    test("forgotten stub — calling an un-stubbed method dies with NoStub") {
      val program =
        for
          _ <- stub[Inventory](_.reserve(slayer.any[Sku], slayer.any[Int]))(ZIO.unit)
          // Pricing is intentionally NOT stubbed.
          _    <- stub[Notifier](_.notify(slayer.any[OrderId], slayer.any[String]))(ZIO.unit)
          svc  <- ZIO.service[OrderService]
          exit <- svc.place(OrderId(99), Sku("Z"), 1).exit
        yield exit
      for exit <- program.provide(
          stubbed[Inventory],
          stubbed[Pricing],
          stubbed[Notifier],
          ZLayer.fromFunction(OrderService(_, _, _)),
        )
      yield assertTrue(
        exit.causeOption.exists(_.dieOption.exists(_.isInstanceOf[SlayerError.NoStub]))
      )
    },
  )
end ExampleSpec
