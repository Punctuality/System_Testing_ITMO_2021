package ru.ifmo.se.software.testing.lab2.func

import cats.MonadThrow
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.Fractional.Implicits._
import scala.Ordering.Implicits._
import ru.ifmo.se.software.testing.lab1.func.Func
import ru.ifmo.se.software.testing.lab2.util.RichFractional._
import ru.ifmo.se.software.testing.lab2.util.RichNumeric._

import scala.math.Pi

class GeneralFunc[F[_]: MonadThrow, N: Fractional: Ordering](
  implicit sin: Sin[F, N],
  ln: Ln[F, N]
) extends Func[F, N, N] {

  // Trigonometric
  private lazy val cos: Cos[F, N] = Cos[F, N]
  private lazy val tan: Tan[F, N] = Tan[F, N]
  private lazy val cot: Cot[F, N] = Cot[F, N]
  // Logarithmic
  private lazy val log2: Log[F, N] = Log(Fractional[N].fromInt(2))
  private lazy val log3: Log[F, N] = Log(Fractional[N].fromInt(3))
  private lazy val log5: Log[F, N] = Log(Fractional[N].fromInt(5))

  /*
    x <= 0 : (((((cos(x) ^ 2) / tan(x)) + sin(x)) * (cos(x) * cos(x))) - (cot(x) + tan(x)))
    x > 0 : (((((log_3(x) - log_2(x)) ^ 2) ^ 2) ^ 3) / log_5(x))
  */

  override def apply(input: N): F[N] =
    if (input <= Fractional[N].fromInt(0)) {
      for {
        cosR <- cos(input)
        sinR <- sin(input)
        tanR <- tan(input)
        cotR <- cot(input)
      } yield {
        val res = ((((cosR ^ 2) / tanR) + sinR) * (cosR * cosR)) - (cotR + tanR)
        val iModded = fromDouble[N](input.toDouble % Pi).abs
        if ((iModded < sin.precision) || ((fromDouble[N](Pi) - iModded).abs < sin.precision))
          Fractional[N].fromInt(0)
        else res
      }
    } else {
      for {
        log2R <- log2(input)
        log3R <- log3(input)
        log5R <- log5(input)
      } yield ((((log3R - log2R) ^ 2) ^ 2) ^ 3) / log5R
    }
}

object GeneralFunc {
  def apply[F[_]: MonadThrow, N: Fractional: Ordering](implicit sin: Sin[F, N], ln: Ln[F, N]): GeneralFunc[F, N] =
    new GeneralFunc[F, N]
}