package ru.ifmo.se.software.testing.lab2.func

import cats.ApplicativeError
import cats.syntax.applicative._
import cats.syntax.functor._
import ru.ifmo.se.software.testing.lab1.func.Func

import scala.math._
import scala.Fractional.Implicits._
import scala.Ordering.Implicits._
import scala.annotation.tailrec

class Ln[F[_]: ApplicativeError[*[_], Throwable], N: Fractional: Ordering](val precision: N) extends Func[F, N, N]{
  override def apply(input: N): F[N] = if (input <= Fractional[N].fromInt(0)) {
    ApplicativeError[F, Throwable] raiseError[N] new IllegalArgumentException(s"Argument should be greater than 0.0, but got: $input")
  } else {
    Fractional[N].fromInt(1).pure[F]
      .map(one => (input - one) / (input + one))
      .map(x => recTaylorLnStep(x, 3, x, Fractional[N].fromInt(0)))
  }

  @tailrec
  private def recTaylorLnStep(point: N, step: Int, previous: N, result: N): N =
    if ((previous * Fractional[N].fromInt(2)).abs <= (precision / Fractional[N].fromInt(10))) {
      result
    } else {
      recTaylorLnStep(
        point = point,
        step = step + 2,
        previous = previous * (point * point / Fractional[N].fromInt(step) * Fractional[N].fromInt(step - 2)),
        result = result + previous * Fractional[N].fromInt(2)
      )
    }
}

object Ln{
  def apply[F[_]: ApplicativeError[*[_], Throwable], N: Fractional: Ordering](precision: N): Ln[F, N] =
    new Ln[F, N](precision)
}

