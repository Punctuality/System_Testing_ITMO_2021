package ru.ifmo.se.software.testing.lab2.func

import cats.ApplicativeError
import cats.syntax.applicative._
import cats.syntax.functor._
import ru.ifmo.se.software.testing.lab1.func.Func
import ru.ifmo.se.software.testing.lab2.util.RichFractional._
import scala.math._
import scala.Fractional.Implicits._
import scala.Ordering.Implicits._
import scala.annotation.tailrec

class Sin[F[_]: ApplicativeError[*[_], Throwable], N: Fractional: Ordering](val precision: N) extends Func[F, N, N]{
  override def apply(input: N): F[N] =
    (input.toDouble % (2 * Pi))
      .pure[F]
      .map(fromDouble[N](_) -> fromDouble[N](Pi))
      .map{
        case (x, pi) if (x.abs - pi).abs < precision => Fractional[N].fromInt(0)
        case (x, pi) if (x.abs - pi * Fractional[N].fromInt(2)).abs < precision
                                                     => Fractional[N].fromInt(0)
        case (x, _)  if x.abs < precision            => Fractional[N].fromInt(0)
        case (x, _)                                  => x
      }.map(x => recSinTaylorStep(x, step = 1, x, Fractional[N].fromInt(0)))

  @tailrec
  private def recSinTaylorStep(point: N, step: Long, previous: N, result: N): N =
    if (previous.abs < precision) result
    else recSinTaylorStep(
      point    = point,
      step     = step + 1L,
      previous = previous * (-point * point / fromDouble[N](2L * step * (2L * step + 1L))),
      result   = result + previous
    )
}

object Sin{
  def apply[F[_]: ApplicativeError[*[_], Throwable], N: Fractional: Ordering](precision: N): Sin[F, N] =
    new Sin[F, N](precision)
}