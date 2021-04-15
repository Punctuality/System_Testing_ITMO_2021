package ru.ifmo.se.software.testing.lab1.func

import cats.{Applicative, ApplicativeError}

import scala.math.pow
import scala.Fractional.Implicits._
import scala.Ordering.Implicits._

class Atan[F[_]: ApplicativeError[*[_], Throwable], N: Fractional: Ordering](val precision: N) extends Func[F, N, N] {

  // Converges at [-1, 1]
  override def apply(input: N): F[N] = if (Fractional[N].fromInt(-1) <= input && input <= Fractional[N].fromInt(1)){
    Applicative[F].pure(Iterator.iterate(0)(_ + 1).map(atanTaylorStep(input, _)).takeWhile(_.abs >= precision).sum)
  } else {
    ApplicativeError[F, Throwable].raiseError(
      new IllegalArgumentException(s"Argument should be between -1 and 1, but got: $input")
    )
  }

  // (-1) ^ n * (x ^ (2*n + 1)) / (2*n + 1)
  private def atanTaylorStep(point: N, step: Int): N =
    Fractional[N].fromInt(if (step % 2 == 0) 1 else -1) *
    Fractional[N].parseString(pow(point.toDouble, 2 * step + 1).toString).get /
    Fractional[N].fromInt(2 * step + 1)
}
