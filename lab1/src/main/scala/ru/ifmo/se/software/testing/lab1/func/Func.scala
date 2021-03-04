package ru.ifmo.se.software.testing.lab1.func

import cats.ApplicativeError
import cats.effect.MonadThrow

trait Func[F[_], -A, B] {
  val precision: B

  def apply(input: A): F[B]
}

object Func {
  def atan[F[_]: ApplicativeError[*[_], Throwable], N: Fractional]: Func[F, N, N] =
    new Atan[F, N](Fractional[N].parseString("0.0001").get)
  def atan[F[_]: ApplicativeError[*[_], Throwable], N: Fractional](precision: N): Func[F, N, N] =
    new Atan[F, N](precision)
}