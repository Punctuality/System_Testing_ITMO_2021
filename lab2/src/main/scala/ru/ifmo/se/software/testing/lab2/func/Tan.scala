package ru.ifmo.se.software.testing.lab2.func

import cats.MonadThrow
import cats.syntax.functor._
import cats.syntax.flatMap._
import scala.Fractional.Implicits._
import ru.ifmo.se.software.testing.lab1.func.Func

class Tan[F[_]: MonadThrow, N: Fractional: Ordering](implicit sin: Sin[F, N]) extends Func[F, N, N] {

  private lazy val cos: Cos[F, N] = Cos.apply

  override def apply(input: N): F[N] = for {
    nominator <- sin(input)
    denominator <- cos(input)
  } yield nominator / denominator
}

object Tan {
  def apply[F[_]: MonadThrow, N: Fractional: Ordering](implicit sin: Sin[F, N]): Tan[F, N] =
    new Tan[F, N]
}