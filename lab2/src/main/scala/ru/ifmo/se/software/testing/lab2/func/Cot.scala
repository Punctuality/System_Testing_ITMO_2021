package ru.ifmo.se.software.testing.lab2.func

import cats.MonadThrow
import cats.syntax.flatMap._
import cats.syntax.functor._
import ru.ifmo.se.software.testing.lab1.func.Func

import scala.Fractional.Implicits._

class Cot[F[_]: MonadThrow, N: Fractional: Ordering](implicit sin: Sin[F, N]) extends Func[F, N, N] {

  private lazy val cos: Cos[F, N] = Cos.apply

  override def apply(input: N): F[N] = for {
    nominator <- cos(input)
    denominator <- sin(input)
  } yield nominator / denominator
}

object Cot {
  def apply[F[_]: MonadThrow, N: Fractional: Ordering](implicit sin: Sin[F, N]): Cot[F, N] =
    new Cot[F, N]
}