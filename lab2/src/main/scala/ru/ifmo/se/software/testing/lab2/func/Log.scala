package ru.ifmo.se.software.testing.lab2.func

import cats.effect.MonadThrow
import cats.syntax.functor._
import cats.syntax.flatMap._
import scala.Fractional.Implicits._
import ru.ifmo.se.software.testing.lab1.func.Func

class Log[F[_]: MonadThrow, N: Fractional: Ordering](val base: N)(implicit val ln: Ln[F, N]) extends Func[F, N, N] {

  override def apply(input: N): F[N] = for {
    natural <- ln(input)
    basis   <- ln(base)
  } yield natural / basis
}

object Log{
  def apply[F[_]: MonadThrow, N: Fractional: Ordering](base: N)(implicit ln: Ln[F, N]): Log[F, N] =
    new Log[F, N](base)
}