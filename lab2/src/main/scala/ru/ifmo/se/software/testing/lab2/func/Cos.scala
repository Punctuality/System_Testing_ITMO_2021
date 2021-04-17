package ru.ifmo.se.software.testing.lab2.func

import cats.effect.MonadThrow
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import scala.math._
import scala.Fractional.Implicits._
import ru.ifmo.se.software.testing.lab1.func.Func
import ru.ifmo.se.software.testing.lab2.util.RichFractional._

import scala.math.{Fractional, Ordering}

class Cos[F[_]: MonadThrow, N: Fractional: Ordering](implicit val sin: Sin[F, N]) extends Func[F, N, N]{
  override def apply(input: N): F[N] =
    (input.toDouble % (2.0 * Pi)).pure[F]
      .map{
        case xModded if xModded < (Pi / 2) && xModded > -Pi / 2 =>  1
        case xModded if xModded < -1.5 * Pi                     =>  1
        case xModded if xModded >  1.5 * Pi                     =>  1
        case _                                                  => -1
      }
      .flatMap(
        sign => sin(input)
          .map(sr => (Fractional[N].fromInt(1) - sr * sr).toDouble)
          .map(max(0.0, _))
          .map(sqrt)
          .map(r => sign.toDouble * r)
          .map(fromDouble[N])
      )
}

object Cos {
  def apply[F[_]: MonadThrow, N: Fractional: Ordering](implicit sin: Sin[F, N]): Cos[F, N] =
    new Cos[F, N]
}