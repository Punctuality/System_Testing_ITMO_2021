package ru.ifmo.se.software.testing.lab1.sorting

import cats.data.Tuple2K
import cats.{Applicative, Functor, ~>}
import cats.syntax.functor._
import cats.tagless.ApplyK


trait OrderingF[F[_], T] {
  def compareF(a: T, b: T): F[Int]

  def lt(a: T, b: T)(implicit F: Functor[F]):   F[Boolean] = compareF(a, b).map(_ <  0)
  def lteq(a: T, b: T)(implicit F: Functor[F]): F[Boolean] = compareF(a, b).map(_ <= 0)
  def gt(a: T, b: T)(implicit F: Functor[F]):   F[Boolean] = compareF(a, b).map(_ >  0)
  def gteq(a: T, b: T)(implicit F: Functor[F]): F[Boolean] = compareF(a, b).map(_ >= 0)
}

object OrderingF {
    implicit def applyK[T]: ApplyK[OrderingF[*[_], T]] = new ApplyK[OrderingF[*[_], T]] {
      override def mapK[F[_], G[_]](af: OrderingF[F, T])(fk: F ~> G): OrderingF[G, T] =
        (a: T, b: T) => fk(af.compareF(a, b))
      override def productK[F[_], G[_]](af: OrderingF[F, T], ag: OrderingF[G, T]): OrderingF[Tuple2K[F, G, *], T] =
        (a: T, b: T) => Tuple2K(af.compareF(a, b), ag.compareF(a, b))
    }

    implicit def orderingLift[F[_]: Applicative, T: Ordering]: OrderingF[F, T] =
      (a: T, b: T) => Applicative[F].pure(Ordering[T].compare(a, b))

    implicit class OrderingFOps[F[_]: Functor, T](lhs: T) {
      def < (rhs: T)(implicit O: OrderingF[F, T]): F[Boolean] = O.lt(lhs, rhs)
      def <=(rhs: T)(implicit O: OrderingF[F, T]): F[Boolean] = O.lteq(lhs, rhs)
      def > (rhs: T)(implicit O: OrderingF[F, T]): F[Boolean] = O.gt(lhs, rhs)
      def >=(rhs: T)(implicit O: OrderingF[F, T]): F[Boolean] = O.gteq(lhs, rhs)
    }
}
