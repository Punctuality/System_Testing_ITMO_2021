package ru.ifmo.se.software.testing.lab1.sorting

import cats.Applicative
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import tofu.syntax.monadic.TofuApplyOps
import ru.ifmo.se.software.testing.lab1.sorting.OrderingF._
import ru.ifmo.se.software.testing.lab1.sorting.ArrayOps.syntax._

import scala.collection.IterableOps
import scala.reflect.ClassTag

class BubbleSorting[F[_]: Sync, CC[X] <: IterableOps[X, CC, CC[X]], A: OrderingF[F, *]: ClassTag](
    implicit AOs: ArrayOps[F, A]
  ) extends Sorting[F, CC, A] {
  override def sort(collection: CC[A]): F[CC[A]] = Sync[F].defer {
    val collArray: Array[A] = collection.toArray
    val arraySize: Int      = collArray.length

    def helper(bound: Int): F[Unit] =
      (0 until arraySize - bound - 1).toList.traverse{ iter =>
        for {
          isSwapped <- Ref.of[F, Boolean](false)
          left <- collArray.lookup(iter)
          right <- collArray.lookup(iter + 1)
          isBigger <- left > right
          _ <-
            if (isBigger) collArray.swap(iter, iter + 1) *> isSwapped.set(true)
            else Applicative[F].unit
          result <- isSwapped.get
        } yield result
      }.map(_.contains(true)).flatMap{
        case true if bound < arraySize - 1 => helper(bound + 1)
        case _                             => Applicative[F].unit
      }

    helper(0).map(_ =>
      collection.iterableFactory.from(collArray)
    )
  }
}