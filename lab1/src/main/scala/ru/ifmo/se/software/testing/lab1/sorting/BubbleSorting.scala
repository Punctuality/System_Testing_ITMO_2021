package ru.ifmo.se.software.testing.lab1.sorting

import cats.Applicative
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import tofu.syntax.monadic.TofuApplyOps
import ru.ifmo.se.software.testing.lab1.sorting.OrderingF._

import scala.collection.IterableOps
import scala.reflect.ClassTag

class BubbleSorting[F[_]: Sync, CC[X] <: IterableOps[X, CC, CC[X]], A: OrderingF[F, *]: ClassTag]
  extends Sorting[F, CC, A] {
  override def sort(collection: CC[A]): F[CC[A]] = Sync[F].defer {
    val collArray: Array[A] = collection.toArray
    val arraySize: Int      = collArray.length

    def helper(bound: Int): F[Unit] =
      (0 until arraySize - bound - 1).toList.traverse{ iter =>
        for {
          isSwapped <- Ref.of[F, Boolean](false)
          left <- lookup(collArray, iter)
          right <- lookup(collArray, iter + 1)
          isBigger <- left > right
          _ <-
            if (isBigger) swap(collArray, iter, iter + 1) *> isSwapped.set(true)
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

  override def lookup(collection: Array[A], idx: Int): F[A] =
    Sync[F].catchNonFatal(collection(idx))
  override def swap(collection: Array[A], idxA: Int, idxB: Int): F[Unit] = for {
    a <- lookup(collection, idxA)
    b <- lookup(collection, idxB)
  } yield {
    collection.update(idxB, a)
    collection.update(idxA, b)
  }
}