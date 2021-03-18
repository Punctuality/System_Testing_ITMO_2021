package ru.ifmo.se.software.testing.lab1.sorting

import cats.MonadThrow
import cats.syntax.flatMap._
import cats.syntax.functor._

class ArrayOpsImpl[F[_]: MonadThrow, A] extends ArrayOps[F, A] {
  override def lookup(collection: Array[A], idx: Int): F[A] =
    MonadThrow[F].catchNonFatal(collection(idx))
  override def swap(collection: Array[A], idxA: Int, idxB: Int): F[Unit] = for {
    a <- lookup(collection, idxA)
    b <- lookup(collection, idxB)
  } yield {
    collection.update(idxB, a)
    collection.update(idxA, b)
  }
}
