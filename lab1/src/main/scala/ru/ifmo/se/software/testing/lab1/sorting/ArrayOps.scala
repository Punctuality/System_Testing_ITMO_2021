package ru.ifmo.se.software.testing.lab1.sorting

import cats.MonadThrow
import cats.tagless.{ApplyK, Derive}

trait ArrayOps[F[_], A] {
  def lookup(collection: Array[A], idx: Int): F[A]
  def swap(collection: Array[A], idxA: Int, idxB: Int): F[Unit]
}

object ArrayOps {
  implicit def applyK[A]: ApplyK[ArrayOps[*[_], A]] = Derive.applyK[ArrayOps[*[_], A]]

  object syntax {
    implicit class ArrayWithOps[F[_], A](array: Array[A])(implicit aos: ArrayOps[F, A]) {
      def lookup(idx: Int): F[A] = aos.lookup(array, idx)
      def swap(idxA: Int, idxB: Int): F[Unit] = aos.swap(array, idxA, idxB)
    }
  }

  def default[F[_]: MonadThrow, A]: ArrayOps[F, A] = new ArrayOpsImpl[F, A]
}