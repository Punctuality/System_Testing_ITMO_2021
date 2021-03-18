package ru.ifmo.se.software.testing.lab1.sorting

import cats.effect.Sync
import cats.tagless.{ApplyK, Derive}

import scala.collection.IterableOps
import scala.reflect.ClassTag

trait Sorting[F[_], CC[_], A]{
  def sort(collection: CC[A]): F[CC[A]]
}

object Sorting {
  implicit def applyK[CC[_], A]: ApplyK[Sorting[*[_], CC, A]] = Derive.applyK[Sorting[*[_], CC, A]]

  def bubble[F[_]: Sync, CC[X] <: IterableOps[X, CC, CC[X]], A: OrderingF[F, *]: ClassTag](
    implicit AOs: ArrayOps[F, A]
  ): Sorting[F, CC, A] = new BubbleSorting[F, CC, A]
}