package ru.ifmo.se.software.testing.lab1.domain.traits.active

import cats.Functor
import cats.syntax.functor._
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Repairable

trait Repairing[F[_]] {
  def repair[T <: Repairable](toRepair: T): F[Unit]
  def repair[T <: Repairable](toRepair: T, affect: () => F[Unit])(implicit functor: Functor[F]): F[Unit] =
    repair(toRepair).map(_ => affect())
}
