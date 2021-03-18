package ru.ifmo.se.software.testing.lab1.domain.traits.active

import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Repairable

trait Repairing[F[_]] {
  def repair[T <: Repairable](toRepair: T): F[Unit]
}
