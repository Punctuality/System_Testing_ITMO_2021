package ru.ifmo.se.software.testing.lab1.domain.traits.active

import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Movable

trait Moving[F[_]] {
  def move[T <: Movable](toMove: T): F[Unit]
}
