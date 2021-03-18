package ru.ifmo.se.software.testing.lab1.domain.traits.active

import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Breakable

trait Breaking[F[_]] {
  def break[T <: Breakable](toBreak: T): F[Unit]
}
