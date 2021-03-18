package ru.ifmo.se.software.testing.lab1.domain.traits.active

import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Sensible

trait SenseChanging[F[_]] {
  def makeFeel[T <: Sensible](newSense: Sensible.Sense, who: T): F[Unit]
  def omitFeel[T <: Sensible](oldSense: Sensible.Sense, who: T): F[Unit]
}
