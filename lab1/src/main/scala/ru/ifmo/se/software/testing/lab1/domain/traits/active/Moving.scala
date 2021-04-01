package ru.ifmo.se.software.testing.lab1.domain.traits.active

import cats.Functor
import cats.syntax.functor._
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Movable

trait Moving[F[_]] {
  def move[T <: Movable](newPosition: Movable.Position, toMove: T): F[Unit]
  def move[T <: Movable](newPosition: Movable.Position, toMove: T, affect: () => F[Unit]
    )(implicit functor: Functor[F]): F[Unit] = move(newPosition, toMove).map(_ => affect())
}
