package ru.ifmo.se.software.testing.lab1.domain.traits.active

import cats.Functor
import cats.syntax.functor._
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Breakable

trait Breaking[F[_]] {
  def break[T <: Breakable](toBreak: T): F[Unit]
  def break[T <: Breakable](toBreak: T, affect: () => F[Unit])(implicit functor: Functor[F]): F[Unit] =
    break(toBreak).map(_ => affect())
}
