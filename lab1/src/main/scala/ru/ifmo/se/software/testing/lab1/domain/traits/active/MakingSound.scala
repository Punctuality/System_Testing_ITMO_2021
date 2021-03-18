package ru.ifmo.se.software.testing.lab1.domain.traits.active

trait MakingSound[F[_], C, S] {
  def makeSound(context: C): F[S]
}
