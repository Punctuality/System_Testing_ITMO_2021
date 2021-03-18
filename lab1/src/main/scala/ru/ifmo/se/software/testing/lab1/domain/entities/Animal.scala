package ru.ifmo.se.software.testing.lab1.domain.entities

import ru.ifmo.se.software.testing.lab1.domain.traits.active._
import ru.ifmo.se.software.testing.lab1.domain.traits.passive._

abstract class Animal[F[_], C, S](override val name: String) extends Item(name)
  with Sensible
  with SenseChanging[F]
  with Breaking[F]
  with Moving[F]
  with MakingSound[F, C, S] {
  override var senses: Set[Sensible.Sense] = Set.empty
}
