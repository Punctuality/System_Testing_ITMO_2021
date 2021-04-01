package ru.ifmo.se.software.testing.lab1.domain.entities

import ru.ifmo.se.software.testing.lab1.domain.traits.active.MakingSound
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.{Movable, Repairable}

case class Furniture[F[_]](
               override val name: String,
               override var position: Movable.Position
               ) extends Item(name) with Repairable with MakingSound[F, (Boolean, String), Furniture.Noise] {
  override var isRepaired: Boolean = false

  override def makeSound(context: (Boolean, String)): F[Furniture.Noise] = ???
}

object Furniture {
  case class Noise(isLoud: Boolean, description: String)
}