package ru.ifmo.se.software.testing.lab1.domain.entities

import ru.ifmo.se.software.testing.lab1.domain.traits.passive._

abstract class Item(val name: String) extends Breakable with Movable {
  override var isBroken: Boolean = false
}
