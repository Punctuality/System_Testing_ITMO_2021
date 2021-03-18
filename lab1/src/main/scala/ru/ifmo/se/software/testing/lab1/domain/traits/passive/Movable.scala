package ru.ifmo.se.software.testing.lab1.domain.traits.passive

trait Movable {
  var position: Movable.Position
}

object Movable {
  case class Position(x: Double, y: Double, z: Double)
}
