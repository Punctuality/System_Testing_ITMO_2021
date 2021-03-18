package ru.ifmo.se.software.testing.lab1.domain.traits.passive

import scala.math._

trait Movable {
  var position: Movable.Position
}

object Movable {
  case class Position(x: Double, y: Double, z: Double) {
    def distanceTo(that: Position): Double = that match {
      case Position(thatX, thatY, thatZ) =>
        sqrt(List(thatX - x, thatY - y, thatZ - z).map(pow(_, 2)).sum)
    }
  }
}
