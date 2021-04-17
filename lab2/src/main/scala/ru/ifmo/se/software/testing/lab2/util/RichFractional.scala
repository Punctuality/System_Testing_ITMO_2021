package ru.ifmo.se.software.testing.lab2.util

object RichFractional {
  def fromDouble[N: Fractional](double: Double): N =
    Fractional[N].parseString(double.toString).get
}
