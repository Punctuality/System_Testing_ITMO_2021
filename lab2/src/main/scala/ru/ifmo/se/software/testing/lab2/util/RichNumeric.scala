package ru.ifmo.se.software.testing.lab2.util

import scala.Fractional.Implicits._

object RichNumeric {
  def fromLong[N: Fractional](long: Long): N =
    Numeric[N].parseString(long.toString).get

  def powNum[N: Fractional](number: N, power: Int): N =
    if (power <= 1) number else number * powNum(number, power - 1)

  implicit class RichNumericOps[N: Fractional](number: N) {
    def ^(power: Int): N = powNum(number, power)
  }
}
