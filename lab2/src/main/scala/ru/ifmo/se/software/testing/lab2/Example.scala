package ru.ifmo.se.software.testing.lab2

import scala.math._
import cats.effect.IO
import ru.ifmo.se.software.testing.lab2.func._

object Example extends App {
  implicit val ln: Ln[IO, Double] = Ln(1e-8)
  implicit val sin: Sin[IO, Double] = Sin(1e-8)
  val f: GeneralFunc[IO, Double] = GeneralFunc[IO, Double]
  println(f(-6.3).unsafeRunSync())
}
