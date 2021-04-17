package ru.ifmo.se.software.testing.lab2

import scala.math._
import cats.effect.IO
import ru.ifmo.se.software.testing.lab2.util.TabularFunc._
import ru.ifmo.se.software.testing.lab2.func._

object CsvProducing extends App {

  implicit val sin: Sin[IO, Double] = Sin(1e-8) // -3*pi => 3*pi
  val cos: Cos[IO, Double] = Cos[IO, Double] //    -3*pi => 3*pi
  val tan: Tan[IO, Double] = Tan[IO, Double] //    -3*pi => 3*pi
  val cot: Cot[IO, Double] = Cot[IO, Double] //    -3*pi => 3*pi

  implicit val ln: Ln[IO, Double] = Ln(1e-8)           // 0 => 10
  val log2: Log[IO, Double] = Log[IO, Double](2) // 0 => 10
  val log3: Log[IO, Double] = Log[IO, Double](3) // 0 => 10
  val log5: Log[IO, Double] = Log[IO, Double](5) // 0 => 10

  val func: GeneralFunc[IO, Double] = GeneralFunc[IO, Double]

  sin.produceCSVFile("lab2/src/test/resources/sin_example.csv")(-3 * Pi -> 3 * Pi, 0.001).unsafeRunSync()
  cos.produceCSVFile("lab2/src/test/resources/cos_example.csv")(-3 * Pi -> 3 * Pi, 0.001).unsafeRunSync()
  tan.produceCSVFile("lab2/src/test/resources/tan_example.csv")(-3 * Pi -> 3 * Pi, 0.001).unsafeRunSync()
  cot.produceCSVFile("lab2/src/test/resources/cot_example.csv")(-3 * Pi -> 3 * Pi, 0.001).unsafeRunSync()

  ln.produceCSVFile("lab2/src/test/resources/ln_example.csv")(0.01 -> 10.0, 0.001).unsafeRunSync()
  log2.produceCSVFile("lab2/src/test/resources/log2_example.csv")(0.01 -> 10.0, 0.001).unsafeRunSync()
  log3.produceCSVFile("lab2/src/test/resources/log3_example.csv")(0.01 -> 10.0, 0.001).unsafeRunSync()
  log5.produceCSVFile("lab2/src/test/resources/log5_example.csv")(0.01 -> 10.0, 0.001).unsafeRunSync()

  func.produceCSVFile("lab2/src/test/resources/func_example.csv")(-3 * Pi -> 10.0, 0.001).unsafeRunSync()
}
