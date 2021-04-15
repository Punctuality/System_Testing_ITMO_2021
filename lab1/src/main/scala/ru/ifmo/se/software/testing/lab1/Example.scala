package ru.ifmo.se.software.testing.lab1

import cats.effect.IO
import ru.ifmo.se.software.testing.lab1.func.Func
import ru.ifmo.se.software.testing.lab1.sorting.{ArrayOps, Sorting}

import scala.util.Random

object Example extends App {
  val atan = Func.atan[IO, Float](1e-4F)

  println(atan(0.5.toFloat).unsafeRunSync())
  println(
    atan(1.5.toFloat)
      .redeem(e => s"GOT ERROR: ${e.getLocalizedMessage}", r => s"GOT result: $r")
      .unsafeRunSync()
  )

  implicit val aos: ArrayOps[IO, Int] = ArrayOps.default
  val sorting: Sorting[IO, List, Int] = Sorting.bubble[IO, List, Int]
  val list: List[Int] = Random.shuffle((0 until 500).toList)

  println(list)
  println(sorting.sort(list).unsafeRunSync())
}
