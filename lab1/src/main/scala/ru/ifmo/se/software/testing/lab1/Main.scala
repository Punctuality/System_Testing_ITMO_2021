package ru.ifmo.se.software.testing.lab1

import cats.effect.IO
import ru.ifmo.se.software.testing.lab1.func.Func
import ru.ifmo.se.software.testing.lab1.sorting.Sorting

import scala.util.Random

object Main extends App {
//  val a = Func.atan[IO, Float]
//
//  println(
//    a
//      .apply(0.5.toFloat)
//      .redeem(e => println(s"GOT ERROR: ${e.getLocalizedMessage}"), r => s"GOT result: $r")
//      .unsafeRunSync()
//  )

  val sorting: Sorting[IO, List, Int] = Sorting.bubble[IO, List, Int]
  val list: List[Int] = Random.shuffle((0 until 500).toList)

  println(
    list
  )
  println(
    sorting.sort(list).unsafeRunSync()
  )

  println(
    sorting.lookup(list.toArray, 0).unsafeRunSync()
  )

  println(
    sorting.lookup(list.toArray, 500)
      .redeem(e => s"GOT ERROR: ${e.getLocalizedMessage}", r => s"GOT result: $r")
      .unsafeRunSync()
  )
}
