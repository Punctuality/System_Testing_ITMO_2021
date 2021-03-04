package ru.ifmo.se.software.testing.lab1

import cats.effect.IO
import ru.ifmo.se.software.testing.lab1.func.Func

object Main extends App {
  val a = Func.atan[IO, Float]

  println(
    a
      .apply(1.99.toFloat)
      .redeem(e => println(s"GOT ERROR: ${e.getLocalizedMessage}"), r => s"GOT result: $r")
      .unsafeRunSync()
  )
}
