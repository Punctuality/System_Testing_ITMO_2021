package ru.ifmo.se.software.testing.lab2.util

import cats.Show
import cats.syntax.show._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.effect.Sync

import scala.Fractional.Implicits._
import scala.Ordering.Implicits._
import ru.ifmo.se.software.testing.lab1.func.Func

import java.io.{File, PrintWriter}

object TabularFunc {
  type CSV = String

  def produceCSV[F[_]: Sync, N: Show: Fractional: Ordering, O: Show](func: Func[F, N, O], dataRange: (N, N), step: N): F[CSV] =
    dataRange -> step match {
      case ((start, stop), step) if (start < stop) && step < (stop - start) =>
        Sync[F] defer Iterator.iterate(start)(_ + step).takeWhile(_ <= stop).toList.traverse {
          x => func(x).map(y => show"$x,$y")
        }.map(
          data => "X,Function\n"+data.mkString("\n")
        )
      case ((start, stop), step) => show"error with: start=$start, stop=$stop, step=$step".pure[F]
    }

  def produceCSVFile[F[_]: Sync, N: Show: Fractional: Ordering, O: Show](path: String)(
    func: Func[F, N, O],
    dataRange: (N, N),
    step: N
  ): F[Unit] = Sync[F].bracket(
    new PrintWriter(new File(path)).pure[F]
  )(pw => produceCSV(func, dataRange, step).map(pw.print)
  )(pw => Sync[F] delay pw.close())

  implicit class TabularFuncOps[F[_]: Sync, N: Show: Fractional: Ordering, O: Show](func: Func[F, N, O]) {
    def produceCSV(dataRange: (N, N), step: N): F[CSV] = TabularFunc.produceCSV(func, dataRange, step)
    def produceCSVFile(path: String)(dataRange: (N, N), step: N): F[Unit] = TabularFunc.produceCSVFile(path)(func, dataRange, step)
  }
}
