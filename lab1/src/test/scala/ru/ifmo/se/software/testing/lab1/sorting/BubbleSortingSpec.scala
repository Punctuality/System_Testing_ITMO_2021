package ru.ifmo.se.software.testing.lab1.sorting

import cats.{Applicative, Monad}
import cats.effect.IO
import org.junit.Assert.assertEquals
import org.junit.Test
import tofu.higherKind.Mid
import tofu.syntax.monadic._

import java.util.concurrent.atomic.AtomicLong
import scala.reflect.ClassTag
import scala.util.Random

class BubbleSortingSpec {

  trait TestEnv {
    def ioBubbleSorting[A: OrderingF[IO, *]: ClassTag]: Sorting[IO, List, A] = {
      implicit val aos: ArrayOps[IO, A] = ArrayOps.default
      Sorting.bubble
    }

    case class Metrics(lookups: AtomicLong, swaps: AtomicLong, comparisons: AtomicLong)

    def ioBubbleSortingWithMetrics[A: Ordering: ClassTag]: (Sorting[IO, List, A], Metrics) = {
      type ArrayOpsA[F[_]] = ArrayOps[F, A]
      type OrderingA[F[_]] = OrderingF[F, A]
      val arrMetrics = new ArrayOpsMetrics[IO, A]
      val ordMetrics = new OrderingMetrics[IO, A]
      implicit val aos: ArrayOpsA[IO] =
        (arrMetrics: ArrayOpsA[Mid[IO, *]]) attach ArrayOps.default[IO, A]
      implicit val ord: OrderingA[IO] =
        (ordMetrics: OrderingA[Mid[IO, *]]) attach OrderingF.orderingLift[IO, A]
      (Sorting.bubble, Metrics(arrMetrics.lookups, arrMetrics.swaps, ordMetrics.comparisons))
    }

    def naturals(n: Int): List[Int] = Iterator.range(0, n).toList

    final class ArrayOpsMetrics[F[_]: Monad, A] extends ArrayOps[Mid[F, *], A] {
      val lookups = new AtomicLong(0)
      val swaps   = new AtomicLong(0)

      override def lookup(collection: Array[A], idx: Int): Mid[F, A] =
        (fa: F[A]) => Applicative[F].pure(lookups.incrementAndGet) *> fa
      override def swap(collection: Array[A], idxA: Int, idxB: Int): Mid[F, Unit] =
        (fa: F[Unit]) => Applicative[F].pure(swaps.incrementAndGet) *> fa
    }

    final class OrderingMetrics[F[_]: Monad, A] extends OrderingF[Mid[F, *], A] {
      val comparisons = new AtomicLong(0)

      override def compareF(a: A, b: A): Mid[F, Int] =
        (fa: F[Int]) => Applicative[F].pure(comparisons.incrementAndGet) *> fa
    }
  }

  @Test
  def testSortingRandomizedIntList(): Unit = new TestEnv {
    val data:       List[Int] = naturals(50)
    val randomized: List[Int] = Random.shuffle(data)

    assertEquals(data, ioBubbleSorting[Int].sort(randomized).unsafeRunSync())
  }

  @Test
  def testSortingRandomizedDoubleList(): Unit = new TestEnv {
    val data:       List[Double] = naturals(50).map(_.toDouble + 1.5)
    val randomized: List[Double] = Random.shuffle(data)

    assertEquals(data, ioBubbleSorting[Double].sort(randomized).unsafeRunSync())
  }

  @Test
  def testSortingRandomizedCharList(): Unit = new TestEnv {
    val data:       List[Char] = naturals(50).map(_.toChar)
    val randomized: List[Char] = Random.shuffle(data)

    assertEquals(data, ioBubbleSorting[Char].sort(randomized).unsafeRunSync())
  }

  @Test
  def testSortingLongRandomizedList(): Unit = new TestEnv {
    val data:       List[Int] = naturals(2000)
    val randomized: List[Int] = Random.shuffle(data)

    assertEquals(data, ioBubbleSorting[Int].sort(randomized).unsafeRunSync())
  }

  @Test
  def testSortingReversedList(): Unit = new TestEnv {
    val data:     List[Int] = naturals(100)
    val reversed: List[Int] = data.reverse

    val (sorting, Metrics(lookups, swaps, comparisons)) = ioBubbleSortingWithMetrics[Int]

    assertEquals(data, sorting.sort(reversed).unsafeRunSync())
    assertEquals(9900, lookups.get())
    assertEquals(4950, swaps.get())
    assertEquals(4950, comparisons.get())
  }

  @Test
  def testSortingSortedList(): Unit = new TestEnv {
    val data:     List[Int] = naturals(100)

    val (sorting, Metrics(lookups, swaps, comparisons)) = ioBubbleSortingWithMetrics[Int]

    assertEquals(data, sorting.sort(data).unsafeRunSync())
    assertEquals(198, lookups.get())
    assertEquals(0, swaps.get())
    assertEquals(99, comparisons.get())
  }

  @Test
  def testSortingTailHeadedList(): Unit = new TestEnv {
    val data:       List[Int] = naturals(100)
    val tailHeaded: List[Int] = data.last :: data.init

    val (sorting, Metrics(lookups, swaps, comparisons)) = ioBubbleSortingWithMetrics[Int]

    assertEquals(data, sorting.sort(tailHeaded).unsafeRunSync())
    assertEquals(198 + 196, lookups.get())
    assertEquals(99, swaps.get())
    assertEquals(197, comparisons.get())
  }
}

