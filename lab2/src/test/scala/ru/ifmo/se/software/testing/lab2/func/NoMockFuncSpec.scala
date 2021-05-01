package ru.ifmo.se.software.testing.lab2.func

import cats.Monad
import cats.Traverse
import cats.implicits._
import cats.effect.IO
import org.junit.Test
import org.junit.Assert._

import scala.math.Pi
import scala.reflect.ClassTag

class NoMockFuncSpec {
  trait TestEnv {
    val doublePrecision: Double = 1e-8
    val funcPrecision: Double = 1e-7

    implicit val sin: Sin[IO, Double] = Sin(doublePrecision)
    implicit val ln: Ln[IO, Double] = Ln(doublePrecision)
    val generalFunc: GeneralFunc[IO, Double] = GeneralFunc[IO, Double]

    case class PointCheck[N](left: N, point: N, right: N) {
      def fmap[F[_]: Monad, B](f: N => F[B]): F[PointCheck[B]] = for {
        leftF <- f(left)
        pointF <- f(point)
        rightF <- f(right)
      } yield PointCheck(leftF, pointF, rightF)

      def toArr(implicit CT: ClassTag[N]): Array[N] = Array(left, point, right)
    }
    implicit def arrayConv[N: ClassTag](pointCheck: PointCheck[N]): Array[N] = pointCheck.toArr
  }

  // Меньше нуля или равно: x <= 0
  // (((((cos(x) ^ 2) / tan(x)) + sin(x)) * (cos(x) * cos(x))) - (cot(x) + tan(x)))
  // Переодична: 2π
  // Монотонно убывает на полу-периоде
  // Присутствует два типа кривых (1-тип около нечетных π, 2-тип около четных π)
  // x = π*k => func = 0 (точка разрыва, первого рода, устранимая)
  // x = π/2 - π*k => func = ±inf (точка разрыва, второго рода)

  @Test
  def testZeroXZeroY(): Unit = new TestEnv {
    private val x = 0.0
    private val y = generalFunc(x).unsafeRunSync()
    private val expected = 0.0

    assertEquals(expected, y, funcPrecision)
  }

  @Test
  def testMinusPiXZeroY(): Unit = new TestEnv {
    private val x = -Pi
    private val y = generalFunc(x).unsafeRunSync()
    private val expected = 0.0

    assertEquals(expected, y, funcPrecision)
  }

  @Test
  def testMinusTwoPiXZeroY(): Unit = new TestEnv {
    private val x = -2*Pi
    private val y = generalFunc(x).unsafeRunSync()
    private val expected = 0.0

    assertEquals(expected, y, funcPrecision)
  }

  @Test
  def testFallsAroundMinusPiXZeroY(): Unit = new TestEnv {
    private val x = PointCheck(-Pi - 0.05, -Pi, -Pi + 0.05)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
      0.19960476,
      0.0,
      -0.19960476
    )

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testFallsAroundMinusTwoPiXZeroY(): Unit = new TestEnv {
    private val x = PointCheck(-2*Pi - 0.05, -2*Pi, -2*Pi + 0.05)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
      0.09989610,
      0.0,
      -0.09989611
    )

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testMinusHalfPiXInfY(): Unit = new TestEnv {
    private val x = -Pi/2
    private val y = generalFunc(x).unsafeRunSync()
    private val expected = Double.NegativeInfinity

    assertEquals(expected, y, funcPrecision)
  }

  @Test
  def testMinusOneAndHalfPiXInfY(): Unit = new TestEnv {
    private val x = -3*Pi/2
    private val y = generalFunc(x).unsafeRunSync()
    private val expected = Double.PositiveInfinity

    assertEquals(expected, y, funcPrecision)
  }

  @Test
  def testAroundMinusHalfPiXInfY(): Unit = new TestEnv {
    private val x = PointCheck(-Pi/2 - 0.1, -Pi/2, -Pi/2 + 0.1)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
      -10.07688770,
      Double.NegativeInfinity,
      10.05705245
    )

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testAroundOneAndHalfPiXInfY(): Unit = new TestEnv {
    private val x = PointCheck(-3*Pi/2 - 0.1, -3*Pi/2, -3*Pi/2 + 0.1)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
      -10.05705933,
      Double.PositiveInfinity,
      10.07688851
    )

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testFallsOnFirstHalfPeriod(): Unit = new TestEnv {
    private val x = Iterator.iterate(-2*Pi - Pi/2)(_ + 0.05).takeWhile(_ <= -2*Pi + Pi/2).toList.tail // Starts with -Inf
    private val y = Traverse[List].traverse(x)(generalFunc.apply).unsafeRunSync()
    private val result = y.foldLeft(Double.PositiveInfinity -> true){
      case ((num, res), next) => next -> (res && (num >= next))
    }._2

    assertTrue("Should fall on half period", result)
  }

  @Test
  def testFallsOnSecondHalfPeriod(): Unit = new TestEnv {
    private val x = Iterator.iterate(-3*Pi - Pi/2)(_ + 0.05).takeWhile(_ <= -3*Pi + Pi/2).toList
    private val y = Traverse[List].traverse(x)(generalFunc.apply).unsafeRunSync()
    private val result = y.foldLeft(Double.PositiveInfinity -> true){
      case ((num, res), next) => next -> (res && (num >= next))
    }._2

    assertTrue("Should fall on half period", result)
  }

  @Test
  def testFirstTypeCurvatureBehaviour(): Unit = new TestEnv {
    private val x1 = Iterator.iterate(-Pi - 1.5)(_ + 0.05).takeWhile(_ <= -Pi + 1.5).toList
    private val x2 = x1.map(_ - 2*Pi)
    private val y1 = Traverse[List].traverse(x1)(generalFunc.apply).unsafeRunSync()
    private val y2 = Traverse[List].traverse(x2)(generalFunc.apply).unsafeRunSync()

    assertArrayEquals(y1.toArray, y2.toArray, funcPrecision)
  }

  @Test
  def testSecondTypeCurvatureBehaviour(): Unit = new TestEnv {
    private val x1 = Iterator.iterate(-2*Pi - 1.5)(_ + 0.05).takeWhile(_ <= -2*Pi + 1.5).toList
    private val x2 = x1.map(_ - 2*Pi)
    private val y1 = Traverse[List].traverse(x1)(generalFunc.apply).unsafeRunSync()
    private val y2 = Traverse[List].traverse(x2)(generalFunc.apply).unsafeRunSync()

    assertArrayEquals(y1.toArray, y2.toArray, funcPrecision)
  }

  // Больше нуля: x > 0
  // (((((log_3(x) - log_2(x)) ^ 2) ^ 2) ^ 3) / log_5(x))
  // Начинает в -inf, монотонно растет до +inf
  // x = 1 => func = 0

  @Test
  def testOneXZeroY(): Unit = new TestEnv {
    private val x = PointCheck(0.99, 1, 1.01)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(0.0, 0.0, 0.0)

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testABitMoreThanZeroXNegativeY(): Unit = new TestEnv {
    private val x = PointCheck(0.29, 0.3, 0.31)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
      -0.00874015,
      -0.00643962,
      -0.00475279
    )

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testMoreThanOneXPositiveY(): Unit = new TestEnv {
    private val x = PointCheck(5.29, 5.3, 5.31)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
      0.22907668,
      0.23194973,
      0.23484999
    )

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testBigXBigY(): Unit = new TestEnv {
    private val x: PointCheck[Double] = PointCheck(80, 100, 120)
    private val y = x.fmap(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
       9560.97645552,
      16511.23773598,
      25308.44815165
    )

    assertArrayEquals(expected, y, funcPrecision)
  }
}
