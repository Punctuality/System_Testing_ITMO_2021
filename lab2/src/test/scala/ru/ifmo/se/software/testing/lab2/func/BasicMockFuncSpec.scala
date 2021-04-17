package ru.ifmo.se.software.testing.lab2.func

import cats.Monad
import cats.implicits._
import cats.effect.IO
import org.junit.Test
import org.junit.Assert._
import org.mockito.Mockito
import org.mockito.Mockito._

import scala.math.Pi
import scala.reflect.ClassTag

class BasicMockFuncSpec {
  trait TestEnv {
    val doublePrecision: Double = 1e-8
    val funcPrecision: Double = 1e-3

    implicit val sin: Sin[IO, Double] = Mockito.mock(classOf[Sin[IO, Double]])
    implicit val ln: Ln[IO, Double] = Mockito.mock(classOf[Ln[IO, Double]])
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

    // Mocks

    // Sins
    when(sin.precision).thenReturn(doublePrecision)
    when(sin.apply(0.0)).thenReturn(IO.pure(0.0))
    when(sin.apply(-Pi/2+0.1)).thenReturn(IO.pure(-0.9950041652780))
    when(sin.apply(-Pi/2)).thenReturn(IO.pure(-1.0))
    when(sin.apply(-Pi/2-0.1)).thenReturn(IO.pure(-0.9950041652780))
    when(sin.apply(-Pi+0.05)).thenReturn(IO.pure(-0.04997916927))
    when(sin.apply(-Pi)).thenReturn(IO.pure(0.0))
    when(sin.apply(-Pi-0.05)).thenReturn(IO.pure(0.04997916927))
    when(sin.apply(-3*Pi/2+0.1)).thenReturn(IO.pure(0.9950041652780))
    when(sin.apply(-3*Pi/2)).thenReturn(IO.pure(1.0))
    when(sin.apply(-3*Pi/2-0.1)).thenReturn(IO.pure(0.9950041652780))
    when(sin.apply(-Pi*2+0.05)).thenReturn(IO.pure(0.04997916927))
    when(sin.apply(-Pi*2)).thenReturn(IO.pure(0.0))
    when(sin.apply(-Pi*2-0.05)).thenReturn(IO.pure(-0.04997916927))

    // Lns
    when(ln.precision).thenReturn(doublePrecision)
    when(ln.apply(2.0)).thenReturn(IO.pure(0.693147180559))
    when(ln.apply(3.0)).thenReturn(IO.pure(1.098612288668))
    when(ln.apply(5.0)).thenReturn(IO.pure(1.609437912434))
    when(ln.apply(0.99)).thenReturn(IO.pure(-0.010050336))
    when(ln.apply(1.0)).thenReturn(IO.pure(0.0))
    when(ln.apply(1.01)).thenReturn(IO.pure(0.009950331))
    when(ln.apply(0.29)).thenReturn(IO.pure(-1.237874356))
    when(ln.apply(0.3)).thenReturn(IO.pure(-1.203972804))
    when(ln.apply(0.31)).thenReturn(IO.pure(-1.171182982))
    when(ln.apply(5.29)).thenReturn(IO.pure(1.665818246))
    when(ln.apply(5.3)).thenReturn(IO.pure(1.667706821))
    when(ln.apply(5.31)).thenReturn(IO.pure(1.669591835))
    when(ln.apply(80)).thenReturn(IO.pure(4.382026634673))
    when(ln.apply(100)).thenReturn(IO.pure(4.60517018598))
    when(ln.apply(120)).thenReturn(IO.pure(4.78749174278))
  }

  // Меньше нуля

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

  // Больше нуля

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
