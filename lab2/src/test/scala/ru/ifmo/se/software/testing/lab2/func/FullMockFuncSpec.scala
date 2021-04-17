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

class FullMockFuncSpec {
  trait TestEnv {
    val doublePrecision: Double = 1e-8
    val funcPrecision: Double = 1e-3

    implicit val sinM: Sin[IO, Double] = Mockito.mock(classOf[Sin[IO, Double]])
    implicit val lnM: Ln[IO, Double] = null
    val cosM: Cos[IO, Double] = Mockito.mock(classOf[Cos[IO, Double]])
    val tanM: Tan[IO, Double] = Mockito.mock(classOf[Tan[IO, Double]])
    val cotM: Cot[IO, Double] = Mockito.mock(classOf[Cot[IO, Double]])

    val log2M: Log[IO, Double] = Mockito.mock(classOf[Log[IO, Double]])
    val log3M: Log[IO, Double] = Mockito.mock(classOf[Log[IO, Double]])
    val log5M: Log[IO, Double] = Mockito.mock(classOf[Log[IO, Double]])
    val generalFunc: GeneralFunc[IO, Double] = new GeneralFunc[IO, Double]{
      override lazy val cos: Cos[IO, Double] = cosM
      override lazy val tan: Tan[IO, Double] = tanM
      override lazy val cot: Cot[IO, Double] = cotM

      override lazy val log2: Log[IO, Double] = log2M
      override lazy val log3: Log[IO, Double] = log3M
      override lazy val log5: Log[IO, Double] = log5M
    }

    case class PointCheck[N](left: N, point: N, right: N) {
      def map[F[_]: Monad, B](f: N => F[B]): F[PointCheck[B]] = for {
        leftF <- f(left)
        pointF <- f(point)
        rightF <- f(right)
      } yield PointCheck(leftF, pointF, rightF)

      def toArr(implicit CT: ClassTag[N]): Array[N] = Array(left, point, right)
    }
    implicit def arrayConv[N: ClassTag](pointCheck: PointCheck[N]): Array[N] = pointCheck.toArr

    // Mocks

    // Sins
    when(sinM.precision).thenReturn(doublePrecision)
    when(sinM.apply(0.0)).thenReturn(IO.pure(0.0))
    when(sinM.apply(-Pi/2+0.1)).thenReturn(IO.pure(-0.9950041652780))
    when(sinM.apply(-Pi/2)).thenReturn(IO.pure(-1.0))
    when(sinM.apply(-Pi/2-0.1)).thenReturn(IO.pure(-0.9950041652780))
    when(sinM.apply(-Pi+0.05)).thenReturn(IO.pure(-0.04997916927))
    when(sinM.apply(-Pi)).thenReturn(IO.pure(0.0))
    when(sinM.apply(-Pi-0.05)).thenReturn(IO.pure(0.04997916927))
    when(sinM.apply(-3*Pi/2+0.1)).thenReturn(IO.pure(0.9950041652780))
    when(sinM.apply(-3*Pi/2)).thenReturn(IO.pure(1.0))
    when(sinM.apply(-3*Pi/2-0.1)).thenReturn(IO.pure(0.9950041652780))
    when(sinM.apply(-Pi*2+0.05)).thenReturn(IO.pure(0.04997916927))
    when(sinM.apply(-Pi*2)).thenReturn(IO.pure(0.0))
    when(sinM.apply(-Pi*2-0.05)).thenReturn(IO.pure(-0.04997916927))

    // Coses
    when(cosM.apply(0.0)).thenReturn(IO.pure(1.0))
    when(cosM.apply(-Pi/2+0.1)).thenReturn(IO.pure(0.09983341664682831))
    when(cosM.apply(-Pi/2)).thenReturn(IO.pure(0.0))
    when(cosM.apply(-Pi/2-0.1)).thenReturn(IO.pure(-0.09983341664682818))
    when(cosM.apply(-Pi+0.05)).thenReturn(IO.pure(-0.9987502603949663))
    when(cosM.apply(-Pi)).thenReturn(IO.pure(-1.0))
    when(cosM.apply(-Pi-0.05)).thenReturn(IO.pure(-0.9987502603949663))
    when(cosM.apply(-3*Pi/2+0.1)).thenReturn(IO.pure(-0.09983341664682799))
    when(cosM.apply(-3*Pi/2)).thenReturn(IO.pure(0.0))
    when(cosM.apply(-3*Pi/2-0.1)).thenReturn(IO.pure(0.09983341664682761))
    when(cosM.apply(-Pi*2+0.05)).thenReturn(IO.pure(0.998750260394966246))
    when(cosM.apply(-Pi*2)).thenReturn(IO.pure(1.0))
    when(cosM.apply(-Pi*2-0.05)).thenReturn(IO.pure(0.998750260394966246))

    // Tans
    when(tanM.apply(0.0)).thenReturn(IO.pure(0.0))
    when(tanM.apply(-Pi/2+0.1)).thenReturn(IO.pure(-9.9666444232592378597))
    when(tanM.apply(-Pi/2)).thenReturn(IO.pure(Double.PositiveInfinity))
    when(tanM.apply(-Pi/2-0.1)).thenReturn(IO.pure(9.9666444232592378597))
    when(tanM.apply(-Pi+0.05)).thenReturn(IO.pure(0.050041708375538736))
    when(tanM.apply(-Pi)).thenReturn(IO.pure(0.0))
    when(tanM.apply(-Pi-0.05)).thenReturn(IO.pure(-0.050041708375538486))
    when(tanM.apply(-3*Pi/2+0.1)).thenReturn(IO.pure(-9.966644423259256))
    when(tanM.apply(-3*Pi/2)).thenReturn(IO.pure(Double.NegativeInfinity))
    when(tanM.apply(-3*Pi/2-0.1)).thenReturn(IO.pure(+9.966644423259256))
    when(tanM.apply(-Pi*2+0.05)).thenReturn(IO.pure(0.050041708375538854))
    when(tanM.apply(-Pi*2)).thenReturn(IO.pure(0.0))
    when(tanM.apply(-Pi*2-0.05)).thenReturn(IO.pure(-0.050041708375538486))

    // Cots
    when(cotM.apply(0.0)).thenReturn(IO.pure(Double.NegativeInfinity))
    when(cotM.apply(-Pi/2+0.1)).thenReturn(IO.pure(-0.1003346720854507))
    when(cotM.apply(-Pi/2)).thenReturn(IO.pure(0.0))
    when(cotM.apply(-Pi/2-0.1)).thenReturn(IO.pure(0.10033467208545058))
    when(cotM.apply(-Pi+0.05)).thenReturn(IO.pure(19.983330554894035))
    when(cotM.apply(-Pi)).thenReturn(IO.pure(Double.NegativeInfinity))
    when(cotM.apply(-Pi-0.05)).thenReturn(IO.pure(-19.983330554894135))
    when(cotM.apply(-3*Pi/2+0.1)).thenReturn(IO.pure(-0.10033467208545037))
    when(cotM.apply(-3*Pi/2)).thenReturn(IO.pure(0.0))
    when(cotM.apply(-3*Pi/2-0.1)).thenReturn(IO.pure(0.10033467208545001))
    when(cotM.apply(-Pi*2+0.05)).thenReturn(IO.pure(19.98333055489399))
    when(cotM.apply(-Pi*2)).thenReturn(IO.pure(Double.NegativeInfinity))
    when(cotM.apply(-Pi*2-0.05)).thenReturn(IO.pure(-19.98333055489418))

    // Logs 2
    when(log2M.apply(0.99)).thenReturn(IO.pure(-0.0144995696951150))
    when(log2M.apply(1.0)).thenReturn(IO.pure(0.0))
    when(log2M.apply(1.01)).thenReturn(IO.pure(0.01435529297707004143))
    when(log2M.apply(0.29)).thenReturn(IO.pure(-1.785875194647152575))
    when(log2M.apply(0.3)).thenReturn(IO.pure(-1.7369655941662061664165))
    when(log2M.apply(0.31)).thenReturn(IO.pure(-1.68965987938784948))
    when(log2M.apply(5.29)).thenReturn(IO.pure(2.40326772233930104884))
    when(log2M.apply(5.3)).thenReturn(IO.pure(2.405992359675837))
    when(log2M.apply(5.31)).thenReturn(IO.pure(2.408711861029429))
    when(log2M.apply(80)).thenReturn(IO.pure(6.321928094887363))
    when(log2M.apply(100)).thenReturn(IO.pure(6.643856189774724))
    when(log2M.apply(120)).thenReturn(IO.pure(6.906890595608519))

    // Logs 3
    when(log3M.apply(0.99)).thenReturn(IO.pure(-0.009148209934631127544154))
    when(log3M.apply(1.0)).thenReturn(IO.pure(0.0))
    when(log3M.apply(1.01)).thenReturn(IO.pure(0.0090571814604688748))
    when(log3M.apply(0.29)).thenReturn(IO.pure(-1.1267617964681065580))
    when(log3M.apply(0.3)).thenReturn(IO.pure(-1.09590327428938460429656))
    when(log3M.apply(0.31)).thenReturn(IO.pure(-1.0660566913217543722))
    when(log3M.apply(5.29)).thenReturn(IO.pure(1.51629311182177300621))
    when(log3M.apply(5.3)).thenReturn(IO.pure(1.5180121665850851196515))
    when(log3M.apply(5.31)).thenReturn(IO.pure(1.51972798090394422481))
    when(log3M.apply(80)).thenReturn(IO.pure(3.988692535003756915595148))
    when(log3M.apply(100)).thenReturn(IO.pure(4.191806548578769208593135))
    when(log3M.apply(120)).thenReturn(IO.pure(4.3577627814322994784956217))

    // Logs 5
    when(log5M.apply(0.99)).thenReturn(IO.pure(-0.00624462476983743827187))
    when(log5M.apply(1.0)).thenReturn(IO.pure(0.0))
    when(log5M.apply(1.01)).thenReturn(IO.pure(0.00618248816949967711327))
    when(log5M.apply(0.29)).thenReturn(IO.pure(-0.769134581979286524162))
    when(log5M.apply(0.3)).thenReturn(IO.pure(-0.7480703635874077555355))
    when(log5M.apply(0.31)).thenReturn(IO.pure(-0.727696901169463457198181))
    when(log5M.apply(5.29)).thenReturn(IO.pure(1.035031070785973033562))
    when(log5M.apply(5.3)).thenReturn(IO.pure(1.0362045082160705214))
    when(log5M.apply(5.31)).thenReturn(IO.pure(1.037375733698711498210))
    when(log5M.apply(80)).thenReturn(IO.pure(2.722706232293572202680426))
    when(log5M.apply(100)).thenReturn(IO.pure(2.861353116146786101340213))
    when(log5M.apply(120)).thenReturn(IO.pure(2.97463586870616444714))
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
    private val y = x.map(generalFunc.apply).unsafeRunSync()
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
    private val y = x.map(generalFunc.apply).unsafeRunSync()
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
    private val y = x.map(generalFunc.apply).unsafeRunSync()
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
    private val y = x.map(generalFunc.apply).unsafeRunSync()
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
    private val y = x.map(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(0.0, 0.0, 0.0)

    assertArrayEquals(expected, y, funcPrecision)
  }

  @Test
  def testABitMoreThanZeroXNegativeY(): Unit = new TestEnv {
    private val x = PointCheck(0.29, 0.3, 0.31)
    private val y = x.map(generalFunc.apply).unsafeRunSync()
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
    private val y = x.map(generalFunc.apply).unsafeRunSync()
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
    private val y = x.map(generalFunc.apply).unsafeRunSync()
    private val expected = PointCheck(
      9560.97645552,
      16511.23773598,
      25308.44815165
    )

    assertArrayEquals(expected, y, funcPrecision)
  }
}
