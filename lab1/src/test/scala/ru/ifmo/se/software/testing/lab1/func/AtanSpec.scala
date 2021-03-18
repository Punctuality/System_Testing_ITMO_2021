package ru.ifmo.se.software.testing.lab1.func

import cats.effect.IO
import org.junit.Test
import org.junit.Assert.{assertEquals, assertThrows, assertTrue, fail}

class AtanSpec {

  trait TestEnv {
    val floatPrecision: Float = 1e-5F
    val doublePrecision: Double = 1e-8F

    val atanFloat:  Func[IO, Float, Float]   = Func.atan(floatPrecision)
    val atanDouble: Func[IO, Double, Double] = Func.atan(doublePrecision)
  }

  @Test
  def testOnePlusFloat(): Unit = new TestEnv {
    atanFloat(1.0F).map(res => assertEquals(0.7853981633F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testOnePlusDouble(): Unit = new TestEnv {
    atanDouble(1.0).map(res => assertEquals(0.7853981633, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testAround1OnePlusDouble(): Unit = new TestEnv {
    atanDouble(0.99).map(res => assertEquals(0.7803730800, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testAround2OnePlusDouble(): Unit = new TestEnv {
    atanDouble(0.999).map(res => assertEquals(0.7848979133, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testTwoThirdsPlusFloat(): Unit = new TestEnv {
    atanFloat(0.666F).map(res => assertEquals(0.5875409230F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testTwoThirdsPlusDouble(): Unit = new TestEnv {
    atanDouble(0.666).map(res => assertEquals(0.5875409230, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testHalfPlusFloat(): Unit = new TestEnv {
    atanFloat(0.5F).map(res => assertEquals(0.4636476090F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testHalfPlusDouble(): Unit = new TestEnv {
    atanDouble(0.5).map(res => assertEquals(0.4636476090, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testOneThirdPlusFloat(): Unit = new TestEnv {
    atanFloat(0.333F).map(res => assertEquals(0.3214505244F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testOneThirdPlusDouble(): Unit = new TestEnv {
    atanDouble(0.333).map(res => assertEquals(0.3214505244, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testZeroFloat(): Unit = new TestEnv {
    atanFloat(0.0F).map(res => assertEquals(0.0F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testZeroDouble(): Unit = new TestEnv {
    atanDouble(0.0).map(res => assertEquals(0.0, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testOneThirdMinusFloat(): Unit = new TestEnv {
    atanFloat(-0.333F).map(res => assertEquals(-0.3214505244F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testOneThirdMinusDouble(): Unit = new TestEnv {
    atanDouble(-0.333).map(res => assertEquals(-0.3214505244, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testHalfMinusFloat(): Unit = new TestEnv {
    atanFloat(-0.5F).map(res => assertEquals(-0.4636476090F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testHalfMinusDouble(): Unit = new TestEnv {
    atanDouble(-0.5).map(res => assertEquals(-0.4636476090, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testTwoThirdsMinusFloat(): Unit = new TestEnv {
    atanFloat(-0.666F).map(res => assertEquals(-0.5875409230F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testTwoThirdsMinusDouble(): Unit = new TestEnv {
    atanDouble(-0.666).map(res => assertEquals(-0.5875409230, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testAround1OneMinusDouble(): Unit = new TestEnv {
    atanDouble(-0.99).map(res => assertEquals(-0.7803730800, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testAround2OneMinusDouble(): Unit = new TestEnv {
    atanDouble(-0.999).map(res => assertEquals(-0.7848979133, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testOneMinusFloat(): Unit = new TestEnv {
    atanFloat(-1.0F).map(res => assertEquals(-0.7853981633F, res, floatPrecision)).unsafeRunSync()
  }

  @Test
  def testOneMinusDouble(): Unit = new TestEnv {
    atanDouble(-1.0).map(res => assertEquals(-0.7853981633, res, doublePrecision)).unsafeRunSync()
  }

  @Test
  def testLowerBound(): Unit = new TestEnv {
    atanDouble(-1.1)
      .redeem(
        exp => assertThrows(
          s"Argument should be between -1 and 1, but got: -1.1",
          classOf[IllegalArgumentException],
          () => throw exp
        ),
        _ => fail("No exception propagated")
      ).unsafeRunSync()
  }

  @Test
  def testHigherBound(): Unit = new TestEnv {
    atanDouble(1.1)
      .redeem(
        exp => assertThrows(
          s"Argument should be between -1 and 1, but got: 1.1",
          classOf[IllegalArgumentException],
          () => throw exp
        ),
        _ => fail("No exception propagated")
      ).unsafeRunSync()
  }

  @Test
  def testBehaviourAroundZero(): Unit = new TestEnv {
    val comparison: IO[Boolean] = for {
      littleMore <- atanDouble(0.01)
      zeroAtan   <- atanDouble(0.0)
      littleLess <- atanDouble(-0.01)
    } yield (littleLess < zeroAtan) && (zeroAtan < littleMore)

    assertTrue("SHOULD BE: atan(-0.01) < atan(0.0) < atan(0.01)", comparison.unsafeRunSync())
  }
}
