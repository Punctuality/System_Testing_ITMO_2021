package ru.ifmo.se.software.testing.lab1.domain

import cats.effect.IO
import org.junit.Test
import org.junit.Assert._
import ru.ifmo.se.software.testing.lab1.domain.entities.{Furniture, Human}
import ru.ifmo.se.software.testing.lab1.domain.exceptions.DomainException.FeelingOmittingException
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Movable.Position
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Sensible.Sense._

class SensesSpec {
  trait TestEnv {
    val bob: Human[IO] = Human[IO]("Bob", Position(0, 0, 0), walkingDistance = 10)
    val tom: Human[IO] = Human[IO]("Tom", Position(1, 1, 1), walkingDistance = 10)
  }

  @Test
  def testMakeYourselfFeel(): Unit = new TestEnv {
    bob.makeFeel(Happy, bob).unsafeRunSync()

    assertEquals("Bob should be happy", Set(Happy), bob.senses)
  }

  @Test
  def testMakeSomeoneElseFeel(): Unit = new TestEnv {
    bob.makeFeel(Happy, tom).unsafeRunSync()

    assertEquals("Tom should be happy", Set(Happy), tom.senses)
  }

  @Test
  def testUpsettingSomeone(): Unit = new TestEnv {
    tom.makeFeel(Happy, tom).unsafeRunSync()
    tom.makeFeel(Love, tom).unsafeRunSync()
    tom.makeFeel(Tired, tom).unsafeRunSync()

    private val tomBeforeBob = tom.senses

    bob.makeFeel(Sad, tom).unsafeRunSync()

    private val tomAfterBob = tom.senses

    assertEquals("Tom should be upset", (Set(Happy, Love, Tired), Set(Sad, Tired)), (tomBeforeBob, tomAfterBob))
  }

  @Test
  def testCheeringUpSomeone(): Unit = new TestEnv {
    tom.makeFeel(Sad, tom).unsafeRunSync()
    tom.makeFeel(Angry, tom).unsafeRunSync()
    tom.makeFeel(Tired, tom).unsafeRunSync()

    private val tomBeforeBob = tom.senses

    bob.makeFeel(Happy, tom).unsafeRunSync()

    private val tomAfterBob = tom.senses

    assertEquals("Tom should be happy", (Set(Sad, Angry, Tired), Set(Happy, Tired)), (tomBeforeBob, tomAfterBob))
  }

  @Test
  def testBeingNeutral(): Unit = new TestEnv {
    tom.makeFeel(Happy, tom).unsafeRunSync()
    tom.makeFeel(Love, tom).unsafeRunSync()

    private val tomBeforeWorkout = tom.senses

    tom.makeFeel(Tired, tom).unsafeRunSync()

    private val tomAfterWorkout = tom.senses

    assertEquals("Tom should be upset", (Set(Happy, Love), Set(Happy, Love, Tired)), (tomBeforeWorkout, tomAfterWorkout))
  }

  @Test
  def testOmitSense(): Unit = new TestEnv {
    tom.makeFeel(Happy, tom).unsafeRunSync()

    private val tomBefore = tom.senses

    bob.omitFeel(Happy, tom).unsafeRunSync()

    private val tomAfter = tom.senses

    assertEquals("Tom should have no feelings", (Set(Happy), Set()), (tomBefore, tomAfter))
  }

  @Test
  def testFailToOmitNonExistent(): Unit = new TestEnv {
    tom.makeFeel(Happy, tom).unsafeRunSync()

    private val expectedException = FeelingOmittingException(
      Tired,
      tom.senses
    )

    bob.omitFeel(Tired, tom).redeem(
      {
        case exp: FeelingOmittingException => assertEquals(expectedException, exp)
        case otherExp => fail(s"Thrown different exp: ${otherExp.getMessage}")
      },
      _ => fail("Shouldn't omit something non-existent")
    ).unsafeRunSync()
  }
}
