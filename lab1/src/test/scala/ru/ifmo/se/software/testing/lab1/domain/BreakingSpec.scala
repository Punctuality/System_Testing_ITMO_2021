package ru.ifmo.se.software.testing.lab1.domain

import cats.effect.IO
import org.junit.Test
import org.junit.Assert._
import ru.ifmo.se.software.testing.lab1.domain.entities.{Furniture, Human}
import ru.ifmo.se.software.testing.lab1.domain.exceptions.DomainException._
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Movable.Position
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Sensible.Sense._

class BreakingSpec {
  trait TestEnv {
    val bob: Human[IO] = Human[IO]("Bob", Position(0, 0, 0), walkingDistance = 10)
    val chair: Furniture[IO] = Furniture("Grandma's chair", Position(1, 1, 1))
  }

  @Test
  def testBreak(): Unit = new TestEnv {
    bob.break(bob).unsafeRunSync()

    assertTrue("Bob should break himself :(", bob.isBroken)
  }

  @Test
  def testBreakRepairable(): Unit = new TestEnv {
    bob.break(chair).unsafeRunSync()

    assertEquals("Chair should be broken and not repaired", true -> false, chair.isBroken -> chair.isRepaired)
  }

  @Test
  def testRepairRepairable(): Unit = new TestEnv {
    bob.break(chair).unsafeRunSync()
    bob.repair(chair).unsafeRunSync()

    assertEquals("Chair should not be broken and be repaired", false -> true, chair.isBroken -> chair.isRepaired)
  }

  @Test
  def testBreakRepaired(): Unit = new TestEnv {
    bob.break(chair).unsafeRunSync()
    bob.repair(chair).unsafeRunSync()
    bob.break(chair).unsafeRunSync()

    assertEquals("Chair should be broken and not repaired", true -> false, chair.isBroken -> chair.isRepaired)
  }

  @Test
  def testFailToBreakBroken(): Unit = new TestEnv {
    bob.break(chair).unsafeRunSync()
    private val expectedException = AlreadyBrokenException(chair)

    bob.break(chair)
      .redeem(
        {
          case exp: AlreadyBrokenException[Furniture[IO]] => assertEquals(expectedException, exp)
          case otherExp => fail(s"Thrown different exp: ${otherExp.getMessage}")
        },
        _ => fail("Shouldn't break something broken")
      ).unsafeRunSync()
  }

  @Test
  def testFailToRepairRepaired(): Unit = new TestEnv {
    bob.break(chair).unsafeRunSync()
    bob.repair(chair).unsafeRunSync()
    private val expectedException = AlreadyRepairedException(chair)

    bob.repair(chair)
      .redeem(
        {
          case exp: AlreadyRepairedException[Furniture[IO]] => assertEquals(expectedException, exp)
          case otherExp => fail(s"Thrown different exp: ${otherExp.getMessage}")
        },
        _ => fail("Shouldn't repair something already repaired")
      ).unsafeRunSync()
  }

  @Test
  def testFailToRepairNotBroken(): Unit = new TestEnv {
    private val expectedException = NotBrokenException(chair)

    bob.repair(chair)
      .redeem(
        {
          case exp: NotBrokenException[Furniture[IO]] => assertEquals(expectedException, exp)
          case otherExp => fail(s"Thrown different exp: ${otherExp.getMessage}")
        },
        _ => fail("Shouldn't repair something which is not broken")
      ).unsafeRunSync()
  }

  @Test
  def testBreakWithAffect(): Unit = new TestEnv {
    bob.break(chair, () => bob.makeFeel(Irritation, bob)).unsafeRunSync()

    assertEquals("Chair should be broken and bob irritated", true -> Set(Irritation), chair.isBroken -> bob.senses)
  }

  @Test
  def testRepairWithAffect(): Unit = new TestEnv {
    bob.break(chair).unsafeRunSync()
    bob.repair(chair, () => bob.makeFeel(Happy, bob)).unsafeRunSync()

    assertEquals("Chair should be repaired and bob happy", true -> Set(Happy), chair.isRepaired -> bob.senses)
  }

  @Test
  def testFailToAffect(): Unit = new TestEnv {
    bob.break(chair).unsafeRunSync()
    private val expectedException = AlreadyBrokenException(chair)

    bob.break(chair, () => bob.makeFeel(Irritation, bob)).redeem(
        {
          case exp: AlreadyBrokenException[Furniture[IO]] =>
            assertEquals("Should fail and not affect bob", (expectedException, Set()), (exp, bob.senses))
          case otherExp => fail(s"Thrown different exp: ${otherExp.getMessage}")
        },
        _ => fail("Shouldn't break something broken")
      ).unsafeRunSync()
  }
}
