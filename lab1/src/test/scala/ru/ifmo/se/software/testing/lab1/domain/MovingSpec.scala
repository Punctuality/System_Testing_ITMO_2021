package ru.ifmo.se.software.testing.lab1.domain

import cats.effect.IO
import org.junit.Test
import org.junit.Assert._
import ru.ifmo.se.software.testing.lab1.domain.entities.{Furniture, Human}
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Movable.Position
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Sensible.Sense._

class MovingSpec {

  trait TestEnv {
    val human: Human[IO] = Human[IO]("Bob", Position(0, 0, 0), walkingDistance = 10)
    val chair: Furniture[IO] = Furniture("Grandma's chaid", Position(1, 1, 1))
  }

  @Test
  def moveToPoint(): Unit = new TestEnv {
    private val expectedPos = Position(6, 6, 6)
    human.move(expectedPos, human).unsafeRunSync()
    assertEquals("Human position equals to new one", expectedPos, human.position)
  }

  @Test
  def moveABit(): Unit = new TestEnv {
    private val expectedPos = Position(1, 1, 1)
    human.move(expectedPos, human).unsafeRunSync()
    assertEquals("Human is moved and is not tired", (expectedPos, Set.empty), (human.position, human.senses))
  }

  @Test
  def moveFarAway(): Unit = new TestEnv {
    private val expectedPos = Position(666, 666, 666)
    human.move(expectedPos, human).unsafeRunSync()
    assertEquals("Human is moved and is tired", (expectedPos, Set(Tired)), (human.position, human.senses))
  }

  @Test
  def moveSomethingElse(): Unit = new TestEnv {
    private val expectedPos = Position(666, 666, 666)
    human.move(expectedPos, chair).unsafeRunSync()
    assertEquals("Human moved chair to pos and is tired", (expectedPos, Set(Tired)), (chair.position, human.senses))
  }

  @Test
  def moveAndAffect(): Unit = new TestEnv {
    var sounds: List[Furniture.Noise] = List.empty

    human.move(chair.position, human, () => chair.makeSound(true, "creak").map(noise => sounds = noise :: sounds))
  }
}
