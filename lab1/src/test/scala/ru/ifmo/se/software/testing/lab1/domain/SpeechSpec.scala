package ru.ifmo.se.software.testing.lab1.domain

import cats.effect.IO
import org.junit.Test
import org.junit.Assert._
import ru.ifmo.se.software.testing.lab1.domain.entities.{Furniture, Human}
import ru.ifmo.se.software.testing.lab1.domain.traits.active.MakingSpeech
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Movable.Position
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Sensible.Sense._

class SpeechSpec {
  trait TestEnv {
    val bob: Human[IO] = Human[IO]("Bob", Position(0, 0, 0), walkingDistance = 10)
    val tom: Human[IO] = Human[IO]("Tom", Position(1, 1, 1), walkingDistance = 10)
    val chair: Furniture[IO] = Furniture("Grandma's chair", Position(1, 1, 1))
  }

  @Test
  def testSimpleSpeech(): Unit = new TestEnv {
    private val expectedSpeech = MakingSpeech.Speech("Bob", "Bla" :: "bla" :: "bla" :: Nil)
    private val resultSpeech = bob.makeSound(("Bla bla bla", () => IO.unit)).unsafeRunSync()

    assertEquals(expectedSpeech, resultSpeech)
  }

  @Test
  def testSpeechPhrase(): Unit = new TestEnv {
    private val expectedPhrase = """Bob said: "Bla bla bla""""
    private val resultSpeech = bob.makeSound(("Bla bla bla", () => IO.unit)).unsafeRunSync()

    assertEquals(expectedPhrase, resultSpeech.writtenForm)
  }

  @Test
  def testHumanWordsParsing(): Unit = new TestEnv {
    private val expectedSpeech = MakingSpeech.Speech("Bob", "Bla" :: "bla" :: "bla" :: Nil)
    private val resultSpeech = bob.makeSound(("Bla\tbla\tbla", () => IO.unit)).unsafeRunSync()

    assertEquals(expectedSpeech, resultSpeech)
  }

  @Test
  def testSpeechAffectsSpeaker(): Unit = new TestEnv {
    private val expectedSpeech = MakingSpeech.Speech("Bob", "I" :: "am" :: "happy" :: Nil)
    private val resultSpeech = bob.makeSound(("I am happy", () => bob.makeFeel(Happy, bob))).unsafeRunSync()

    assertEquals((expectedSpeech, Set(Happy)), (resultSpeech, bob.senses))
  }

  @Test
  def testSpeechAffectsSomeoneElse(): Unit = new TestEnv {
    private val expectedSpeech = MakingSpeech.Speech("Bob", "You" :: "are" :: "ugly" :: Nil)
    private val resultSpeech = bob.makeSound(("You are ugly", () => bob.makeFeel(Sad, tom))).unsafeRunSync()

    assertEquals((expectedSpeech, Set(Sad)), (resultSpeech, tom.senses))
  }

  @Test
  def testFurnitureNoise(): Unit = new TestEnv {
    private val expectedSpeech = Furniture.Noise(isLoud = true, "creak")
    private val resultSpeech = chair.makeSound(true -> "creak").unsafeRunSync()

    assertEquals(expectedSpeech, resultSpeech)
  }
}
