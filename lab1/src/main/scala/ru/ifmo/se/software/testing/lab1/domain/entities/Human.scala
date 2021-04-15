package ru.ifmo.se.software.testing.lab1.domain.entities

import cats.{Applicative, MonadThrow}
import tofu.syntax.monadic._
import ru.ifmo.se.software.testing.lab1.domain.exceptions.DomainException._
import ru.ifmo.se.software.testing.lab1.domain.traits.active.{MakingSpeech, Repairing}
import ru.ifmo.se.software.testing.lab1.domain.traits.active.MakingSpeech.Speech
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Sensible.Sense.Tired
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.Sensible.SenseGroup
import ru.ifmo.se.software.testing.lab1.domain.traits.passive.{Breakable, Movable, Repairable, Sensible}

case class Human[F[_]: MonadThrow](
    override val name: String,
    override var position: Movable.Position,
    walkingDistance: Double
  ) extends Animal[F, (String, () => F[Unit]), Speech](name)
  with MakingSpeech[F, (String, () => F[Unit])]
  with Repairing[F] {

  override def makeSound(context: (String, () => F[Unit])): F[Speech] =
    Applicative[F].pure(Speech(name, context._1.split("""\s""").toList)).flatTap(_ => context._2())

  override def makeFeel[T <: Sensible](newSense: Sensible.Sense, who: T): F[Unit] =
    Applicative[F].pure(who.senses = (newSense.group match {
      case SenseGroup.Positive => who.senses.filterNot(_.group == SenseGroup.Negative)
      case SenseGroup.Negative => who.senses.filterNot(_.group == SenseGroup.Positive)
      case SenseGroup.Neutral  => who.senses
    }) + newSense)

  override def omitFeel[T <: Sensible](oldSense: Sensible.Sense, who: T): F[Unit] =
    MonadThrow[F].ensure(Applicative[F] pure who.senses)(
      FeelingOmittingException(oldSense, who.senses)
    )(_.contains(oldSense)).map( senses =>
      who.senses = senses - oldSense
    )

  override def break[T <: Breakable](toBreak: T): F[Unit] =
    toBreak match {
      case alreadyBroken if alreadyBroken.isBroken =>
        MonadThrow[F].raiseError(AlreadyBrokenException(alreadyBroken))
      case repairable: Repairable =>
        Applicative[F].pure {
          repairable.isBroken   = true
          repairable.isRepaired = false
        }
      case justBreakable          =>
        Applicative[F].pure(justBreakable.isBroken = true)
    }

  override def repair[T <: Repairable](toRepair: T): F[Unit] = toRepair match {
    case alreadyRepaired if alreadyRepaired.isRepaired =>
      MonadThrow[F].raiseError(AlreadyRepairedException(toRepair))
    case notBroken if !notBroken.isBroken && !notBroken.isRepaired =>
      MonadThrow[F].raiseError(NotBrokenException(toRepair))
    case repairable =>
      Applicative[F].pure{
        repairable.isBroken   = false
        repairable.isRepaired = true
      }
  }

  override def move[T <: Movable](newPosition: Movable.Position, toMove: T): F[Unit] =
      Applicative[F]
        .whenA(newPosition.distanceTo(toMove.position) > walkingDistance)(makeFeel(Tired, this)).map(_ =>
        toMove.position = newPosition
      )
}
