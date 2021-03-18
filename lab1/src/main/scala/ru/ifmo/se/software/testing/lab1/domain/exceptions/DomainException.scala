package ru.ifmo.se.software.testing.lab1.domain.exceptions

import ru.ifmo.se.software.testing.lab1.domain.traits.passive.{Breakable, Repairable, Sensible}

sealed abstract class DomainException(msg: String) extends Exception(msg)

object DomainException {
  case class FeelingOmittingException(toOmit: Sensible.Sense, currentSenses: Set[Sensible.Sense])
    extends DomainException(s"Wasn't able to omit $toOmit, but the one has just: ${currentSenses.mkString(", ")}")

  case class AlreadyBrokenException[T <: Breakable](entity: T)
    extends DomainException(s"Cannot break $entity. Already broken.")

  case class AlreadyRepairedException[T <: Repairable](entity: T)
    extends DomainException(s"Cannot repair $entity. Already repaired.")

  case class NotBrokenException[T <: Repairable](entity: T)
    extends DomainException(s"Cannot repair $entity. The $entity isn't broken.")
}