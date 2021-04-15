package ru.ifmo.se.software.testing.lab1.domain.traits.passive

import enumeratum._

trait Sensible {
  var senses: Set[Sensible.Sense]
}

object Sensible {
  sealed trait SenseGroup extends EnumEntry
  object SenseGroup extends Enum[SenseGroup] {
    case object Positive extends SenseGroup
    case object Neutral extends SenseGroup
    case object Negative extends SenseGroup
    override def values: IndexedSeq[SenseGroup] = findValues
  }

  sealed abstract class Sense(val group: SenseGroup) extends EnumEntry
  object Sense extends Enum[Sense] {
    case object Irritation extends Sense(SenseGroup.Negative)
    case object Happy      extends Sense(SenseGroup.Positive)
    case object Angry      extends Sense(SenseGroup.Negative)
    case object Love       extends Sense(SenseGroup.Positive)
    case object Sad        extends Sense(SenseGroup.Negative)
    case object Tired      extends Sense(SenseGroup.Neutral)
    override def values: IndexedSeq[Sense] = findValues
  }
}