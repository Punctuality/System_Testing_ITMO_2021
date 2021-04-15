package ru.ifmo.se.software.testing.lab1.domain.traits.active

trait MakingSpeech[F[_], C] extends MakingSound[F, C, MakingSpeech.Speech]

object MakingSpeech {
  case class Speech(author: String, words: List[String]) {
    lazy val writtenForm: String = s"""$author said: "${words.mkString(" ")}""""
  }
}