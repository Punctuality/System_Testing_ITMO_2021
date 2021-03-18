package ru.ifmo.se.software.testing.lab1

import cats.MonadError
import ru.ifmo.se.software.testing.lab1.domain.exceptions.DomainException

package object domain {
  type DomainMonad[F[_]] = MonadError[F, DomainException]
  object DomainMonad {
    def apply[F[_]: DomainMonad]: DomainMonad[F] = implicitly[DomainMonad[F]]
  }
}
