/*
 * Copyright 2020-2021 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.effect.testing.scalatest

import cats.effect.Sync
import org.scalactic.source
import org.scalatest.{Informing, GivenWhenThen => ScalaTestGivenWhenThen}

trait GivenWhenThen[F[_]] extends ScalaTestGivenWhenThen { this: Informing =>

  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "Given."
   *
   * @param message the message to forward to the passed informer
   */
  def Given(message: String)(implicit F: Sync[F], pos: source.Position): F[Unit] = F.delay(super.Given(message))

  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "When ".
   *
   * @param message the message to forward to the passed informer
   */
  def When(message: String)(implicit F: Sync[F], pos: source.Position): F[Unit] = F.delay(super.When(message))

  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "Then ".
   *
   * @param message the message to forward to the passed informer
   */
  def Then(message: String)(implicit F: Sync[F], pos: source.Position): F[Unit] = F.delay(super.Then(message))


  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "And ".
   *
   * @param message the message to forward to the passed informer
   */
  def And(message: String)(implicit F: Sync[F], pos: source.Position): F[Unit] = F.delay(super.And(message))
}
