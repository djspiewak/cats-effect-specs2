/*
 * Copyright 2021 Daniel Spiewak
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

package cats.effect.testing.minitest

import scala.concurrent.ExecutionContext

import cats.effect.IO
import minitest.api._
import cats.effect.Temporal


private[effect] abstract class BaseIOTestSuite[Ec <: ExecutionContext] extends AbstractTestSuite with Asserts {
  protected def makeExecutionContext(): Ec

  private[effect] lazy val executionContext: Ec = makeExecutionContext()
  protected[effect] implicit def suiteEc: ExecutionContext = executionContext

  implicit def ioContextShift: ContextShift[IO] = IO.contextShift(executionContext)
  implicit def ioTimer: Temporal[IO] = IO.timer(executionContext)

  protected[effect] def mkSpec(name: String, ec: Ec, io: => IO[Unit]): TestSpec[Unit, Unit]

  def test(name: String)(f: => IO[Unit]): Unit =
    synchronized {
      if (isInitialized) throw new AssertionError("Cannot define new tests after TestSuite was initialized")
      propertiesSeq :+= mkSpec(name, executionContext, f)
    }

  lazy val properties: Properties[_] =
    synchronized {
      if (!isInitialized) isInitialized = true
      Properties[Unit](() => (), _ => Void.UnitRef, () => (), () => (), propertiesSeq)
    }

  private[this] var propertiesSeq = Vector.empty[TestSpec[Unit, Unit]]
  private[this] var isInitialized = false
}
