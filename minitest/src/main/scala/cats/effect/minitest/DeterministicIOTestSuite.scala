/*
 * Copyright 2019 Daniel Spiewak
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

package cats.effect.minitest

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO, Timer}
import cats.effect.laws.util.TestContext
import scala.concurrent.duration._

import minitest.api.{DefaultExecutionContext, TestSpec}

abstract class DeterministicIOTestSuite extends BaseIOTestSuite[Unit, TestContext] {
  override protected final def makeExecutionContext(): TestContext = TestContext()


  override protected[effect] implicit def suiteEc: ExecutionContext = DefaultExecutionContext

  override final implicit def ioContextShift: ContextShift[IO] =
    executionContext.contextShift[IO](IO.ioEffect)
  override final implicit def ioTimer: Timer[IO] = executionContext.timer[IO](IO.ioEffect)

  setup(IO.pure(()))
  def test(name: String)(f: => IO[Unit]): Unit = super.test(name)(_ => f)

  override protected[effect] def mkSpec(name: String, ec: TestContext, io: Unit => IO[Unit]): TestSpec[Unit, Unit] =
    TestSpec.sync(name, _ => {
      val f = io(()).unsafeToFuture()
      ec.tick(365.days)
      f.value match {
        case Some(value) => value.get
        case None => throw new RuntimeException(
          s"The IO in ${this.getClass.getName}.$name did not terminate.\n" +
          "It's possible that you are using a ContextShift that is backed by other ExecutionContext or" +
          "the test code is waiting indefinitely."
        )
      }
    })
}
