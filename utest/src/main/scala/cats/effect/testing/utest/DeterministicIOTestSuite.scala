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

package cats.effect.testing.utest

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

import cats.effect.IO
import cats.effect.laws.util.TestContext
import utest.TestSuite
import cats.effect.Temporal


abstract class DeterministicIOTestSuite extends TestSuite {
  protected val testContext: TestContext = TestContext()
  protected def allowNonIOTests: Boolean = false

  implicit def ioContextShift: ContextShift[IO] = testContext.contextShift(IO.ioEffect)
  implicit def ioTimer: Temporal[IO] = testContext.timer(IO.ioEffect)

  override def utestWrap(path: Seq[String], runBody: => Future[Any])(implicit ec: ExecutionContext): Future[Any] = {
    runBody.flatMap {
      case io: IO[Any] =>
        val f = io.unsafeToFuture()
        testContext.tick(365.days)
        assert(testContext.state.tasks.isEmpty)
        f.value match {
          case Some(_) => f
          case None => throw new RuntimeException(
            s"The IO in ${path.mkString(".")} did not terminate.\n" +
            "It's possible that you are using a ContextShift that is backed by other ExecutionContext or" +
            "the test code is waiting indefinitely."
          )
        }
      case other if allowNonIOTests => Future.successful(other)
      case other =>
        throw new RuntimeException(s"Test body must return an IO value. Got $other")
    }(new ExecutionContext {
      def execute(runnable: Runnable): Unit = runnable.run()
      def reportFailure(cause: Throwable): Unit = throw cause
    })
  }
}
