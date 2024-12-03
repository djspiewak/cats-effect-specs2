/*
 * Copyright 2020 Typelevel
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

import cats.syntax.all._
import cats.effect.IO
import utest.{Tests, assert, test}
import scala.concurrent.duration._

object TestNondetSuite extends EffectTestSuite[IO] {

  override val timeout: FiniteDuration = 2.seconds
  override val allowNonIOTests: Boolean = true

  val tests = Tests {
    test("IO values should work") {
      IO(true).flatMap(b => IO(assert(b)))
    }

    test("Timer and ContextShift should be available for respective operations") {
      IO.sleep(1.second) >> IO(assert(true)).timeout(1.second)
    }

    test("Non-IO values should be OK if allowNonIOTests is overriden") {
      assert(1 == 1)
    }
  }
}
