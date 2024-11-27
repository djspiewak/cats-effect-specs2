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

package cats.effect.testing.specs2

import cats.effect.{IO, Resource}
import cats.implicits._
import org.specs2.mutable.Specification
import cats.effect.{ Deferred, Ref }

class CatsEffectSpecs extends Specification with CatsEffect {

  "cats effect specifications" should {
    "run a non-effectful test" in {
      true must beTrue
    }

    "run a simple effectful test" in IO {
      true must beTrue
      false must beFalse
    }

    "run a simple resource test" in {
      true must beTrue
    }.pure[Resource[IO, *]]

    "resource must be live for use" in {
      Resource.make(Ref[IO].of(true))(_.set(false)).map{ 
        _.get.map(_ must beTrue)
      }
    }

    "really execute effects" in {
      "First, this check creates a deferred value.".br

      val deferredValue = Deferred.unsafeUncancelable[IO, Boolean]

      "Then it executes two mutually associated steps:".br.tab

      "forcibly attempt to get the deferred value" in {
        deferredValue.get.unsafeRunTimed(Timeout) must beSome(true)
      }

      "Since specs2 executes steps in parallel by default, the second step gets executed anyway.".br

      "complete the deferred value inside IO context" in {
        deferredValue.complete(true) *> IO.pure(success)
      }

      "If effects didn't get executed then the previous step would fail after timeout.".br
    }

    // "timeout a failing test" in (IO.never: IO[Boolean])
  }
}
