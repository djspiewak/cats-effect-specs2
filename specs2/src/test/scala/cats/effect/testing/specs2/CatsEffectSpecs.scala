/*
 * Copyright 2020-2021 Daniel Spiewak
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

import cats.effect.{IO, Ref, Resource}
import cats.implicits._
import org.specs2.mutable.Specification

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
      Resource.make(Ref[IO].of(true))(_.set(false)) evalMap { r =>
        r.get.map(_ must beTrue)
      }
    }

    "really execute effects" in {
      var gate = false

      "forcibly attempt to get the deferred value" in {
        IO.cede.untilM_(IO(gate)).as(ok)
      }

      "complete the deferred value inside IO context" in {
        (IO { gate = true }).as(ok)
      }
    }

    // "timeout a failing test" in (IO.never: IO[Boolean])
  }
}
