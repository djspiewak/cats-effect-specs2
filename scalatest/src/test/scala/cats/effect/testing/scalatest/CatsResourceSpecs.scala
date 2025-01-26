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

package cats.effect.testing.scalatest

import cats.effect._
import org.scalatest.matchers.must.Matchers._
import org.scalatest.wordspec.FixtureAsyncWordSpec
import cats.effect.Ref

class CatsResourceSpecs extends FixtureAsyncWordSpec with AsyncIOSpec with CatsResourceIO[Ref[IO, Int]] {

  override val resource: Resource[IO, Ref[IO, Int]] =
    Resource.make(Ref[IO].of(0))(_.set(Int.MinValue))

  "cats resource specifications" should {
    "run a resource modification" in { ref =>
      ref
        .modify { a =>
          (a + 1, a)
        }
        .map(
          _ must be(0)
        )
    }

    "be shared between tests" in { ref =>
      ref
        .modify { a =>
          (a + 1, a)
        }
        .map(
          _ must be(1)
        )
    }
  }
}
