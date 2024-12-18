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

package tests

import cats.effect.*
import cats.effect.testing.specs2.*
import org.specs2.mutable.SpecificationLike

import scala.concurrent.duration.*

class CatsResourceParallelSpecs extends CatsResource[IO, Unit] with SpecificationLike {
  // *not* sequential

  val resource = Resource.eval(IO.sleep(500.millis))

  "cats resource parallel test support" should {
    "await the resource availability" in withResource(_ => IO.pure(ok))
  }
}
