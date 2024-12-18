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

import cats.effect.testing.specs2.*
import cats.effect.{IO, Resource}
import org.specs2.execute.Result
import org.specs2.mutable.SpecificationLike

case class BlowUpResourceException() extends RuntimeException("boom")

class CatsResourceErrorSpecs
  extends CatsResource[IO, Unit]
    with SpecificationLike {

  private val expectedException = BlowUpResourceException()

  val resource: Resource[IO, Unit] =
    Resource.eval(IO.raiseError(expectedException))

  "cats resource support" should {
    "report failure when the resource acquisition fails" in withResource { (_: Unit) =>
      IO(failure("we shouldn't get here if an exception was raised"))
    }
      .recover[Result] {
        case ex: RuntimeException =>
          ex must beEqualTo(expectedException)
      }
  }
}
