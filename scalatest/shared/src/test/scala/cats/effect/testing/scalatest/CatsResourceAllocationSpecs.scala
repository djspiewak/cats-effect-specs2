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

package cats.effect.testing.scalatest

import cats.effect.{IO, Resource}
import org.scalatest.concurrent.Eventually
import org.scalatest.events.Event
import org.scalatest.{Args, Reporter}
import org.scalatest.matchers.must.Matchers._
import org.scalatest.wordspec.{AsyncWordSpec, FixtureAsyncWordSpec}

import scala.concurrent.duration._

class CatsResourceAllocationSpecs extends AsyncWordSpec with Eventually {

  override implicit def patienceConfig: PatienceConfig =
    super.patienceConfig.copy(timeout = 1.second)

  @volatile
  var beforeCalled: Int = 0

  @volatile
  var afterCalled: Int = 0

  class ResourceSpec
      extends FixtureAsyncWordSpec
      with AsyncIOSpec
      with CatsResourceIO[Unit] {

    override val resource: Resource[IO, Unit] =
      Resource.make { IO.delay { beforeCalled += 1 } } { _ =>
        IO.delay { afterCalled += 1 }
      }

    "test" should {
      "doFoo" in { _ => true mustBe true }
    }
  }

  val reporter: Reporter = (_: Event) => ()

  "cats resource allocation" should {
    "release the resource" in {

      val outerResourceSpec = new ResourceSpec

      outerResourceSpec.run(None, Args(reporter))

      eventually {
        beforeCalled mustBe 1
        afterCalled mustBe 1
      }
    }
  }
}
