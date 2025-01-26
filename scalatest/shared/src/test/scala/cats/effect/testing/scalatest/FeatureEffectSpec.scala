/*
 * Copyright 2020-2021 Typelevel
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

import cats.effect.IO
import org.scalatest.featurespec.AsyncFeatureSpec
import org.scalatest.matchers.should.Matchers

class FeatureEffectSpec extends AsyncFeatureSpec with AsyncIOSpec with GivenWhenThen[IO] with Matchers {

  Feature("GivenWhenThen Syntax") {
    Scenario("For comprehension") {
        for {
          _ <- Given("a precondition")
          _ <- IO.println("test setup")
          _ <- When("an action happens")
          result <- IO.pure(1)
          _ <- Then("assert something")
          _ <- IO(result should be > 0)
          _ <- And("assert something else")
          _ <- IO(result should be < 10)
        } yield ()
    }

    Scenario("Individual statements") {
      val precondition = Given("a precondition") >> IO.println("test setup")
      val action = When("an action happens") >> IO.pure(1)

      def assertions(result: Int) =
        Then("assert something") >> IO(result should be > 0) >>
          And("assert something else") >> IO(result should be < 10)

      precondition >> action.flatMap(assertions)
    }
  }

}
