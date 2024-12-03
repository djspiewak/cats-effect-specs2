# cats-effect-testing

A quickie little utility which makes it easier to write tests using [specs2](https://specs2.org) (mutable or functional), [scalatest](http://scalatest.org), [µTest](https://github.com/lihaoyi/utest) or [minitest](https://github.com/monix/minitest) where the examples are effectful within `cats.effect.IO`.

## Specs2

```scala
import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

// for some reason, only class works here; object will not be detected by sbt
class ExampleSpec extends Specification with CatsEffect {
  "examples" should {
    "do the things" in IO {
      true must beTrue
    }
  }
}
```

The above compiles and runs exactly as you would expect.

By default, tests run with a 10 second timeout. If you wish to override this, simply override the inherited `Timeout` val:

```scala
override val Timeout = 5.seconds
```

If you need an `ExecutionContext`, one is available in the `executionContext` val.

And if you need to share Cats Effect's Resource between test examples you can extend `CatsResource`, like so:

```scala
import cats.effect.{IO, Ref, Resource}
import org.specs2.mutable.SpecificationLike

class CatsResourceSpecs extends CatsResource[IO, Ref[IO, Int]] with SpecificationLike {
  sequential

  val resource: Resource[IO, Ref[IO, Int]] =
    Resource.make(Ref[IO].of(0))(_.set(Int.MinValue))

  "cats resource support" should {
    "run a resource modification" in withResource { ref =>
      ref.modify{ a =>
        (a + 1, a)
      }.map(
        _ must_=== 0
      )
    }

    "be shared between tests" in withResource { ref =>
      ref.modify{ a =>
        (a + 1, a)
      }.map(
        _ must_=== 1
      )
    }
  }
}
```

The Resource is acquired before the tests are run and released afterwards. A more realistic example would be to share a Resource that takes a long time to start up.

### Usage

```sbt
libraryDependencies += "org.typelevel" %% "cats-effect-testing-specs2" % "<version>" % Test
```

Published for Scala 3.1+, 2.13, 2.12, as well as ScalaJS 1.7+. Depends on Cats Effect 3.1+ and Specs2 4.13.x. Specs2 5.0 is not yet supported.

Early versions (`0.x.y`) were published under the `com.codecommit` groupId.

## ScalaTest

```scala
import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AsyncFreeSpec

class MySpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  "My Code " - {
    "works" in {
      IO(1).asserting(_ shouldBe 1)
    }
}

```
### Usage

```sbt
libraryDependencies += "org.typelevel" %% "cats-effect-testing-scalatest" % "<version>" % Test
```

Published for Scala 3.1+, 2.13, 2.12, as well as ScalaJS 1.7.x. Depends on Cats Effect 3.1+ and scalatest 3.2.6.

Early versions (`0.x.y`) were published under the `com.codecommit` groupId.

## µTest

```scala
import scala.concurrent.duration._
import utest._
import cats.syntax.all._
import cats.effect.IO
import cats.effect.testing.utest.IOTestSuite

object SimpleSuite extends IOTestSuite {
  override val timeout = 1.second // Default timeout is 10 seconds

  val tests = Tests {
    test("do the thing") {
      IO(assert(true))
    }
  }
}

```

### Usage

```sbt
libraryDependencies += "org.typelevel" %% "cats-effect-testing-utest" % "<version>" % Test
```

Published for Scala 3.1+, 2.13, 2.12, as well as ScalaJS 1.7.x. Depends on Cats Effect 3.1+ and µTest 0.7.9.

Early versions (`0.x.y`) were published under the `com.codecommit` groupId.

## Minitest

Minitest is very similar to uTest, but being strongly typed, there's no need to support
non-IO tests

```scala
import scala.concurrent.duration._
import cats.syntax.all._
import cats.effect.IO
import cats.effect.testing.minitest.IOTestSuite

object SimpleSuite extends IOTestSuite {
  override val timeout = 1.second // Default timeout is 10 seconds

  test("do the thing") {
    IO(assert(true))
  }
}
```

### Usage

```sbt
libraryDependencies += "org.typelevel" %% "cats-effect-testing-minitest" % "<version>" % Test
```

Published for Scala 3.1+, 2.13, 2.12, as well as ScalaJS 1.7.x. Depends on Cats Effect 3.1+ and minitest 2.9.5.

Early versions (`0.x.y`) were published under the `com.codecommit` groupId.

## Similar projects

### scalacheck-effect

[scalacheck-effect](https://github.com/typelevel/scalacheck-effect) is a library that extends the functionality of [ScalaCheck](https://scalacheck.org) to support "effectful" properties. An effectful property is one that evaluates each sample in some type constructor `F[_]`.
