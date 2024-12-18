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

package cats.effect.testing.specs2

import cats.*
import cats.effect.testing.UnsafeRun
import cats.syntax.all.*
import org.specs2.execute.{AsResult, Result}

import scala.concurrent.{ExecutionContext, Future}

/**
 * `AsFutureResult` can be thought of an extension of `UnsafeRun[F].unsafeToFuture[A]` when
 * an `AsResult[A]` is available. This is necessary to support structures like
 * `org.scalacheck.effect.PropF[F]`, where it is not possible to write an instance of
 * `UnsafeRun[PropF]`, since `PropF#check` returns `Result` and
 * not `A`. (This doesn't work, even using e.g. `UnsafeRun[({ type λ[α] = PropF[F] })#λ]`.)
 *
 * This lets us abstract over different kinds of results, such as `A` (assuming an
 * `AsResult[A]`), `F[A]` (assuming an `UnsafeRun[F]` and `AsResult[A]`), or `PropF[F]`
 * (assuming an `UnsafeRun[F]`).
 */
trait AsFutureResult[T] {
  def asResult(t: => T): Future[Result]
}

object AsFutureResult extends LowPriorityAsFutureResultInstances {
  def apply[T: AsFutureResult]: AsFutureResult[T] = implicitly

  implicit def asResult[T: AsResult](implicit ec: ExecutionContext): AsFutureResult[T] =
    new AsFutureResult[T] {
      override def asResult(t: => T): Future[Result] =
        Future(AsResult[T](t))
    }

  implicit def effectualResult[F[_]: Functor: UnsafeRun, T: AsResult]: AsFutureResult[F[T]] =
    new AsFutureResult[F[T]] {
      override def asResult(t: => F[T]): Future[Result] =
        UnsafeRun[F].unsafeToFuture(t.map(AsResult[T](_)))
    }
}

sealed trait LowPriorityAsFutureResultInstances {
  implicit def effectualResultViaAsFutureResult[F[_]: UnsafeRun, T: AsFutureResult](implicit
                                                                                    ec: ExecutionContext
                                                                                   ): AsFutureResult[F[T]] = new AsFutureResult[F[T]] {
    override def asResult(t: => F[T]): Future[Result] =
      UnsafeRun[F].unsafeToFuture(t).flatMap(AsFutureResult[T].asResult(_))
  }

  implicit def effectualResultWithoutFunctor[F[_] : UnsafeRun, T: AsResult](implicit ec: ExecutionContext): AsFutureResult[F[T]] =
    new AsFutureResult[F[T]] {
      override def asResult(t: => F[T]): Future[Result] =
        UnsafeRun[F].unsafeToFuture(t).map(AsResult[T](_))
    }
}
