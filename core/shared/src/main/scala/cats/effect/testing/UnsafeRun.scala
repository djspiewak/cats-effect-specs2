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

package cats.effect
package testing

import scala.annotation.nowarn
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait UnsafeRun[F[_]] {
  def unsafeToFuture[A](fa: F[A]): Future[A]
  def unsafeToFuture[A](fa: F[A], @nowarn("msg=never used") timeout: Option[FiniteDuration]): Future[A]
    = unsafeToFuture(fa) // For binary compatibility
}

object UnsafeRun {

  def apply[F[_]](implicit F: UnsafeRun[F]): UnsafeRun[F] = F

  implicit object unsafeRunForCatsIO extends UnsafeRun[IO] {
    import unsafe.implicits.global

    override def unsafeToFuture[A](ioa: IO[A]): Future[A] =
      unsafeToFuture(ioa, None)

    // TODO is it worth isolating runtimes between test runs?
    override def unsafeToFuture[A](ioa: IO[A], timeout: Option[FiniteDuration]): Future[A] =
      timeout.fold(ioa)(ioa.timeout).unsafeToFuture()
  }

  implicit val unsafeRunScalaFuture: UnsafeRun[scala.concurrent.Future] = new UnsafeRun[scala.concurrent.Future] {
    override def unsafeToFuture[AA](fa: scala.concurrent.Future[AA]): scala.concurrent.Future[AA] = fa
  }
}
