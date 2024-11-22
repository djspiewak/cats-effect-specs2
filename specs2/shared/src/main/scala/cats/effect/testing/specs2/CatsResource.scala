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

package cats.effect.testing
package specs2

import cats.effect._
import cats.effect.syntax.all._
import cats.syntax.all._
import org.specs2.specification.BeforeAfterAll

import scala.concurrent.duration._

abstract class CatsResource[F[_]: Async: UnsafeRun, A] extends BeforeAfterAll with CatsEffect {

  val resource: Resource[F, A]

  protected val ResourceTimeout: Duration = 10.seconds
  protected def finiteResourceTimeout: Option[FiniteDuration] =
    Some(ResourceTimeout) collect {
      case fd: FiniteDuration => fd
    }

  // we use the gate to prevent further step execution
  // this isn't *ideal* because we'd really like to block the specs from even starting
  // but it does work on scalajs
  @volatile
  private var gate: Option[Deferred[F, (Either[Throwable, A], F[Unit])]] = None

  override def beforeAll(): Unit = {
    val toRun = for {
      d <- Deferred[F, (Either[Throwable, A], F[Unit])]
      _ <- Sync[F] delay {
        gate = Some(d)
      }

      pair <- resource.attempt.allocated

      _ <- d.complete(pair)
    } yield ()

    UnsafeRun[F].unsafeToFuture(toRun, finiteResourceTimeout)
    ()
  }

  override def afterAll(): Unit =
    gate
      .map(_.get._2F)
      .map {
        finiteResourceTimeout.foldl(_)(_.timeout(_))
          .flatten
          .guarantee(Sync[F].delay {
            gate = None
          })
      }
      .foreach(UnsafeRun[F].unsafeToFuture(_, finiteResourceTimeout))

  def withResource[R](f: A => F[R]): F[R] =
    gate match {
      case Some(g) =>
        finiteResourceTimeout
          .foldl(g.get._1F)(_.timeout(_))
          .rethrow
          .flatMap(f)

      // specs2's runtime should prevent this case
      case None =>
        Spawn[F].cede >> withResource(f)
    }
}
