import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.{MonoidK, _}
import tv.teads._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import cats.instances.list._

import scala.language.reflectiveCalls

type ExecutionResult[T] = Either[String, T]
type Rule[Effect[_], T] = Kleisli[Effect, T, ExecutionResult[T]]

type SyncRule[T] = Rule[Id, T]

object SyncRule {
  def apply[T](rule: T => ExecutionResult[T]): SyncRule[T] = {
    Kleisli[Id, T, ExecutionResult[T]](rule)
  }
}

type AsyncRule[T] = Rule[Future, T]

object AsyncRule {
  def apply[T](rule: T => Future[ExecutionResult[T]]): AsyncRule[T] = {
    Kleisli[Future, T, ExecutionResult[T]](rule)
  }
}

object Rule {

  implicit def monoidK[Effect[_] : Monad]: MonoidK[({type L[A] = Rule[Effect, A]})#L] = new MonoidK[({type L[A] = Rule[Effect, A]})#L] {
    val effectApplicative = Applicative[Effect]

    override def empty[A]: Rule[Effect, A] = Kleisli[Effect, A, ExecutionResult[A]](a =>
      effectApplicative.pure(Right(a))
    )

    override def combineK[A](x: Rule[Effect, A], y: Rule[Effect, A]): Rule[Effect, A] = {
      x.flatMap[ExecutionResult[A]] {
        case Right(value) => y
        case error => Kleisli.pure(error)
      }
    }
  }

  implicit def monoid[Effect[_] : Monad, T]: Monoid[Rule[Effect, T]] = {
    monoidK[Effect].algebra[T]
  }

  def combine[Effect[_] : Monad, T](left: Rule[Effect, T], right: Rule[Effect, T]): Rule[Effect, T] = {
    monoidK[Effect].algebra[T].combine(left, right)
  }

  def fold[Effect[_] : Monad, T](rules: List[Rule[Effect, T]]): Rule[Effect, T] = {
    Traverse[List].fold(rules)
  }

  def transform[Effect1[_], Effect2[_] : Monad, T](left: Rule[Effect1, T], right: Rule[Effect2, T])(implicit transformer: FunctionK[Effect1, Effect2]): Rule[Effect2, T] = {
    combine(left.transform(transformer), right)
  }

  def run[Effect[_] : Monad, T](rules: List[Rule[Effect, T]], t: T): Effect[ExecutionResult[T]] = {
    fold(rules).run(t)
  }

}

object Transformation {
  implicit val idToFuture: FunctionK[Id, Future] = {
    new FunctionK[Id, Future] {
      override def apply[A](fa: Id[A]): Future[A] = {
        Future.successful(fa)
      }
    }
  }
}

def deviceRule(device: Device): SyncRule[Ad] = {
  SyncRule(ad => Either.cond(ad.device == device, ad, "Device does not match"))
}

def countryRule(country: Country)(implicit ec: ExecutionContext): AsyncRule[Ad] = {
  AsyncRule(ad => Future(Either.cond(ad.country == country, ad, "Country does not match")))
}

val ec = ExecutionContext.global
implicit val futureMonad = cats.instances.future.catsStdInstancesForFuture(ec)

import Transformation._

Await.result(Rule.run(List(Rule.transform(deviceRule("Desktop"), countryRule("FR")(ec))), Ad("FR", "Desktop")), atMost = 1.second)