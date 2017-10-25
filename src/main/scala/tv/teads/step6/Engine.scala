package tv.teads.step6

import cats.arrow.FunctionK
import cats.data._
import cats.kernel.Monoid
import cats.{Applicative, Apply, Id, Monad, Semigroup, SemigroupK, Traverse}
import tv.teads.{Ad, Country, Device}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.reflectiveCalls

object Engine {

  type ExecutionResult[T] = ValidatedNel[String, T]

  type Rule[Effect[_], T] = Kleisli[Effect, T, ExecutionResult[T]]
  type SyncRule[T] = Rule[Id, T]
  type AsyncRule[T] = Rule[Future, T]

  object SyncRule {
    def apply[T](rule: T => ExecutionResult[T]): SyncRule[T] =
      Rule[Id, T](rule)
  }

  object AsyncRule {
    def apply[T](rule: T => Future[ExecutionResult[T]]): AsyncRule[T] =
      Rule[Future, T](rule)
  }

  object ExecutionResult {

    implicit val semigroupK: SemigroupK[({type L[A] = ValidatedNel[String, A]})#L] =
      Validated.catsDataSemigroupKForValidated(NonEmptyList.catsDataSemigroupForNonEmptyList)

    implicit def semigroup[T]: Semigroup[ValidatedNel[String, T]] =
      semigroupK.algebra[T]

  }

  object Rule {

    def apply[Effect[_], T](rule: T ⇒ Effect[ExecutionResult[T]]): Rule[Effect, T] = {
      Kleisli[Effect, T, ExecutionResult[T]](rule)
    }

    implicit def monoid[Effect[_] : Apply : Applicative, T]: Monoid[Rule[Effect, T]] = {
      import ExecutionResult.semigroup

      val resultWithEffectSemigroup: Semigroup[Effect[ExecutionResult[T]]] =
        Apply.semigroup[Effect, ExecutionResult[T]]

      val ruleSemigroup: Semigroup[Rule[Effect, T]] =
        Kleisli.catsDataSemigroupForKleisli(resultWithEffectSemigroup)

      new Monoid[Rule[Effect, T]] {
        override def empty: Rule[Effect, T] = {
          Kleisli(t => Applicative[Effect].pure(Validated.validNel[String, T](t)))
        }

        override def combine(x: Rule[Effect, T], y: Rule[Effect, T]): Rule[Effect, T] = {
          ruleSemigroup.combine(x, y)
        }
      }
    }

    def combine[Effect[_] : Monad, T](left: Rule[Effect, T], right: Rule[Effect, T]): Rule[Effect, T] = {
      monoid[Effect, T].combine(left, right)
    }

    def combineEffect[Effect1[_], Effect2[_] : Monad, T](left: Rule[Effect1, T], right: Rule[Effect2, T])(implicit effectTransformer: FunctionK[Effect1, Effect2]): Rule[Effect2, T] = {
      combine(transform(left), right)
    }

    def transform[Effect1[_], Effect2[_], T](rule: Rule[Effect1, T])(implicit effectTransformer: FunctionK[Effect1, Effect2]): Rule[Effect2, T] = {
      rule.transform(effectTransformer)
    }

    def fold[Effect[_] : Apply : Applicative, T](rules: List[Rule[Effect, T]]): Rule[Effect, T] = {
      import cats.instances.list.catsStdInstancesForList

      Traverse[List].fold(rules)
    }
  }

  object Transformer {
    implicit val idToFuture: FunctionK[Id, Future] = new FunctionK[Id, Future] {
      override def apply[A](fa: Id[A]): Future[A] = Future.successful(fa)
    }
  }

  def deviceRule(device: Device): SyncRule[Ad] = {
    SyncRule { ad =>
      if (ad.device == device)
        Validated.validNel(ad)
      else
        Validated.invalidNel("Device does not match")
    }
  }

  def countryRule(country: Country)(implicit ec: ExecutionContext): AsyncRule[Ad] = {
    AsyncRule { ad =>
      Future {
        if (ad.country == country)
          Validated.validNel(ad)
        else
          Validated.invalidNel("Country does not match")
      }
    }
  }


}
