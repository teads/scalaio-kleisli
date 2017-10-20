package tv.teads.step6bis

import cats.arrow.FunctionK
import cats.data.{Kleisli, Validated}
import cats.functor.Strong
import cats.kernel.Monoid
import cats.{Applicative, Apply, Id, Monad, Semigroup, SemigroupK, Traverse}
import tv.teads.{Ad, Country, Device}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.reflectiveCalls

object Engine {

  type ExecutionResult[T] = Validated[List[String], T]

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

    import cats.instances.list.catsStdInstancesForList

    Validated.cats
  }

  object Rule {

    def apply[Effect[_], T](rule: T ⇒ Effect[ExecutionResult[T]]): Rule[Effect, T] = {
      Kleisli[Effect, T, ExecutionResult[T]](rule)
    }

    //    def combine[Effect[_] : Monad, T](left: Rule[Effect, T], right: Rule[Effect, T]): Rule[Effect, T] = {
    //      monoid[Effect, T].combine(left, right)
    //    }
    //
    //    def combineEffect[Effect1[_], Effect2[_] : Monad, T](left: Rule[Effect1, T], right: Rule[Effect2, T])(implicit effectTransformer: FunctionK[Effect1, Effect2]): Rule[Effect2, T] = {
    //      combine(transform(left), right)
    //    }
    //
    //    def transform[Effect1[_], Effect2[_], T](rule: Rule[Effect1, T])(implicit effectTransformer: FunctionK[Effect1, Effect2]): Rule[Effect2, T] = {
    //      rule.transform(effectTransformer)
    //    }

    def fold[Effect[_] : Monad, T](rules: List[Rule[Effect, T]]): Rule[Effect, T] = {
      import cats.instances.list.catsStdInstancesForList

      val traverse: Traverse[({type λ[β$4$] = Validated[List[String], β$4$]})#λ] =
        Validated.catsDataTraverseFunctorForValidated[List[String]]

      Kleisli(t => Traverse[List].foldMapM(rules)(rule => rule.run(t)))
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
        Validated.valid(ad)
      else
        Validated.invalid(List("Device does not match"))
    }
  }

  def countryRule(country: Country)(implicit ec: ExecutionContext): AsyncRule[Ad] = {
    AsyncRule { ad =>
      Future {
        if (ad.country == country)
          Validated.valid(ad)
        else
          Validated.invalid(List("Country does not match"))
      }
    }
  }


}
