package tv.teads.step6bis

import cats.arrow.FunctionK
import cats.data.{Validated, _}
import cats.kernel.Monoid
import cats.{Applicative, Apply, Eval, Id, Monad, MonoidK, Semigroup, SemigroupK, Traverse}
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

    // IMPOSSIBLE CAR EMPTY[A] NE PEUT PAS EXISTER
    /*val monoidK: MonoidK[({type L[A] = ValidatedNel[String,A]})#L] = new MonoidK[({type L[A] = ValidatedNel[String, A]})#L] {
      override def empty[A]: ValidatedNel[String, A] = Validated.validNel()

      override def combineK[A](x: ValidatedNel[String, A], y: ValidatedNel[String, A]): ValidatedNel[String, A] = ???
    }*/

    val monad: Applicative[({type L[A] = ValidatedNel[String, A]})#L] = new Monad[({type L[A] = ValidatedNel[String, A]})#L] {

      override def flatMap[A, B](fa: ExecutionResult[A])(f: (A) => ExecutionResult[B]): ExecutionResult[B] = fa match {
        case Validated.Valid(a) => f(a)
        case invalid@Validated.Invalid(reasons) => invalid.asInstanceOf[ExecutionResult[B]]
      }

      override def tailRecM[A, B](a: A)(f: (A) => ExecutionResult[Either[A, B]]): ExecutionResult[B] = {
        f(a) match {
          case Validated.Valid(Right(b)) => Validated.Valid(b)
          case Validated.Valid(Left(aa)) => tailRecM(aa)(f)
          case invalid@Validated.Invalid(reasons) => invalid.asInstanceOf[ExecutionResult[B]]
        }
      }

      override def pure[A](a: A): ExecutionResult[A] = {
        Validated.validNel(a)
      }
    }
  }

  object Rule {

    def apply[Effect[_], T](rule: T ⇒ Effect[ExecutionResult[T]]): Rule[Effect, T] = {
      Kleisli[Effect, T, ExecutionResult[T]](rule)
    }

    def transform[Effect1[_], Effect2[_], T](rule: Rule[Effect1, T])(implicit effectTransformer: FunctionK[Effect1, Effect2]): Rule[Effect2, T] = {
      rule.transform(effectTransformer)
    }

    def fold[Effect[_] : Applicative, T](rules: List[Rule[Effect, T]]): Rule[Effect, T] = {
      import cats.instances.list.catsStdInstancesForList

      val applicative: Applicative[({type L[A] = Effect[ValidatedNel[String, A]]})#L] =
        Applicative[Effect].compose[({type L[A] = ValidatedNel[String, A]})#L](ExecutionResult.monad)

      Rule[Effect, T] { t =>
        applicative.map(applicative.traverse(rules)(rule => rule.run(t)))(as => as.head)
      }

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
