import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.{Foldable, Id, Monad, Monoid, MonoidK, Semigroup, SemigroupK}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.reflectiveCalls

type ExecutionResult[T] = Either[String, T]

//TODO 1
type Rule[Effect[_], T] = Kleisli[Effect, T, ExecutionResult[T]]
type SyncRule[T] = Rule[Id, T]
type AsyncRule[T] = Rule[Future, T]

object Rule {

  def apply[Effect[_], T](rule: T ⇒ Effect[ExecutionResult[T]]): Rule[Effect, T] =
    Kleisli[Effect, T, ExecutionResult[T]](rule)

  def sync[T](rule: T => ExecutionResult[T]): SyncRule[T] =
    Rule[Id, T](rule)

  def async[T](rule: T => Future[ExecutionResult[T]]): AsyncRule[T] =
    Rule[Future, T](rule)

  // TODO 4
  // TODO 6 SEMIGROUP
  // TODO 7 SEMIGROUPK

  def semigroupK[Effect[_] : Monad]: SemigroupK[({type L[A] = Rule[Effect, A]})#L] =
    new SemigroupK[({type L[A] = Rule[Effect, A]})#L] {
      val effectMonad = Monad[Effect]

      override def combineK[T](left: Rule[Effect, T], right: Rule[Effect, T]): Rule[Effect, T] = {
        Rule[Effect, T] { (t: T) =>
          effectMonad.flatMap(left(t)) {
            case Right(value) => right(value)
            case error@Left(reason) => effectMonad.pure(error)
          }
        }
      }
    }

  implicit def monoidK[Effect[_] : Monad]: MonoidK[({type L[A] = Rule[Effect, A]})#L] = new MonoidK[({type L[A] = Rule[Effect, A]})#L] {
    val effectMonad = Monad[Effect]

    override def combineK[T](left: Rule[Effect, T], right: Rule[Effect, T]): Rule[Effect, T] = {
      Rule[Effect, T] { (t: T) =>
        effectMonad.flatMap(left(t)) {
          case Right(value) => right(value)
          case error@Left(reason) => effectMonad.pure(error)
        }
      }
    }

    override def empty[T]: Rule[Effect, T] = {
      Rule[Effect, T]((t: T) => effectMonad.pure(Right(t)))
    }
  }

  def combine[Effect[_] : Monad, T](left: Rule[Effect, T], right: Rule[Effect, T]): Rule[Effect, T] = {
    val combiner: Semigroup[Rule[Effect, T]] = semigroupK[Effect].algebra[T]

    combiner.combine(left, right)
  }

  //TODO 5: coincé, passage à Kleisli
  // TODO 8 FUNCTION K + KLEISLI TRANSFORM
  def combineF[Effect1[_], Effect2[_] : Monad, T](left: Rule[Effect1, T], right: Rule[Effect2, T])(implicit effectTransformer: FunctionK[Effect1, Effect2]): Rule[Effect2, T] = {
    combine(left.transform(effectTransformer), right)
  }

}

implicit val idToFuture: FunctionK[Id, Future] = new FunctionK[Id, Future] {
  override def apply[A](fa: Id[A]): Future[A] = Future.successful(fa)
}

type Device = String

type Country = String

def deviceRule(device: Device): Rule[Id, Ad] = {
  Rule.sync(ad => Either.cond(ad.device == device, ad, "Device does not match"))
}
def countryRule(country: Country): Rule[Future, Ad] = {
  Rule.async(ad => Future.successful(Either.cond(ad.country == country, ad, "Country does not match")))
}

case class Ad(country: Country, device: Device)

import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._

val superRule = Rule.combine[Id, Ad](deviceRule("Mobile"), deviceRule("Desktop"))
val complexRule = Rule.combineF[Id, Future, Ad](deviceRule("Mobile"), countryRule("FR"))

deviceRule("Mobile")(Ad("FR", "Mobile"))
superRule(Ad("FR", "Mobile"))
Await.result(complexRule(Ad("FR", "Mobile")), atMost = 10.seconds)