package tv.teads.step4next

import cats.{Foldable, Monoid, MonoidK, Semigroup}
import cats.instances.list._
import tv.teads.{Ad, Country, Device}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object Engine {

  type ExecutionResult[T] = Either[String, T]

  sealed trait Rule[T]

  trait SyncRule[T] extends Rule[T] {
    def apply(t: T): ExecutionResult[T]
  }

  object SyncRule {
    def apply[T](rule: T => ExecutionResult[T]): SyncRule[T] = (t: T) => rule(t)

    def semigroup[T]: Semigroup[SyncRule[T]] = new Semigroup[SyncRule[T]] {
      override def combine(left: SyncRule[T], right: SyncRule[T]): SyncRule[T] = (t: T) => {
        left(t).fold(error => Left(error), value => right(value))
      }
    }

    def combine[T](left: SyncRule[T], right: SyncRule[T]): SyncRule[T] = {
      semigroup.combine(left, right)
    }
  }

  trait AsyncRule[T] extends Rule[T] {
    def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]]
  }

  object AsyncRule {
    def apply[T](rule: T => Future[ExecutionResult[T]]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = rule(t)
    }

    def semigroup[T]: Semigroup[AsyncRule[T]] = new Semigroup[AsyncRule[T]] {
      override def combine(left: AsyncRule[T], right: AsyncRule[T]): AsyncRule[T] = new AsyncRule[T] {
        override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = left(t).flatMap {
          case Right(value) => right(value)
          case Left(error) => Future.successful(Left(error))
        }
      }
    }

    def combine[T](left: AsyncRule[T], right: AsyncRule[T]): AsyncRule[T] = {
      semigroup.combine(left, right)
    }

    def transform[T](left: SyncRule[T], right: AsyncRule[T]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = left(t).fold(
        error => Future.successful(Left(error)),
        value => right(value)
      )
    }
  }

  object Rule {

    implicit def monoid[T]: Monoid[Rule[T]] = new Monoid[Rule[T]] {
      override def empty: Rule[T] = SyncRule[T](t => Right(t))

      override def combine(x: Rule[T], y: Rule[T]): Rule[T] = (x, y) match {
        case (leftRule: SyncRule[T], rightRule: SyncRule[T]) => SyncRule.combine(leftRule, rightRule)
        case (syncRule: SyncRule[T], asyncRule: AsyncRule[T]) => AsyncRule.transform(syncRule, asyncRule)
        case (asyncRule: AsyncRule[T], syncRule: SyncRule[T]) => AsyncRule.transform(syncRule, asyncRule)
        case (leftRule: AsyncRule[T], rightRule: AsyncRule[T]) => AsyncRule.combine(leftRule, rightRule)
      }
    }

    implicit val monoidK: MonoidK[Rule] = new MonoidK[Rule] {
      override def empty[T]: Rule[T] = SyncRule[T](t => Right(t))

      override def combineK[T](x: Rule[T], y: Rule[T]): Rule[T] = (x, y) match {
        case (leftRule: SyncRule[T], rightRule: SyncRule[T]) => SyncRule.combine(leftRule, rightRule)
        case (syncRule: SyncRule[T], asyncRule: AsyncRule[T]) => AsyncRule.transform(syncRule, asyncRule)
        case (asyncRule: AsyncRule[T], syncRule: SyncRule[T]) => AsyncRule.transform(syncRule, asyncRule)
        case (leftRule: AsyncRule[T], rightRule: AsyncRule[T]) => AsyncRule.combine(leftRule, rightRule)
      }
    }

    def combine[T](left: Rule[T], right: Rule[T]): Rule[T] = monoidK.algebra[T].combine(left, right)

    def fold[T](rules: List[Rule[T]]): Rule[T] = {
      Foldable[List].foldK(rules)
    }

    def transformAll(rules: List[Rule[Ad]]): AsyncRule[Ad] = {
      fold[Ad](rules) match {
        case syncRule: SyncRule[Ad] => AsyncRule(ad => Future.successful(syncRule(ad)))
        case asyncRule: AsyncRule[Ad] => asyncRule
      }
    }

  }


  def deviceRule(device: Device): SyncRule[Ad] = {
    SyncRule(ad => Either.cond(ad.device == device, ad, "Device does not match"))
  }

  def countryRule(country: Country)(implicit ec: ExecutionContext): AsyncRule[Ad] = {
    AsyncRule(ad => Future(Either.cond(ad.country == country, ad, "Country does not match")))
  }

}
