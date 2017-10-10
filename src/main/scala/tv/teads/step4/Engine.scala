package tv.teads.step4

import tv.teads.{Ad, Device, Country}

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

    def compose[T](left: SyncRule[T], right: SyncRule[T]): SyncRule[T] = (t: T) => {
      left(t).fold(error => Left(error), value => right(value))
    }
  }

  trait AsyncRule[T] extends Rule[T] {
    def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]]
  }

  object AsyncRule {
    def apply[T](rule: T => Future[ExecutionResult[T]]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = rule(t)
    }

    def compose[T](left: AsyncRule[T], right: AsyncRule[T]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = left(t).flatMap {
        case Right(value) => right(value)
        case Left(error) => Future.successful(Left(error))
      }
    }

    def merge[T](left: SyncRule[T], right: AsyncRule[T]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = left(t).fold(
        error => Future.successful(Left(error)),
        value => right(value)
      )
    }
  }

  def combine[T](left: Rule[T], right: Rule[T]): Rule[T] = {
    (left, right) match {
      case (leftRule: SyncRule[T], rightRule: SyncRule[T]) => SyncRule.compose(leftRule, rightRule)
      case (syncRule: SyncRule[T], asyncRule: AsyncRule[T]) => AsyncRule.merge(syncRule, asyncRule)
      case (asyncRule: AsyncRule[T], syncRule: SyncRule[T]) => AsyncRule.merge(syncRule, asyncRule)
      case (leftRule: AsyncRule[T], rightRule: AsyncRule[T]) => AsyncRule.compose(leftRule, rightRule)
    }
  }

  def fold[T](rules: List[Rule[T]]): Rule[T] = {
    val firstRule: Rule[T] = SyncRule[T](t => Right(t))
    rules.foldLeft(firstRule) {
      case (acc, rule) => combine(acc, rule)
    }
  }

  def combineAll(rules: List[Rule[Ad]]): AsyncRule[Ad] = {
    fold[Ad](rules) match {
      case syncRule: SyncRule[Ad] => AsyncRule(ad => Future.successful(syncRule(ad)))
      case asyncRule: AsyncRule[Ad] => asyncRule
    }
  }

  def deviceRule(device: Device): SyncRule[Ad] = {
    SyncRule(ad => Either.cond(ad.device == device, ad, "Device does not match"))
  }

  def countryRule(country: Country)(implicit ec: ExecutionContext): AsyncRule[Ad] = {
    AsyncRule(ad => Future(Either.cond(ad.country == country, ad, "Country does not match")))
  }

}
