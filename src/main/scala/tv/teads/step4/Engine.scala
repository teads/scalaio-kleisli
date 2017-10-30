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

    def combine[T](left: SyncRule[T], right: SyncRule[T]): SyncRule[T] = (t: T) => {
      left(t).flatMap(value => right(value))
    }
  }

  trait AsyncRule[T] extends Rule[T] {
    def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]]
  }

  object AsyncRule {
    def apply[T](rule: T => Future[ExecutionResult[T]]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = rule(t)
    }

    def combine[T](left: AsyncRule[T], right: AsyncRule[T]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = left(t).flatMap {
        case Right(value) => right(value)
        case Left(error) => Future.successful(Left(error))
      }
    }

  }

  object Rule{
    def transform[T](left: SyncRule[T], right: AsyncRule[T]): AsyncRule[T] = new AsyncRule[T] {
      override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = left(t).fold(
        error => Future.successful(Left(error)),
        value => right(value)
      )
    }

    def combine[T](left: Rule[T], right: Rule[T]): Rule[T] = {
      (left, right) match {
        case (leftRule: SyncRule[T], rightRule: SyncRule[T]) => SyncRule.combine(leftRule, rightRule)
        case (syncRule: SyncRule[T], asyncRule: AsyncRule[T]) => Rule.transform(syncRule, asyncRule)
        case (asyncRule: AsyncRule[T], syncRule: SyncRule[T]) => Rule.transform(syncRule, asyncRule)
        case (leftRule: AsyncRule[T], rightRule: AsyncRule[T]) => AsyncRule.combine(leftRule, rightRule)
      }
    }

    def fold[T](rules: List[Rule[T]]): Rule[T] = {
      val firstRule: Rule[T] = SyncRule[T](t => Right(t))
      rules.foldLeft(firstRule) {
        case (acc, rule) => combine(acc, rule)
      }
    }

    def transformAll[T](rules: List[Rule[T]]): AsyncRule[T] = {
      fold[T](rules) match {
        case syncRule: SyncRule[T] => AsyncRule(ad => Future.successful(syncRule(ad)))
        case asyncRule: AsyncRule[T] => asyncRule
      }
    }

    def run[T](rules: List[Rule[T]], t: T)(implicit executionContext: ExecutionContext): Future[ExecutionResult[T]] = {
      transformAll(rules)(t)
    }

    def foldSync[T](rules: List[SyncRule[T]]): SyncRule[T] = {
      val firstRule: SyncRule[T] = SyncRule[T](t => Right(t))
      rules.foldLeft(firstRule) {
        case (acc, rule) => SyncRule.combine(acc, rule)
      }
    }

    def runSync[T](rules: List[SyncRule[T]], t: T): ExecutionResult[T] = {
      foldSync(rules)(t)
    }
  }

  def deviceRule(device: Device): SyncRule[Ad] = {
    SyncRule(ad => Either.cond(ad.device == device, ad, "Device does not match"))
  }

  def countryRule(country: Country)(implicit ec: ExecutionContext): AsyncRule[Ad] = {
    AsyncRule(ad => Future(Either.cond(ad.country == country, ad, "Country does not match")))
  }

}
