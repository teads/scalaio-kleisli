import cats._
import cats.instances.list._
import tv.teads._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

type ExecutionResult[T] = Either[String, T]

sealed trait Rule[T]

trait SyncRule[T] extends Rule[T] {
  def apply(t: T): ExecutionResult[T]
}

object SyncRule {
  def apply[T](rule: T => ExecutionResult[T]): SyncRule[T] = (t: T) => rule(t)

  implicit val monoidK: MonoidK[SyncRule] =
    new MonoidK[SyncRule] {
      override def combineK[T](left: SyncRule[T], right: SyncRule[T]): SyncRule[T] = (t: T) => {
        left(t).fold(error => Left(error), value => right(value))
      }

      override def empty[A]: SyncRule[A] = {
        SyncRule(t => Right(t))
      }
    }

  implicit def monoid[T] = monoidK.algebra[T]

  def combine[T](left: SyncRule[T], right: SyncRule[T]): SyncRule[T] = {
    monoidK.combineK(left, right)
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
  def transform[T](left: SyncRule[T], right: AsyncRule[T]): AsyncRule[T] = new AsyncRule[T] {
    override def apply(t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = left(t).fold(
      error => Future.successful(Left(error)),
      value => right(value)
    )
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

  def combine[T](left: Rule[T], right: Rule[T]): Rule[T] = {
    monoidK.combineK(left, right)
  }


  def fold[T](rules: List[Rule[T]]): Rule[T] = {
    Traverse[List].foldK(rules)
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
    Foldable[List].foldK(rules)
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

implicit val ec = concurrent.ExecutionContext.global

val targeting = List(
  deviceRule("Mobile"),
  countryRule("FR")
)

val ad = Ad("FR", "Mobile")

Await.result(Rule.run(targeting, ad), Duration.Inf)

import cats.syntax.semigroupk._
import cats.syntax.semigroup._

deviceRule("Mobile") combineK deviceRule("Mobile")
deviceRule("Mobile") <+> deviceRule("Mobile")

deviceRule("Mobile") |+| deviceRule("Mobile")

Rule.runSync(List(deviceRule("Mobile")), ad)