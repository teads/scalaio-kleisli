import tv.teads._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

type ExecutionResult[T] = Either[String, T]
type Rule[T] = T => Future[ExecutionResult[T]]

object Rule {

  def run[T](rules: List[Rule[T]], t: T)(implicit ec: ExecutionContext): Future[ExecutionResult[T]] = {
    val checkingRules = Future.sequence(rules.map(rule => rule(t)))
    checkingRules.map { results =>
      results.foldLeft(Right(t): ExecutionResult[T]) {
        case (Right(a), result) => result
        case (left, _) => left
      }
    }
  }


}

def deviceRule(device: Device): Rule[Ad] = {
  ad => Future.successful(Either.cond(ad.device == device, ad, "Device does not match"))
}

def countryRule(country: Country)(implicit ec: ExecutionContext): Rule[Ad] = {
  ad => Future(Either.cond(ad.country == country, ad, "Country does not match"))
}

implicit val ec = concurrent.ExecutionContext.global

val targeting = List(
  deviceRule("Mobile"),
  countryRule("FR")
)

val ad = Ad("FR", "Mobile")


Await.result(Rule.run(targeting, ad), Duration.Inf)