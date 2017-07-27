package tv.teads.step3

import tv.teads.{Ad, Device, Country}
import scala.concurrent.Future

import scala.concurrent.ExecutionContext

object Engine {

  type ExecutionResult[T] = Either[String, T]
  type Rule[T] = T => Future[ExecutionResult[T]]

  object Rule{

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

}
