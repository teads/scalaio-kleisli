package tv.teads.step3

import tv.teads.{Ad, Device, Country}

import scala.concurrent.ExecutionContext

object Engine {

  import scala.concurrent.Future

  type ExecutionResult = Either[String, Ad]
  type Rule[T] = T => Future[ExecutionResult]

  def deviceRule(device: Device)(implicit ec: ExecutionContext): Rule[Ad] = {
    ad => Future.successful(Either.cond(ad.device == device, ad, "Device does not match"))
  }

  def countryRule(country: Country)(implicit ec: ExecutionContext): Rule[Ad] = {
    ad => Future(Either.cond(ad.country == country, ad, "Country does not match"))
  }


  def canBroadcast(rules: List[Rule[Ad]])(implicit ec: ExecutionContext): Rule[Ad] = (ad) => {
    val checkingRules = Future.sequence(rules.map(rule => rule(ad)))
    checkingRules.map { results =>
      results.foldLeft(Right(ad): ExecutionResult) {
        case (Right(a), result) => result
        case (left, _) => left
      }
    }
  }
}
