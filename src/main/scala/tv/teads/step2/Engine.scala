package tv.teads.step2

import tv.teads.{Ad, Device, Country}

object Engine {

  type ExecutionResult = Either[String, Ad]
  type Rule[T] = T => ExecutionResult

  def deviceRule(device: Device): Rule[Ad] = {
    ad => Either.cond(ad.device == device, ad, "Device does not match")
  }

  def countryRule(country: Country): Rule[Ad] = {
    ad => Either.cond(ad.country == country, ad, "Country does not match")
  }


  def canBroadcast(rules: List[Rule[Ad]]): Rule[Ad] = (ad) => {
    rules.foldLeft(Right(ad): ExecutionResult) {
      case (Right(_), rule) => rule(ad)
      case (left, _) => left
    }
  }
}
