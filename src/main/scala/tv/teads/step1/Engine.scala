package tv.teads.step1

import tv.teads.{Ad, Device, Country}

object Engine {

  type ExecutionResult = Boolean
  type Rule[T] = T => ExecutionResult

  def deviceRule(device: Device): Rule[Ad] = {
    ad => ad.device == device
  }

  def countryRule(country: Country): Rule[Ad] = {
    ad => ad.country == country
  }


  def canBroadcast(rules: List[Rule[Ad]]): Rule[Ad] = (ad) => {
    rules.foldLeft(true) {
      case (true, rule) => rule(ad)
      case _ => false
    }
  }
}
