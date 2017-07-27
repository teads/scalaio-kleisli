package tv.teads.step1

import tv.teads.{Ad, Device, Country}

object Engine {

  type ExecutionResult = Boolean
  type Rule[T] = T => ExecutionResult

  object Rule {

    def run[T](rules: List[Rule[T]], t: T): ExecutionResult = {
      rules.forall(rule => rule(t))
    }

  }

  def deviceRule(device: Device): Rule[Ad] = {
    ad => ad.device == device
  }

  def countryRule(country: Country): Rule[Ad] = {
    ad => ad.country == country
  }

}
