import tv.teads._

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


val targeting = List(
  deviceRule("Mobile"),
  countryRule("FR")
)

val ad = Ad("FR", "Mobile")


Rule.run(targeting, ad)