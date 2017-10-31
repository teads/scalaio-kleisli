import tv.teads._

type ExecutionResult[T] = Either[String, T]
type Rule[T] = T => ExecutionResult[T]

object Rule {

  def run[T](rules: List[Rule[T]], t: T): ExecutionResult[T] = {
    rules.foldLeft(Right(t): ExecutionResult[T]) {
      case (Right(_), rule) => rule(t)
      case (left, _) => left
    }
  }

}

def deviceRule(device: Device): Rule[Ad] = {
  ad => Either.cond(ad.device == device, ad, "Device does not match")
}

def countryRule(country: Country): Rule[Ad] = {
  ad => Either.cond(ad.country == country, ad, "Country does not match")
}


val targeting = List(
  deviceRule("Mobile"),
  countryRule("FR")
)

val ad = Ad("FR", "Mobile")


Rule.run(targeting, ad)