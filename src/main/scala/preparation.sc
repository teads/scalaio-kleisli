type Rule[T] = T => Either[String, Ad]

type Device = String

type Country = String

def deviceRule(device: Device): Rule[Ad] = {
  ad => Either.cond(ad.device == device, ad, "Device does not match")
}
def countryRule(country: Country): Rule[Ad] = {
  ad => Either.cond(ad.country == country, ad, "Country does not match")
}

case class Ad(country: Country, device: Device)

val rules: List[Rule[Ad]] = List(
  countryRule("FR"),
  deviceRule("Mobile")
)

def canBroadcast(rules: List[Rule[Ad]]): Rule[Ad] = (ad) => {
  rules.foldLeft(Right(ad): Either[String, Ad]) {
    case (Right(a), rule) => rule.apply(a)
    case (left, _) => left
  }
}

canBroadcast(rules)(Ad("FR", "Desktop"))