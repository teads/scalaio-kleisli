type Rule[T] = T => Boolean

type Device = String

type Country = String

def deviceRule(device: Device): Rule[Ad] = {
  ad => ad.device == device
}
def countryRule(country: Country): Rule[Ad] = {
  ad => ad.country == country
}

case class Ad(country: Country, device: Device)

val rules: List[Rule[Ad]] = List(
  countryRule("FR"),
  deviceRule("Mobile")
)

def canBroadcast(rules: List[Rule[Ad]])(ad: Ad): Boolean = {
  rules.foldLeft(true) {
    case (acc, rule) => acc && rule(ad)
  }
}

canBroadcast(rules)(Ad("FR", "Mobile"))