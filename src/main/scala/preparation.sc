import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

type ExecutionResult = Either[String, Ad]
type Rule[T] = T => Future[ExecutionResult]

type Device = String

type Country = String

def deviceRule(device: Device): Rule[Ad] = {
  ad => Future.successful(Either.cond(ad.device == device, ad, "Device does not match"))
}
def countryRule(country: Country): Rule[Ad] = {
  ad => Future(Either.cond(ad.country == country, ad, "Country does not match"))
}

case class Ad(country: Country, device: Device)

val rules: List[Rule[Ad]] = List(
  countryRule("FR"),
  deviceRule("Mobile")
)

def canBroadcast(rules: List[Rule[Ad]]): Rule[Ad] = (ad) => {
  val checkingRules = Future.sequence(rules.map(rule => rule(ad)))
  checkingRules.map{results =>
    results.foldLeft(Right(ad): ExecutionResult) {
      case (Right(a), result) => result
      case (left, _) => left
    }
  }
}

Await.result(canBroadcast(rules)(Ad("FR", "Mobile")), atMost = 10.seconds)