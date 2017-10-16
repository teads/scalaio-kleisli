import tv.teads.Ad

import scala.language.reflectiveCalls
import tv.teads.step6.Engine

val ad = Ad("FR", "Mobile")

val syncRules = List(
  Engine.deviceRule("Desktop"),
  Engine.deviceRule("Desktop")
)

Engine.Rule.fold(syncRules).run(ad)
