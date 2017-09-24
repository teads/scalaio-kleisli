package tv.teads

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, _}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Threads(20)
@Fork(value = 1, jvmArgsAppend = Array("-Djmh.stack.lines=3"))
@BenchmarkMode(Array(Mode.Throughput))
class RuleBenchmark {



  val step_1_rules = List(
    step1.Engine.countryRule("FR"), step1.Engine.deviceRule("Mobile")
  )
  @Benchmark
  def benchmark_step1_predicate(): Unit = {
    val ad = Ad("FR", "Mobile")

    step1.Engine.canBroadcast(step_1_rules)(ad)
  }



  val step_2_rules = List(
    step2.Engine.countryRule("FR"), step2.Engine.deviceRule("Mobile")
  )
  @Benchmark
  def benchmark_step2_predicate(): Unit = {
    val ad = Ad("FR", "Mobile")

    step2.Engine.canBroadcast(step_2_rules)(ad)
  }



  val step_3_rules = List(
    step3.Engine.countryRule("FR"), step3.Engine.deviceRule("Mobile")
  )
  @Benchmark
  def benchmark_step3_futures(): Unit = {
    val ad = Ad("FR", "Mobile")

    Await.result(step3.Engine.canBroadcast(step_3_rules)(global)(ad), atMost = 1.minute)
  }
}
