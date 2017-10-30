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

  val ad = Ad("FR", "Mobile")

  /** ******
    * STEP 1
    * ***** */
  val step_1_rules = List(
    step1.Engine.countryRule("FR"), step1.Engine.deviceRule("Mobile")
  )

  @Benchmark
  def benchmark_step1_predicate(): Unit = {
    step1.Engine.Rule.run(step_1_rules, ad)
  }

  /** ******
    * STEP 2
    * ***** */
  val step_2_rules = List(
    step2.Engine.countryRule("FR"), step2.Engine.deviceRule("Mobile")
  )

  @Benchmark
  def benchmark_step2_predicate(): Unit = {
    step2.Engine.Rule.run(step_2_rules, ad)
  }

  /** ******
    * STEP 3
    * ***** */
  val step_3_rules = List(
    step3.Engine.countryRule("FR"), step3.Engine.deviceRule("Mobile")
  )

  @Benchmark
  def benchmark_step3_futures(): Unit = {
    Await.result(step3.Engine.Rule.run(step_3_rules, ad), atMost = 1.second)
  }

  /** ******
    * STEP 4
    * ***** */
  val step_4_sync_rules = List(
    step4.Engine.deviceRule("Mobile"),
    step4.Engine.deviceRule("Mobile")
  )

  @Benchmark
  def benchmark_step4_sync_rules(): Unit = {
    step4.Engine.Rule.runSync(step_4_sync_rules, ad)
  }

  val step_4_async_rules = List(
    step4.Engine.countryRule("FR"),
    step4.Engine.deviceRule("Mobile")
  )

  @Benchmark
  def benchmark_step4_async_rules(): Unit = {
    Await.result(step4.Engine.Rule.run(step_4_async_rules, ad), atMost = 1.second)
  }

  /** ******
    * STEP 5
    * ***** */
  val step_5_sync_rules = List(
    step5.Engine.deviceRule("Mobile"),
    step5.Engine.deviceRule("Mobile")
  )

  @Benchmark
  def benchmark_step5_sync_rules(): Unit = {
    step5.Engine.Rule.fold(step_5_sync_rules).run(ad)
  }

  val step5CountryRule = step5.Engine.countryRule("FR")
  val step5DeviceRule = step5.Engine.deviceRule("Mobile")

  import cats.instances.future._
  import step5.Engine.Rule.Transformer.idToFuture

  @Benchmark
  def benchmark_step5_async_rules(): Unit = {
    Await.result(step5.Engine.Rule.transform(step5DeviceRule, step5CountryRule).run(ad), atMost = 1.second)
  }
}
