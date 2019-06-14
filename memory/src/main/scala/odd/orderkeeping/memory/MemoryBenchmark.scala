package odd
package orderkeeping
package memory

import java.io.File
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Paths}
import java.time.Instant
import scala.collection.mutable.LinkedHashMap
import collection.immutable.{KeyLinkedSeqMap, KeyLinkedSeqMapH, QueueSeqMap, TreeHashSeqMap, TreeSeqMap, VectorMap}

object MemoryBenchmark extends App {
  val resultsDir = new File("memory/results")
  resultsDir.mkdirs()
  val reportPath = Paths.get("memory/results/memory-" + Instant.now().toString.replace(':', '-') + ".json")
  val sizes = scala.List(8, 64, 512, 4096) //, 32768, 262144) //, 2097152)
  val runtime = Runtime.getRuntime
  var placeholder: Any = _

  case class Metric(score: Double, scoreConfidence: (Double, Double))
  case class Result(benchmark: String, params: Map[String, String], primaryMetric: Metric)

  def getCurrentlyUsedMemory = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed + ManagementFactory.getMemoryMXBean.getNonHeapMemoryUsage.getUsed
  def getGcCount = {
    import scala.collection.JavaConverters._
    var sum = 0L
    for (b <- ManagementFactory.getGarbageCollectorMXBeans.asScala) {
      val count = b.getCollectionCount
      if (count != -1) sum += count
    }
    sum
  }
  def getReallyUsedMemory = {
    val before = getGcCount
    System.gc()
    while ( {
      getGcCount == before
    }) {}
    getCurrentlyUsedMemory
  }

  def benchmark[A](gen: Int => A) = (

      for (_ <- scala.Range(0, 5)) yield {
        for (size <- sizes) yield {
          placeholder = null
          val memBefore = getReallyUsedMemory
          placeholder = gen(size)
          val memAfter = getReallyUsedMemory
          size -> (memAfter - memBefore)
        }
      }
      ).last

  val memories =
    Map(
      //"ListMap"               -> benchmark(n => ListMap((0 until n).map(n => n -> -n): _*)),
      "TreeHashSeqMap"        -> benchmark(n => TreeHashSeqMap((0 until n).map(n => n -> -n): _*)),
      "TreeSeqMap"            -> benchmark(n => TreeSeqMap((0 until n).map(n => n -> -n): _*)),
      "KeyLinkedSeqMap"       -> benchmark(n => KeyLinkedSeqMap((0 until n).map(n => n -> -n): _*)),
      "KeyLinkedSeqMapH"      -> benchmark(n => KeyLinkedSeqMapH((0 until n).map(n => n -> -n): _*)),
      "VectorMap"             -> benchmark(n => VectorMap((0 until n).map(n => n -> -n): _*)),
      "QueueSeqMap"           -> benchmark(n => QueueSeqMap((0 until n).map(n => n -> -n): _*)),
      "LinkedHashMap"         -> benchmark(n => LinkedHashMap((0 until n).map(n => n -> -n): _*)),
    )

  // We use a format similar to the one used by JMH so that
  // our charts can be generated in the same way
  val report =
  memories.toList.flatMap { case (name, values) =>
    values.map { case (size, value) =>
      val metric = Metric(value, (value, value))
      Result(s"$name.memory", Map("size" -> size.toString), metric)
    }
  }

  Files.write(reportPath, format(report).getBytes("UTF-8"))

  def format(report: List[Result]): String = {
    report.map { result =>
      val paramsStr = (s"""      "impl" : "${result.benchmark}"""" :: result.params.map {
        case (name, value) => s"""      "$name" : "$value""""
      }.toList).mkString("{\n", ",\n", "\n    }")
      s"""
         |  {
         |    "jmhVersion" : "1.21",
         |    "benchmark" : "Memory",
         |    "mode" : "avgt",
         |    "threads" : 1,
         |    "forks" : 1,
         |    "jvm" : "${System.getProperty("java.home")}",
         |    "jvmArgs" : [],
         |    "jdkVersion" : "${System.getProperty("java.version")}",
         |    "vmName" : "Java HotSpot(TM) 64-Bit Server VM",
         |    "vmVersion" : "25.181-b13",
         |    "warmupIterations" : 5,
         |    "warmupTime" : "200 ms",
         |    "warmupBatchSize" : 1,
         |    "measurementIterations" : 7,
         |    "measurementTime" : "500 ms",
         |    "measurementBatchSize" : 1,
         |    "params" : $paramsStr,
         |    "primaryMetric" : {
         |      "score" : ${result.primaryMetric.score},
         |      "scoreError" : 0,
         |      "scoreConfidence" : [
         |        ${result.primaryMetric.scoreConfidence._1},
         |        ${result.primaryMetric.scoreConfidence._2}
         |      ],
         |      "scorePercentiles" : {
         |        "100.0" : ${result.primaryMetric.score}
         |      },
         |      "scoreUnit" : "B/op",
         |      "rawData" : [
         |        [
         |          ${result.primaryMetric.score}
         |        ]
         |      ]
         |    },
         |    "secondaryMetrics" : {}
         |  }""".stripMargin
    }.mkString("[\n", ",\n", "\n]")
  }
}

