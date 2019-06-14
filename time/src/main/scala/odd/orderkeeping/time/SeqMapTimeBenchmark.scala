package odd
package orderkeeping
package time

import scala.collection.immutable._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class SeqMapTimeBenchmark extends AbstractMapTimeBenchmark {
  //@Param(scala.Array("ListMap", "KeyLinkedSeqMap", "TreeHashSeqMap", "TreeSeqMap", "VectorMap", "QueueSeqMap"))
  //@Param(scala.Array("KeyLinkedSeqMap", "TreeHashSeqMap", "TreeSeqMap", "TreeSeqMapO", "VectorMap", "QueueSeqMap"))
  @Param(scala.Array("KeyLinkedSeqMap", "TreeHashSeqMap", "TreeSeqMap", "VectorMap", "QueueSeqMap"))
  //@Param(scala.Array("KeyLinkedSeqMap", "KeyLinkedSeqMapH", "TreeSeqMap"))
  //@Param(scala.Array("KeyLinkedSeqMap", "TreeSeqMap"))
  //@Param(scala.Array("TreeSeqMap", "TreeSeqMapO"))
  //@Param(scala.Array("QueueSeqMap"))
  var impl: String = _

  def create(n: Int = 0): Map[Long, Long] = {
    impl match {
      case "ListMap" => ListMap((0L until size).map(n => (n, -n)): _*)
      case "KeyLinkedSeqMap" => KeyLinkedSeqMap((0L until size).map(n => (n, -n)): _*)
      case "KeyLinkedSeqMapH" => KeyLinkedSeqMapH((0L until size).map(n => (n, -n)): _*)
      case "TreeHashSeqMap" => TreeHashSeqMap((0L until size).map(n => (n, -n)): _*)
      case "TreeSeqMap" => TreeSeqMap((0L until size).map(n => (n, -n)): _*)
      case "TreeSeqMapO" => TreeSeqMapO((0L until size).map(n => (n, -n)): _*)
      case "VectorMap" => VectorMap((0L until size).map(n => (n, -n)): _*)
      case "QueueSeqMap" => QueueSeqMap((0L until size).map(n => (n, -n)): _*)
    }
  }
  override def exceptions = {
    case "transform_removeConsecutive" => size > 4096 && (impl.contains("VectorMap") || impl.contains("ListMap") || impl.contains("QueueSeqMap"))
    case "transform_removeConsecutiveIterator" => size > 4096 && (impl.contains("VectorMap") || impl.contains("ListMap") || impl.contains("QueueSeqMap"))
    case "traverse_headTail" => size >= 4096 && impl.contains("ListMap")
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_append(bh: Blackhole): Unit = {
    except("expand_append")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys += (-i -> i)
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def transform_removeConsecutiveIterator(bh: Blackhole): Unit = {
    except("transform_removeConsecutiveIterator")
    var n = 0L
    val it = (xs -- Seq.range[Long](size / 50, size - (size / 50))).iterator
    while (it.hasNext) {
      n += it.next()._2
      bh.consume(n)
    }
    bh.consume(n)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_splitAt(bh: Blackhole): Unit = {
    except("transform_splitAt")
    var i = 2
    while (i < 100) {
      bh.consume(xs.splitAt(size / i))
      i += 1
    }
  }
}
