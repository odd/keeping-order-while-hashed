package odd
package orderkeeping
package time

import scala.collection.immutable._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

abstract class AbstractMapTimeBenchmark extends AbstractTimeBenchmark {
  //@Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  @Param(scala.Array("63", "4096")) //, "128000")) /*/*"7", "65", "1024", "4096"*//*, "128000"*/))*/
  var size: Int = _

  var xs: Map[Long, Long] = _
  var ys: Map[Long, Long] = _
  var zs: Map[Long, Long] = _
  val random = new scala.util.Random(19740115L)
  var randomIndices: scala.Array[Int] = _

  def create(n: Int = 0): Map[Long, Long]
  def zero: (Long, Long) = (0L, 0L)
  def successor: ((Long, Long)) => (Long, Long) = { case (x, y) => (x + 1, y - 1) }
  def mapper: ((Long, Long)) => (Long, Long) = { case (x, y) => (-x, -y) }
  def grouper: ((Long, Long)) => (Long, Long) = { case (x, y) => (x % 5L, y) }
  def combiner: (((Long, Long), (Long, Long))) => (Long, Long) = { case ((x1, y1), (x2, y2)) => (x1 + x2, y1 - y2) }
  def finder: ((Long, Long)) => Boolean = { case (x, y) => x > size / 2L }
  def spanner: ((Long, Long)) => Boolean = _._1 < size / 2L

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    except("initTrial")
    xs = create(size)
    ys = create(size / 2) ++ create(size / 2).map(mapper)
    zs = create((size / 1000).max(2))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(random.nextInt(size))
    }
  }

  @Benchmark
  def expand_concat(bh: Blackhole): Unit = {
    except("expand_concat")
    bh.consume(xs ++ zs)
  }

  @Benchmark
  def traverse_iterator(bh: Blackhole): Unit = {
    except("traverse_iterator")
    val it = xs.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def traverse_foreach(bh: Blackhole): Unit = {
    except("traverse_foreach")
    xs.foreach(bh.consume)
  }

  @Benchmark
  def traverse_headTail(bh: Blackhole): Unit = {
    except("traverse_headTail")
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.head)
      ys = ys.tail
    }
  }

  @Benchmark
  def traverse_initLast(bh: Blackhole): Unit = {
    except("traverse_initLast")
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.last)
      ys = ys.init
    }
  }

  @Benchmark
  def traverse_foldLeft(bh: Blackhole): Unit = {
    except("traverse_foldLeft")
    bh.consume(xs.foldLeft(zero) {
      case (acc, t) =>
        bh.consume(acc)
        combiner(acc, t)
    })
  }

  @Benchmark
  def traverse_foldRight(bh: Blackhole): Unit = {
    except("traverse_foldRight")
    bh.consume(xs.foldRight(zero) {
      case (t, acc) =>
        bh.consume(acc)
        combiner(acc, t)
    })
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_last(bh: Blackhole): Unit = {
    except("access_last")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.last)
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_tail(bh: Blackhole): Unit = {
    except("access_tail")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.tail)
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_init(bh: Blackhole): Unit = {
    except("access_init")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.init)
      i += 1
    }
    bh.consume(i)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def access_slice(bh: Blackhole): Unit = {
    except("access_slice")
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(i * size / 200, size - i * size / 200))
      i += 1
    }
  }

  @Benchmark
  def transform_map(bh: Blackhole): Unit = {
    except("transform_map")
    bh.consume(xs.map(mapper))
  }

  @Benchmark
  def transform_collect(bh: Blackhole): Unit = {
    except("transform_collect")
    bh.consume(xs.collect {
      case (k: Long, v: Long) if k % 5L == 0L =>
        val t = mapper((k, v))
        bh.consume(t)
        t
      case (k: Long, v: Long) if k % 3L == 0L =>
        val t = mapper((k, v))
        bh.consume(t)
        t
      case (k: Long, v: Long) if k == size - 1 =>
        val t = mapper((k, v))
        bh.consume(t)
        t
    })
  }

  @Benchmark
  def transform_flatMap(bh: Blackhole): Unit = {
    except("transform_flatMap")
    bh.consume(xs.flatMap {
      case (k: Long, v: Long) if k % 50L == 0L =>
        bh.consume(k)
        List.range(1L, k / 5, 5).map(n => (-n, v))
      case (k: Long, v: Long) if k % 3L == 0L =>
        bh.consume(k)
        List((k, -v), (-k, -v))
      case (k: Long, v: Long) if k == size - 1 =>
        bh.consume(k)
        List.range(1L, k / 10).map(n => (-n, -v))
      case _ =>
        List()
    })
  }

  @Benchmark
  def transform_filter(bh: Blackhole): Unit = {
    except("transform_filter")
    bh.consume(xs.filter(_._2 % 5L == 0L))
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def transform_span(bh: Blackhole): Unit = {
    except("transform_span")
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(spanner)
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def transform_zip(bh: Blackhole): Unit = {
    except("transform_zip")
    bh.consume(xs.zip(xs))
  }

  @Benchmark
  def transform_zipWithIndex(bh: Blackhole): Unit = {
    except("transform_zipWithIndex")
    bh.consume(xs.zipWithIndex)
  }

  @Benchmark
  def transform_groupBy(bh: Blackhole): Unit = {
    except("transform_groupBy")
    bh.consume(xs.groupBy(grouper))
  }

  @Benchmark
  def access_find(bh: Blackhole): Unit = {
    except("access_find")
    bh.consume(xs.find(finder))
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def expand_update(bh: Blackhole): Unit = {
    except("expand_update")
    var ys = xs
    var i = 0L
    while (i < 1000) {
      ys += (i -> -i)
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_apply(bh: Blackhole): Unit = {
    except("access_apply")
    var i = 0
    while (i < 1000) {
      bh.consume(xs(randomIndices(i)))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def access_contains(bh: Blackhole): Unit = {
    except("access_contains")
    var i = 0
    while (i < 1000) {
      bh.consume(xs.contains(i))
      i += 1
    }
  }

  @Benchmark
  def transform_removeAllIterator(bh: Blackhole): Unit = {
    except("transform_removeAllIterator")
    var n = 0L
    val it = (xs -- randomIndices.map(_.toLong)).iterator
    while (it.hasNext) {
      n += it.next()._2
      bh.consume(n)
    }
    bh.consume(n)
  }
}
