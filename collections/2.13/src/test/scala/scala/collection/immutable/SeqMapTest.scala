package scala
package collection
package immutable

import scala.util.Try
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SubjectTest {
  //type Subject[K, V] = ListMap[K, V]
  //val Subject = ListMap
  type Subject[K, V] = KeyLinkedSeqMap[K, V]
  val Subject = KeyLinkedSeqMap
  //type Subject[K, V] = VectorMap[K, V]
  //val Subject = VectorMap
  //type Subject[K, V] = TreeHashSeqMap[K, V]
  //val Subject = TreeHashSeqMap
  //type Subject[K, V] = TreeSeqMap[K, V]
  //val Subject = TreeSeqMap
  //type Subject[K, V] = TreeSeqMapO[K, V]
  //val Subject = TreeSeqMapO
  //type Subject[K, V] = QueueSeqMap[K, V]
  //val Subject = QueueSeqMap
  /*
  object Subject {
    def apply[K, V](entries: (K, V)*) = VectorMap(entries: _*)
    def empty[K, V] = VectorMap.empty[K, V]
  }*/
  @Test
  def t7445(): Unit = {
    val m = Subject(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4)
    assertEquals(Subject(2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), m.tail)
  }
  @Test
  def testBuilder(): Unit = {
    val m = Subject("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4")
    assertEquals(List("d" -> "1", "b" -> "2", "a" -> "3", "e" -> "2.2", "c" -> "4"), m.toList)
  }
  @Test
  def testHeadTailLastInitWhenOrderingByInsertion(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(3 -> 1, m.head)
    assertEquals(Subject(2 -> 2, 1 -> 3), m.tail)
    assertEquals(1 -> 3, m.last)
    assertEquals(Subject(3 -> 1, 2 -> 2), m.init)
  }
  /*@Test
  def testHeadTailLastInitWhenOrderingByModification(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(Subject.OrderBy.Modification).updated(2, 4)
    assertEquals(3 -> 1, m.head)
    assertEquals(Subject(1 -> 3, 2 -> 4), m.tail)
    assertEquals(2 -> 4, m.last)
    assertEquals(Subject(3 -> 1, 1 -> 3), m.init)
  }*/
  @Test
  def testAddWhenOrderingByInsertion(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(Subject(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(Subject(3 -> 1, 2 -> 4, 1 -> 3), m + (2 -> 4))
    assertEquals(Subject(3 -> 1, 2 -> 2, 1 -> 3), m + (2 -> 2))
    assertEquals(Subject(3 -> 2, 2 -> 2, 1 -> 3), m + (3 -> 2))
  }
  @Test
  def testRemoveWhenOrderingByInsertion(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3)
    assertEquals(Subject(3 -> 1, 2 -> 2), m - 1)
    assertEquals(Subject(3 -> 1, 1 -> 3), m - 2)
    assertEquals(Subject(3 -> 1, 2 -> 2, 1 -> 3), m - 4)
  }
  /*@Test
  def testAddWhenOrderingByModification(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(Subject.OrderBy.Modification)
    assertEquals(Subject(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(Subject(3 -> 1, 1 -> 3, 2 -> 4), m + (2 -> 4))
    assertEquals(Subject(3 -> 1, 1 -> 3, 2 -> 2), m + (2 -> 2))
    assertEquals(Subject(2 -> 2, 3 -> 2, 1 -> 4), m + (3 -> 2) + (1 -> 4))
  }*/
  /*@Test
  def testRemoveWhenOrderingByModification(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3).orderingBy(Subject.OrderBy.Modification).updated(3, 3)
    assertEquals(Subject(2 -> 2, 3 -> 3), m - 1)
    assertEquals(Subject(1 -> 3, 3 -> 3), m - 2)
    assertEquals(Subject(2 -> 2, 1 -> 3, 3 -> 3), m - 4)
  }*/
  @Test
  def testRemoveMultipleWhenOrderingByInsertion(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(Subject(3 -> 1, 2 -> 2, 5 -> 4), (m - 1) - 4)
    assertEquals(Subject(1 -> 3, 5 -> 4), (m - 3) - 2 - 4)
    assertEquals(Subject(4 -> 5, 5 -> 4), (m - 3) - 1 - 2)
  }
  /*@Test
  def testRemoveMultipleWhenOrderingByModification(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4).orderingBy(Subject.OrderBy.Modification).updated(3, 3)
    assertEquals(Subject(2 -> 2, 5 -> 4, 3 -> 3), (m - 1) - 4)
    assertEquals(Subject(1 -> 3, 5 -> 4), (m - 3) - 2 - 4)
    assertEquals(Subject(4 -> 5, 5 -> 4), (m - 3) - 1 - 2)
  }*/
  @Test
  def testIterator(): Unit = {
    assertEquals(Nil, Subject.empty.iterator.toList)
    assertEquals(List(4 -> 1), Subject(4 -> 1).iterator.toList)
    assertEquals(List(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4), Subject(4 -> 1, 2 -> 2, 1 -> 3, 5 -> 5, 3 -> 4).iterator.toList)
  }
  @Test
  def testRemoveIterator(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2, 5 -> 4), ((m - 1) - 4).iterator.toList)
    assertEquals(List(1 -> 3, 5 -> 4), ((m - 3) - 2 - 4).iterator.toList)
    assertEquals(List(4 -> 5, 5 -> 4), ((m - 3) - 1 - 2).iterator.toList)
  }
  @Test
  def testSlice(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    assertEquals(List(3 -> 1, 2 -> 2), ((m - 1) - 4).slice(0, 2).iterator.toList)
    assertEquals(List(5 -> 4), ((m - 3) - 2 - 4).slice(1, 2).iterator.toList)
    assertEquals(List(), ((m - 3) - 1 - 2).slice(2, 2).iterator.toList)
    assertEquals(List(), ((m - 3) - 1 - 2).slice(1, 1).iterator.toList)
    val x = List(3 -> 3, 4 -> 4, 5 -> 5)
    val y = Subject(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7)
    assertEquals(x, y.slice(2, 5).iterator.toList)
    assertEquals(List(3 -> 3, 4 -> 4, 5 -> 5), Subject(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(2, 5).iterator.toList)
    assertEquals(List(7 -> 7), Subject(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(6, 7).iterator.toList)
    assertEquals(List(), Subject(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6, 7 -> 7).slice(7, 7).iterator.toList)
  }

  @Test
  def testSplitAt(): Unit = {
    val m = Subject(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)
    var t = m.splitAt(0)
    assertEquals((List(), List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(1)
    assertEquals((List(3 -> 1), List(2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(2)
    assertEquals((List(3 -> 1, 2 -> 2), List(1 -> 3, 4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(3)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3), List(4 -> 5, 5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(4)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5), List(5 -> 4)), (t._1.iterator.toList, t._2.iterator.toList))
    t = m.splitAt(5)
    assertEquals((List(3 -> 1, 2 -> 2, 1 -> 3, 4 -> 5, 5 -> 4), List()), (t._1.iterator.toList, t._2.iterator.toList))
  }

  @Test
  def testConcatAsSeqs(): Unit = {
    val x = Subject(3L -> "wish")
    val y = Subject(9L -> "nine", 3L -> "three", 7L -> "seven", -1L -> "negative", 42L -> "Adams", 6L -> "vi")
    val xy = x ++ y
    val xySeq = x.toSeq ++ y.toSeq
    val xyMap = xySeq.to(Subject)
    assertEquals(xy.toList, xyMap.toList)
  }

  /*@Test
  def testAppend(): Unit = {
    import Subject._
    import Subject.Ordering._
    val x = Ordering(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six")
    val y = Ordering(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
    val z = Ordering(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
    val y2 = y.appendInPlace(6, "six").cleanInPlace()
    assertEquals(x, y2)
    val z2 = z.append(6, "six")
    assertEquals(x, z2)

    for (i <- 1 until 1027) {
      val a1 = TreeSeqMapZ.Ordering((1 until i).map(n => n -> toBinaryString(n)): _*).exclude(i / 2).exclude(i / 10).exclude(i - 1)
      val value = Ordering((1 until i).map(n => n -> toBinaryString(n)): _*)
      val b1 = value.exclude(i / 2).exclude(i / 10).exclude(i - 1)
      val c1 = Ordering((1 until i).map(n => n -> toBinaryString(n)): _*).exclude(i / 2).exclude(i / 10).exclude(i - 1)
      assertEquals(s"$i", a1.toString.trim, b1.toString.trim)
      assertEquals(s"$i", a1.toString.trim, c1.toString.trim)
      val v = toBinaryString(i)
      val a2 = a1.include(i, v)
      val b2 = b1.include(i, v)
      val c2 = c1.append(i, v)
      assertEquals(s"$i", a2.toString.trim, b2.toString.trim)
      assertEquals(s"$i", a2.toString.trim, c2.toString.trim)
      assertEquals(s"$i", b2, c2)
      val d2 = c1.appendInPlace(i, v).cleanInPlace()
      assertEquals(s"$i", c2, d2)
    }
  }*/

  /*@Test
  def testIncludeAndExclude(): Unit = {
    import Subject.Ordering._

    var o1 = TreeSeqMap.Ordering.empty[String]
    var o2 = Subject.Ordering.empty[String]
    for (i <- 1 until 1027) {
      val v = toBinaryString(i)
      o1 = o1.include(i, v)
      o2 = o2.include(i, v)
      if (i % 5 == 0) {
        o1 = o1.exclude(i / 2)
        o2 = o2.exclude(i / 2)
      }
      val s1 = o1.toString
      val s2 = o2.toString
      assertEquals(s"$i [$v]", (if (!s1.endsWith("\n")) s1 + "\n" else s1) + o1.toString, s2 + o2.toString)
    }
  }*/
  @Test
  def testHeadTail(): Unit = {
    for (i <- 1 until 35) {
      var xs = Subject.from((1 to i).iterator.map(n => (n, -n)))
      var ys = VectorMap.from((1 to i).iterator.map(n => (n, -n)))
      var j = 1
      while (ys.nonEmpty) {
        Try {
          val xh = xs.head
          xs = xs.tail
          val yh = ys.head
          ys = ys.tail
          assertEquals(s"heads [i = $i, j = $j]", xh, yh)
          assertEquals(s"tails [i = $i, j = $j]", xs, ys)
          assertTrue(s"tails equals [i = $i, j = $j]", xs.equals(ys))
          j += 1
        }.fold(t => {
          println(s"i = $i, j = $j: ${t.getMessage}")
          t.printStackTrace()
          throw t
        }, identity)
      }
    }
  }
  @Test
  def testInitLast(): Unit = {
    for (i <- 1 until 35) {
      var xs = Subject.from((1 to i).iterator.map(n => (n, -n)))
      var ys = VectorMap.from((1 to i).iterator.map(n => (n, -n)))
      var j = 1
      while (ys.nonEmpty) {
        Try {
          val xl = xs.last
          xs = xs.init
          val yl = ys.last
          ys = ys.init
          assertEquals(s"lasts [i = $i, j = $j]", xl, yl)
          assertEquals(s"inits [i = $i, j = $j]", xs, ys)
          assertTrue(s"inits equals [i = $i, j = $j]", xs.equals(ys))
          j += 1
        }.fold(t => {
          println(s"i = $i, j = $j: ${t.getMessage}")
          t.printStackTrace()
          throw t
        }, identity)
      }
    }
  }
}
/*object SubjectMain extends App {
  import Subject.Ordering._

  var o1 = TreeSeqMap.Ordering.empty[String]
  var o2 = Subject.Ordering.empty[String]
  for (i <- 1 until 10) {
    val v = toBinaryString(i)
    o1 = o1.include(i, v)
    o2 = o2.include(i, v)
    println(s"$i [$v]:\n" + o1)
    println(s"$i [$v] Y:\n" + o2)
    println()
  }
}*/