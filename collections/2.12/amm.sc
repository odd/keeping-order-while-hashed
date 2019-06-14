
import collection.immutable._
import collection.immutable.extended._
import collection.mutable
import collection.mutable.extended._
import scala.util.Random
import reftree.core._
import reftree.diagram._
import reftree.render._
import reftree.geometry._
import reftree.svg.animation.Frame
import reftree.svg.XmlSvgApi
import reftree.svg.XmlSvgApi.svgUnzip
import reftree.contrib.XmlInstances._
import reftree.contrib.OpticInstances._
import reftree.contrib.ZipperInstances._
import reftree.contrib.ShapelessInstances._
import reftree.util.Optics
import Diagram.{sourceCodeCaption ⇒ diagram}
import odd.orderkeeping._

val both = IndexedSeq(
  Color.fromRgbaString("#104e8b"),
  Color.fromRgbaString("#228b22")
)
val first = IndexedSeq(
  Color.fromRgbaString("#104e8b"),
  Color.fromRgbaString("#ffffff")
)
val lastOnly = IndexedSeq(
  Color.fromRgbaString("#ffffff"),
  Color.fromRgbaString("#228b22")
)
val renderer = Renderer(RenderingOptions(density = 75))
import renderer._

def className(x: Any) = {
  val last = x.getClass.getName.split('.').last
  val i = last.indexOf('$')
  last.substring(0, if (i > 0) i else last.length)
}

val f = (n: Int) => ('A' + n).toChar -> ('a' + n).toChar
val abcd = List.range(0, 4).map(f)
val abcdLinkedHashMap = LinkedHashMap(abcd: _*)
val abcdListMap = ListMap(abcd: _*)
val abcdVectorMap = VectorMap(abcd: _*)
val abcdTreeSeqMap = TreeSeqMap(abcd: _*)
val abcdTreeHashSeqMap = TreeHashSeqMap(abcd: _*)
val abcdKeyLinkedSeqMap = KeyLinkedSeqMap(abcd: _*)
val abcdQueueSeqMap = QueueSeqMap(abcd: _*)

val abcdeLinkedHashMap = abcdLinkedHashMap + f(4)
val abcdeListMap = abcdListMap + f(4)
val abcdeVectorMap = abcdVectorMap + f(4)
val abcdeTreeSeqMap = abcdTreeSeqMap + f(4)
val abcdeTreeHashSeqMap = abcdTreeHashSeqMap + f(4)
val abcdeKeyLinkedSeqMap = abcdKeyLinkedSeqMap + f(4)
val abcdeQueueSeqMap = abcdQueueSeqMap + f(4)

val abdLinkedHashMap = abcdLinkedHashMap - 'C'
val abdListMap = abcdListMap - 'C'
val abdVectorMap = abcdVectorMap - 'C'
val abdTreeSeqMap = abcdTreeSeqMap - 'C'
val abdTreeHashSeqMap = abcdTreeHashSeqMap - 'C'
val abdKeyLinkedSeqMap = abcdKeyLinkedSeqMap - 'C'
val abdQueueSeqMap = abcdQueueSeqMap - 'C'

diagram(abcdListMap).withCaption("abcd").render("ListMap-abcd")
diagram(abcdKeyLinkedSeqMap).withCaption("abcd").render("KeyLinkedSeqMap-abcd")
diagram(abcdVectorMap).withCaption("abcd").render("VectorMap-abcd")
diagram(abcdTreeSeqMap).withCaption("abcd").render("TreeSeqMap-abcd")
diagram(abcdTreeHashSeqMap).withCaption("abcd").render("TreeHashSeqMap-abcd")
diagram(abcdQueueSeqMap).withCaption("abcd").render("QueueSeqMap-abcd")

(diagram(abcdListMap).withCaption("abcd") + diagram(abcdeListMap).withoutCaptions).render("ListMap-abcd-abcde-1", _.copy(palette = first))
(diagram(abcdListMap).withCaption("abcd") + diagram(abcdeListMap).withCaption("abcde")).render("ListMap-abcd-abcde-2")
(diagram(abcdKeyLinkedSeqMap).withCaption("abcd") + diagram(abcdeKeyLinkedSeqMap).withoutCaptions).render("KeyLinkedSeqMap-abcd-abcde-1", _.copy(palette = first))
(diagram(abcdKeyLinkedSeqMap).withCaption("abcd") + diagram(abcdeKeyLinkedSeqMap).withCaption("abcde")).render("KeyLinkedSeqMap-abcd-abcde-2")
(diagram(abcdVectorMap).withCaption("abcd") + diagram(abcdeVectorMap).withoutCaptions).render("VectorMap-abcd-abcde-1", _.copy(palette = first))
(diagram(abcdVectorMap).withCaption("abcd") + diagram(abcdeVectorMap).withCaption("abcde")).render("VectorMap-abcd-abcde-2")
(diagram(abcdTreeSeqMap).withCaption("abcd") + diagram(abcdeTreeSeqMap).withoutCaptions).render("TreeSeqMap-abcd-abcde-1", _.copy(palette = first))
(diagram(abcdTreeSeqMap).withCaption("abcd") + diagram(abcdeTreeSeqMap).withoutCaptions).render("TreeSeqMap-abcd-abcde-2")
(diagram(abcdTreeHashSeqMap).withCaption("abcd") + diagram(abcdeTreeHashSeqMap).withoutCaptions).render("TreeHashSeqMap-abcd-abcde-1", _.copy(palette = first))
(diagram(abcdTreeHashSeqMap).withCaption("abcd") + diagram(abcdeTreeHashSeqMap).withCaption("abcde")).render("TreeHashSeqMap-abcd-abcde-2")
(diagram(abcdQueueSeqMap).withCaption("abcd") + diagram(abcdeQueueSeqMap).withoutCaptions).render("QueueSeqMap-abcd-abcde-1", _.copy(palette = first))
(diagram(abcdQueueSeqMap).withCaption("abcd") + diagram(abcdeQueueSeqMap).withCaption("abcde")).render("QueueSeqMap-abcd-abcde-2")

(diagram(abcdListMap).withCaption("abcd") + diagram(abdListMap).withoutCaptions).render("ListMap-abcd-abd-1", _.copy(palette = first))
(diagram(abcdListMap).withCaption("abcd") + diagram(abdListMap).withCaption("abd")).render("ListMap-abcd-abd-2")
(diagram(abcdKeyLinkedSeqMap).withCaption("abcd") + diagram(abdKeyLinkedSeqMap).withoutCaptions).render("KeyLinkedSeqMap-abcd-abd-1", _.copy(palette = first))
(diagram(abcdKeyLinkedSeqMap).withCaption("abcd") + diagram(abdKeyLinkedSeqMap).withCaption("abd")).render("KeyLinkedSeqMap-abcd-abd-2")
(diagram(abcdVectorMap).withCaption("abcd") + diagram(abdVectorMap).withoutCaptions).render("VectorMap-abcd-abd-1", _.copy(palette = first))
(diagram(abcdVectorMap).withCaption("abcd") + diagram(abdVectorMap).withoutCaptions).render("VectorMap-abcd-abd-2")
(diagram(abcdTreeSeqMap).withCaption("abcd") + diagram(abdTreeSeqMap).withoutCaptions).render("TreeSeqMap-abcd-abd-1", _.copy(palette = first))
(diagram(abcdTreeSeqMap).withCaption("abcd") + diagram(abdTreeSeqMap).withCaption("abd")).render("TreeSeqMap-abcd-abd-2")
(diagram(abcdTreeHashSeqMap).withCaption("abcd") + diagram(abdTreeHashSeqMap).withoutCaptions).render("TreeHashSeqMap-abcd-abd-1", _.copy(palette = first))
(diagram(abcdTreeHashSeqMap).withCaption("abcd") + diagram(abdTreeHashSeqMap).withCaption("abd")).render("TreeHashSeqMap-abcd-abd-2")
(diagram(abcdQueueSeqMap).withCaption("abcd") + diagram(abdQueueSeqMap).withoutCaptions).render("QueueSeqMap-abcd-abd-1", _.copy(palette = first))
(diagram(abcdQueueSeqMap).withCaption("abcd") + diagram(abdQueueSeqMap).withCaption("abd")).render("QueueSeqMap-abcd-abd-2")

def animation[K : ToRefTree, V : ToRefTree, M <: Map[K, V] : ToRefTree](initial: M, count: Int, from: Int = 0, operation: String = null)(next: (M, Int) => M) = {
  val to = from + count
  def className(x: Any) = {
    val last = x.getClass.getName.split('.').last
    val i = last.indexOf('$')
    last.substring(0, if (i > 0) i else last.length)
  }
  val name = s"${className(initial)}${if (operation != null) "-" + operation else ""}-$from-$to"
  Animation
      .startWith(initial)
      .iterateWithIndex(count) {
        case (m, i) => next(m, from + i)
      }
      .build(Diagram(_).withAnchor(name))
      .toNamespace(name)
}
def animateCharCharMap[M <: Map[Char, Char] : ToRefTree](initial: M, count: Int, from: Int = 0, operation: String = null)(next: (M, Int) => M) = {
  val to = from + count
  val name = s"${className(initial)}${if (operation != null) "-" + operation else ""}-$from-$to"
  animation[Char, Char, M](initial, count, from, operation)(next).render(name)
}

animateCharCharMap(abcdVectorMap, 1, 3) {
  case (m, n) => m + f(n)
}
animateCharCharMap(abcdListMap, 1, 3) {
  case (m, n) => m + f(n)
}
animateCharCharMap(abcdQueueSeqMap, 1, 3) {
  case (m, n) => m + f(n)
}
animateCharCharMap(abcdKeyLinkedSeqMap, 1, 3) {
  case (m, n) => m + f(n)
}
animateCharCharMap(abcdTreeHashSeqMap, 1, 3) {
  case (m, n) => m + f(n)
}
animateCharCharMap(abcdTreeSeqMap, 1, 3) {
  case (m, n) => m + f(n)
}


animateCharCharMap(VectorMap.empty[Char, Char], 5) {
  case (m, n) => m + f(n)
}


animateCharCharMap(abcdeVectorMap, 2, 5, "remove") {
  case (m, n) => m - m.head._1
}
animateCharCharMap(abcdeTreeSeqMap, 2, 5, "remove") {
  case (m, n) => m - m.head._1
}

Diagram.diagram(abcdeTreeHashSeqMap).render("TreeHashSeqMap")
Diagram.diagram(abcdeTreeSeqMap).render("TreeSeqMap")
Diagram.diagram(abcdeKeyLinkedSeqMap).render("KeyLinkedSeqMap")
Diagram.diagram(abcdeVectorMap).render("VectorMap")
Diagram.diagram(abcdeListMap).render("ListMap")


def generateWithCaption[M <: collection.Map[Char, Char] : ToRefTree](m0: (M, String), n: Int) (generator: (M, Int) => (M, String)): IndexedSeq[(M, String)] = {
  (0 until n).scanLeft(m0) {
    case ((m, c), i) =>
      val (m2, c2) = generator(m, i)
      (m2, c + c2)
  }
}

def adding[M <: collection.Map[Char, Char] : ToRefTree](m0: (M, String), n: Int) (generator: (M, Int) => (M, String)): Unit = {
  (generateWithCaption(m0, n)(generator).takeRight(2).toList match {
    case (m, c) :: Nil =>
      diagram(m).withCaption(c)
    case (m1, c1) :: (m2, c2) :: Nil =>
      diagram(m1).withCaption(c1) + diagram(m2).withCaption(c2)
    case _ =>
      diagram(m0._1).withCaption(m0._2)
  }).render(s"${className(m0._1)}-adding-$n")
}
def removing[M <: collection.Map[Char, Char] : ToRefTree](m0: (M, String), n: Int)(generator: (M, Int) => (M, String)): Unit = {
  generateWithCaption(m0, n)(generator).take(2).foldLeft[Diagram](Diagram.empty) {
    case (d, (m, c)) =>
      d + diagram(m).withCaption(c)
  }.render(s"${className(m0._1)}-removing-$n")
}

adding(QueueSeqMap.empty[Char, Char] -> "Ø", 5) {
  case (m, i) =>
    val a = ('A' + i).toChar
    val z = ('z' - i).toChar
    (m + (a -> z), s"+('$a'->'$z')")
}
adding(LinkedHashMap.empty[Char, Char] -> "Ø", 5) {
  case (m, i) =>
    val a = ('A' + i).toChar
    val z = ('z' - i).toChar
    (m += (a -> z), s"+('$a'->'$z')")
}
adding(ListMap.empty[Char, Char] -> "Ø", 5) {
  case (m, i) =>
    val a = ('A' + i).toChar
    val z = ('z' - i).toChar
    (m + (a -> z), s"+($a->$z)")
}
adding(VectorMap.empty[Char, Char] -> "Ø", 5) {
  case (m, i) =>
    val a = ('A' + i).toChar
    val z = ('z' - i).toChar
    (m + (a -> z), s"+('$a'->'$z')")
}
adding(TreeSeqMap.empty[Char, Char] -> "Ø", 5) {
  case (m, i) =>
    val a = ('A' + i).toChar
    val z = ('z' - i).toChar
    (m + (a -> z), s"+('$a'->'$z')")
}
adding(TreeHashSeqMap.empty[Char, Char] -> "Ø", 5) {
  case (m, i) =>
    val a = ('A' + i).toChar
    val z = ('z' - i).toChar
    (m + (a -> z), s"+('$a'->'$z')")
}
adding(KeyLinkedSeqMap.empty[Char, Char] -> "Ø", 5) {
  case (m, i) =>
    val a = ('A' + i).toChar
    val z = ('z' - i).toChar
    (m + (a -> z), s"+('$a'->'$z')")
}

removing(abcdeQueueSeqMap -> "ABCDE", 3) { (m, i) =>
  val c = i match {
    case 0 => 'B'
    case 1 => 'C'
    case 2 => 'D'
  }
  (m - c, " -" + c)
}
removing(abcdeLinkedHashMap -> "ABCDE", 3) { (m, i) =>
  val c = i match {
    case 0 => 'B'
    case 1 => 'C'
    case 2 => 'D'
  }
  (m -= c, " -" + c)
}
removing(abcdeListMap -> "ABCDE", 3) { (m, i) =>
  val c = i match {
    case 0 => 'B'
    case 1 => 'C'
    case 2 => 'D'
  }
  (m - c, " -" + c)
}
removing(abcdeVectorMap -> "ABCDE", 3) { (m, i) =>
  val c = i match {
    case 0 => 'B'
    case 1 => 'C'
    case 2 => 'D'
  }
  (m - c, " -" + c)
}
removing(abcdeTreeSeqMap -> "ABCDE", 3) { (m, i) =>
  val c = i match {
    case 0 => 'B'
    case 1 => 'C'
    case 2 => 'D'
  }
  (m - c, " -" + c)
}
removing(abcdeTreeHashSeqMap -> "ABCDE", 3) { (m, i) =>
  val c = i match {
    case 0 => 'B'
    case 1 => 'C'
    case 2 => 'D'
  }
  (m - c, " -" + c)
}
removing(abcdeKeyLinkedSeqMap -> "ABCDE", 3) { (m, i) =>
  val c = i match {
    case 0 => 'B'
    case 1 => 'C'
    case 2 => 'D'
  }
  (m - c, " -" + c)
}



