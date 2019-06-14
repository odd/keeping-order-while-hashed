package odd
package orderkeeping

import scala.annotation.tailrec
import scala.collection.generic.{CanBuildFrom, ImmutableMapFactory, MapFactory}
import scala.collection.{immutable, mutable}
import scala.collection.immutable.{AbstractMap, HashMap, MapLike}

final class TreeSeqMap[K, +V] private (
    private val ordering: TreeSeqMap.Ordering[K],
    private val mapping: TreeSeqMap.Mapping[K, V],
    private val ordinal: Int,
    val orderedBy: TreeSeqMap.OrderBy)
  extends AbstractMap[K, V]
      with immutable.Map[K, V]
      with MapLike[K, V, TreeSeqMap[K, V]]
      with Serializable {

  import TreeSeqMap._

  override def stringPrefix = "TreeSeqMap"

  override val size = mapping.size

  override def isEmpty = size == 0

  override def empty = TreeSeqMap.empty[K, V]

  def orderingBy(orderBy: OrderBy): TreeSeqMap[K, V] = {
    if (orderBy == this.orderedBy) this
    else new TreeSeqMap(ordering, mapping, ordinal, orderBy)
  }

  override def updated[V1 >: V](key: K, value: V1): TreeSeqMap[K, V1] = {
    mapping.get(key) match {
      /*
      case e if ordinal == -1 && (orderedBy == OrderBy.Modification || e.isEmpty) =>
        // Reinsert into fresh instance to restart ordinal counting, expensive but only done after 2^32 updates.
        TreeSeqMap.empty[K, V](orderedBy) ++ (this + (key -> value))
        */
      case Some((o, _)) if orderedBy == OrderBy.Insertion =>
        new TreeSeqMap(
          ordering.include(o, key),
          mapping.updated[(Int, V1)](key, (o, value)),
          ordinal, // Do not increment the ordinal since the key is already present, i.e. o <= ordinal.
          orderedBy)
      case Some((o, _)) =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.exclude(o).append(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
      case None =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.append(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
    }
  }

  def removed(key: K): TreeSeqMap[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) =>
        new TreeSeqMap(
          ordering.exclude(o),
          mapping - key,
          ordinal,
          orderedBy)
      case None =>
        this
    }
  }
  override def +[V1 >: V](kv: (K, V1)): TreeSeqMap[K, V1] = {
    val (key, value) = kv
    mapping.get(key) match {
      /*
      case e if ordinal == -1 && (orderedBy == OrderBy.Modification || e.isEmpty) =>
        // Reinsert into fresh instance to restart ordinal counting, expensive but only done after 2^32 updates.
        TreeSeqMap.empty[K, V](orderedBy) ++ (this + (key -> value))
        */
      case Some((o, _)) if orderedBy == OrderBy.Insertion =>
        new TreeSeqMap(
          ordering.include(o, key),
          mapping + (key -> (o, value)),
          ordinal, // Do not increment the ordinal since the key is already present, i.e. o <= ordinal.
          orderedBy)
      case Some((o, _)) =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.exclude(o).append(o1, key),
          mapping + (key -> (o1, value)),
          o1,
          orderedBy)
      case None =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.append(o1, key),
          mapping + (key -> (o1, value)),
          o1,
          orderedBy)
    }
  }
  override def -(key: K): TreeSeqMap[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) =>
        new TreeSeqMap(
          ordering.exclude(o),
          mapping - key,
          ordinal,
          orderedBy)
      case None =>
        this
    }
  }
  def refresh(key: K): TreeSeqMap[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) =>
        val o1 = increment(ordinal)
        new TreeSeqMap(
          ordering.exclude(o).append(o1, key),
          mapping,
          o1,
          orderedBy)
      case None =>
        this
    }
  }

  def get(key: K): Option[V] = mapping.get(key).map(value)

  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    private[this] val iter = ordering.iterator
    override def hasNext: Boolean = iter.hasNext
    override def next(): (K, V) = binding(iter.next())
  }
  /*
  override def keysIterator: Iterator[K] = new Iterator[K] {
    private[this] val iter = ordering.iterator
    override def hasNext: Boolean = iter.hasNext
    override def next(): K = iter.next()
  }

  override def valuesIterator: Iterator[V] = new Iterator[V] {
    private[this] val iter = ordering.iterator
    override def hasNext: Boolean = iter.hasNext
    override def next(): V = value(binding(iter.next()))
  }

  override def contains(key: K): Boolean = mapping.contains(key)

  override def head: (K, V) = binding(ordering.head)

  override def headOption = ordering.headOption.map(binding)

  override def last: (K, V) = binding(ordering.last)

  override def lastOption: Option[(K, V)] = ordering.lastOption.map(binding)

  override def tail: TreeSeqMap[K, V] = {
    val (head, tail) = ordering.headTail
    new TreeSeqMap(tail, mapping - head, ordinal, orderedBy)
  }

  override def init: TreeSeqMap[K, V] = {
    val (init, last) = ordering.initLast
    new TreeSeqMap(init, mapping - last, ordinal, orderedBy)
  }

  override def slice(from: Int, until: Int): TreeSeqMap[K, V] = {
    val sz = size
    if (sz == 0 || from >= until) TreeSeqMap.empty
    else {
      val sz = size
      val f = if (from >= 0) from else 0
      val u = if (until <= sz) until else sz
      val l = u - f
      if (l <= 0) TreeSeqMap.empty
      else if (l > sz / 2) {
        // Remove front and rear incrementally if majority of elements are to be kept
        val (front, rest) = ordering.splitAt(f)
        val (ong, rear) = rest.splitAt(l)
        var mng = this.mapping
        val frontIter = front.iterator
        while (frontIter.hasNext) {
          mng = mng - frontIter.next()
        }
        val rearIter = rear.iterator
        while (rearIter.hasNext) {
          mng = mng - rearIter.next()
        }
        new TreeSeqMap(ong, mng, ordinal, orderedBy)
      } else {
        // Populate with builder otherwise
        val bdr = TreeSeqMap.newBuilder[K, V](orderedBy)
        val iter = ordering.iterator
        var i = 0
        while (i < f) {
          iter.next()
          i += 1
        }
        while (i < u) {
          val k = iter.next()
          bdr += (k -> mapping(k)._2)
          i += 1
        }
        bdr.result()
      }
    }
  }

  override def map[K2, V2](f: ((K, V)) => (K2, V2)): TreeSeqMap[K2, V2] = {
    val bdr = TreeSeqMap.newBuilder[K2, V2](orderedBy)
    val iter = ordering.iterator
    while (iter.hasNext) {
      val k = iter.next()
      val (_, v) = mapping(k)
      val (k2, v2) = f((k, v))
      bdr += (k2 -> v2)
    }
    bdr.result()
  }

  override def flatMap[K2, V2](f: ((K, V)) => TraversableOnce[(K2, V2)]): TreeSeqMap[K2, V2] = {
    val bdr = TreeSeqMap.newBuilder[K2, V2](orderedBy)
    val iter = ordering.iterator
    while (iter.hasNext) {
      val k = iter.next()
      val (_, v) = mapping(k)
      val jter = f((k, v)).iterator
      while (jter.hasNext) {
        val (k2, v2) = jter.next()
        bdr.addOne(k2, v2)
      }
    }
    bdr.result()
  }

  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]): TreeSeqMap[K2, V2] = {
    val bdr = newBuilder[K2, V2](orderedBy)
    val iter = ordering.iterator
    while (iter.hasNext) {
      val k = iter.next()
      val (_, v) = mapping(k)
      if (pf.isDefinedAt((k, v))) {
        val (k2, v2) = pf((k, v))
        bdr.addOne(k2, v2)
      }
    }
    bdr.result()
  }

  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]): TreeSeqMap[K, V2] = {
    var ong: Ordering[K] = ordering
    var mng: Mapping[K, V2] = mapping
    var ord = increment(ordinal)
    val iter = suffix.iterator
    while (iter.hasNext) {
      val (k, v2) = iter.next()
      mng.get(k) match {
        case Some((o, v)) =>
          if (orderedBy == OrderBy.Insertion && v != v2) mng = mng.updated(k, (o, v2))
          else if (orderedBy == OrderBy.Modification) {
            mng = mng.updated(k, (ord, v2))
            ong = ong.exclude(o).append(ord, k)
            ord = increment(ord)
          }
        case None =>
          mng = mng.updated(k, (ord, v2))
          ong = ong.append(ord, k)
          ord = increment(ord)
      }
    }
    new TreeSeqMap[K, V2](ong, mng, ord, orderedBy)
  }
  */

  @`inline` private[this] def value(p: (_, V)) = p._2
  @`inline` private[this] def binding(k: K) = mapping(k).copy(_1 = k)
}
object TreeSeqMap extends ImmutableMapFactory[TreeSeqMap] {
  implicit def canBuildFrom[K, V]: CanBuildFrom[TreeSeqMap[_, _], (K, V), TreeSeqMap[K, V]] = new MapCanBuildFrom[K, V]

  sealed trait OrderBy
  object OrderBy {
    case object Insertion extends OrderBy
    case object Modification extends OrderBy
  }

  val Empty = new TreeSeqMap[Nothing, Nothing](Ordering.empty, HashMap.empty, 0, OrderBy.Insertion)

  def empty[K, V]: TreeSeqMap[K, V] = Empty.asInstanceOf[TreeSeqMap[K, V]]
  /*def empty[K, V]: TreeSeqMap[K, V] = Empty.asInstanceOf[TreeSeqMap[K, V]]
  def empty[K, V](orderBy: OrderBy): TreeSeqMap[K, V] = Empty.asInstanceOf[TreeSeqMap[K, V]]*/

  /*
  def from[K, V](it: collection.IterableOnce[(K, V)]): TreeSeqMap[K, V] =
    it match {
      case om: TreeSeqMap[K, V] => om
      case _ => (newBuilder[K, V] ++= it).result()
    }
  */

  @inline private def increment(ord: Int) = if (ord == Int.MaxValue) Int.MinValue else ord + 1

  /*
  override def newBuilder[K, V]: Builder[(K, V), TreeSeqMap[K, V]] = TreeSeqMap.newBuilder(OrderBy.Insertion)
  def newBuilder[K, V](orderedBy: OrderBy): Builder[(K, V), TreeSeqMap[K, V]] = new TreeSeqMap.Builder[K, V](orderedBy)

  final class Builder[K, V](orderedBy: OrderBy) extends mutable.Builder[(K, V), TreeSeqMap[K, V]] {
    private[this] val bdr = new MapBuilderImpl[K, (Int, V)]
    private[this] var ong = Ordering.empty[K]
    private[this] var ord = 0
    private[this] var aliased: TreeSeqMap[K, V] = _

    override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
    def addOne(key: K, value: V): this.type = {
      if (aliased ne null) {
        aliased = aliased.updated(key, value)
      } else {
        bdr.getOrElse(key, null) match {
          case (o, v) =>
            if (orderedBy == OrderBy.Insertion && v != value) bdr.addOne(key, (o, value))
            else if (orderedBy == OrderBy.Modification) {
              bdr.addOne(key, (ord, value))
              ong = ong.exclude(o).appendInPlace(ord, key)
              ord = increment(ord)
            }
          case null =>
            bdr.addOne(key, (ord, value))
            ong = ong.appendInPlace(ord, key)
            ord = increment(ord)
        }
      }
      this
    }

    override def clear(): Unit = {
      ong = Ordering.empty
      ord = 0
      bdr.clear()
      aliased = null
    }

    override def result(): TreeSeqMap[K, V] = {
      if (aliased eq null) {
        aliased = new TreeSeqMap(ong, bdr.result(), ord, orderedBy)
      }
      aliased
    }
  }
  */

  private type Mapping[K, +V] = Map[K, (Int, V)]
  private val Mapping = Map

  private object BitOperations {
    trait Int {
      type Int = scala.Int
      def zero(i: Int, mask: Int) = (i & mask) == 0
      def mask(i: Int, mask: Int) = i & (complement(mask - 1) ^ mask)
      def hasMatch(key: Int, prefix: Int, m: Int) = mask(key, m) == prefix
      def unsignedCompare(i: Int, j: Int) = (i < j) ^ (i < 0) ^ (j < 0)
      def shorter(m1: Int, m2: Int) = unsignedCompare(m2, m1)
      def complement(i: Int) = (-1) ^ i
      def bits(num: Int) = 31 to 0 by -1 map (i => (num >>> i & 1) != 0)
      def bitString(num: Int, sep: String = "") = bits(num) map (b => if (b) "1" else "0") mkString sep
      def highestOneBit(j: Int) = java.lang.Integer.highestOneBit(j)
    }
    object Int extends Int
  }
  import BitOperations.Int.{highestOneBit, zero, mask, hasMatch}

  /* The ordering implementation below is an adapted version of immutable.IntMap. */
  sealed abstract class Ordering[+T] {
    import Ordering._
    import scala.annotation.tailrec

    override final def toString: String = format
    final def format: String = {
      val sb = new StringBuilder
      format(sb, "", "")
      sb.toString()
    }
    protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit

    @tailrec
    final def head: T = this match {
      case Zero => throw new NoSuchElementException("head of empty map")
      case Tip(k, v) => v
      case Bin(_, _, l, _) => l.head
    }

    @tailrec
    final def headOption: Option[T] = this match {
      case Zero => None
      case Tip(_, v) => Some(v)
      case Bin(_, _, l, _) => l.headOption
    }

    @tailrec
    final def last: T = this match {
      case Zero => throw new NoSuchElementException("last of empty map")
      case Tip(_, v) => v
      case Bin(_, _, _, r) => r.last
    }

    @tailrec
    final def lastOption: Option[T] = this match {
      case Zero => None
      case Tip(_, v) => Some(v)
      case Bin(_, _, _, r) => r.lastOption
    }

    @tailrec
    final def ordinal: Int = this match {
      case Zero => 0
      case Tip(o, _) => o
      case Bin(_, _, _, r) => r.ordinal
    }

    final def tail: Ordering[T] = this match {
      case Zero => throw new NoSuchElementException("tail of empty map")
      case Tip(_, _) => Zero
      case Bin(p, m, l, r) => bin(p, m, l.tail, r)
    }

    final def headTail: (T, Ordering[T]) = this match {
      case Zero => throw new NoSuchElementException("init of empty map")
      case Tip(_, v) => (v, Zero)
      case Bin(p, m, l, r) =>
        val (head, tail) = l.headTail
        (head, bin(p, m, tail, r))
    }

    final def init: Ordering[T] = this match {
      case Zero => throw new NoSuchElementException("init of empty map")
      case Tip(_, _) => Zero
      case Bin(p, m, l, r) =>
        bin(p, m, l, r.init)
    }

    final def initLast: (Ordering[T], T) = this match {
      case Zero => throw new NoSuchElementException("init of empty map")
      case Tip(_, v) => (Zero, v)
      case Bin(p, m, l, r) =>
        val (init, last) = r.initLast
        (bin(p, m, l, init), last)
    }

    final def iterator: Iterator[T] = this match {
      case Zero => Iterator.empty
      case _ => new Iterator(this)
    }

    final def include[S >: T](ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) =>
        if (ordinal == o) Tip(ordinal, value)
        else join(ordinal, Tip(ordinal, value), o, this)
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) join(ordinal, Tip(ordinal, value), p, this)
        else if (zero(ordinal, m)) Bin(p, m, l.include(ordinal, value), r)
        else Bin(p, m, l, r.include(ordinal, value))
    }

    final def append[S >: T](ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) =>
        if (ordinal == o) Tip(ordinal, value)
        else join(ordinal, Tip(ordinal, value), o, this)
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) join(ordinal, Tip(ordinal, value), p, this)
        else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else Bin(p, m, l, r.append(ordinal, value))
    }

    @inline private final def appendInPlace[S >: T](ordinal: Int, value: S): Ordering[S] = appendInPlace1(null, ordinal, value)
    private final def appendInPlace1[S >: T](parent: Bin[S], ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) if o >= ordinal =>
        throw new IllegalArgumentException(s"Append called with ordinal out of range: $o is not greater than current max ordinal ${this.ordinal}")
      case Tip(o, _) if parent == null =>
        join(ordinal, Tip(ordinal, value), o, this)
      case Tip(o, _) =>
        parent.right = join(ordinal, Tip(ordinal, value), o, this)
        parent
      case b @ Bin(p, m, _, r) =>
        if (!hasMatch(ordinal, p, m)) {
          val b2 = join(ordinal, Tip(ordinal, value), p, this)
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          r.appendInPlace1(b, ordinal, value)
          this
        }
    }

    final def exclude(ordinal: Int): Ordering[T] = this match {
      case Zero =>
        Zero
      case Tip(o, _) =>
        if (ordinal == o) Zero
        else this
      case Bin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) this
        else if (zero(ordinal, m)) bin(p, m, l.exclude(ordinal), r)
        else bin(p, m, l, r.exclude(ordinal))
    }

    final def splitAt(n: Int): (Ordering[T], Ordering[T]) = {
      var rear = Ordering.empty[T]
      var i = n
      (modifyOrRemove { (o, v) =>
        i -= 1
        if (i >= 0) Some(v)
        else {
          rear = rear.appendInPlace(o, v)
          None
        }
      }, rear)
    }

    /**
     * A combined transform and filter function. Returns an `Ordering` such that
     * for each `(key, value)` mapping in this map, if `f(key, value) == None`
     * the map contains no mapping for key, and if `f(key, value) == Some(x)` the
     * map contains `(key, x)`.
     *
     * @tparam S  The type of the values in the resulting `LongMap`.
     * @param f   The transforming function.
     * @return    The modified map.
     */
    final def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = this match {
      case Zero => Zero
      case Tip(key, value) =>
        f(key, value) match {
          case None => Zero
          case Some(value2) =>
            // hack to preserve sharing
            if (value.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) this.asInstanceOf[Ordering[S]]
            else Tip(key, value2)
        }
      case Bin(prefix, mask, left, right) =>
        val l = left.modifyOrRemove(f)
        val r = right.modifyOrRemove(f)
        if ((left eq l) && (right eq r)) this.asInstanceOf[Ordering[S]]
        else bin(prefix, mask, l, r)
    }
  }
  import reftree.core._
  import reftree.contrib.SimplifiedInstances.{string}
  object Ordering {
    @inline private def toBinaryString(i: Int): String = s"$i/${i.toBinaryString}"

    def empty[T] : Ordering[T] = Zero

    def apply[T](elems: (Int, T)*): Ordering[T] =
      elems.foldLeft(empty[T])((x, y) => x.include(y._1, y._2))

    // Iterator over a non-empty Ordering.
    final class Iterator[+V](it: Ordering[V]) {
      // Basically this uses a simple stack to emulate conversion over the tree. However
      // because we know that Ints are at least 32 bits we can have at most 32 Bins and
      // one Tip sitting on the tree at any point. Therefore we know the maximum stack
      // depth is 33
      private[this] var index = 0
      private[this] val buffer = new Array[AnyRef](33)

      private[this] def pop = {
        index -= 1
        buffer(index).asInstanceOf[Ordering[V]]
      }

      private[this] def push[V2 >: V](x: Ordering[V2]): Unit = {
        buffer(index) = x.asInstanceOf[AnyRef]
        index += 1
      }

      if (it != Zero) push(it)

      def hasNext = index != 0
      @tailrec
      def next(): V =
        pop match {
          case Bin(_,_, Tip(_, v), right) =>
            push(right)
            v
          case Bin(_, _, left, right) =>
            push(right)
            push(left)
            next()
          case Tip(_, v) => v
          // This should never happen. We don't allow Ordering.Zero in subtrees of the Ordering
          // and don't return an Ordering.Iterator for Ordering.Zero.
          case Zero => throw new IllegalStateException("empty subtree not allowed")
        }
    }

    object Iterator {
      val Empty = new Iterator[Nothing](Ordering.empty[Nothing])
      def empty[V]: Iterator[V] = Empty.asInstanceOf[Iterator[V]]
    }

    case object Zero extends Ordering[Nothing] {
      // Important! Without this equals method in place, an infinite
      // loop from Map.equals => size => pattern-match-on-Nil => equals
      // develops.  Case objects and custom equality don't mix without
      // careful handling.
      override def equals(that : Any): Boolean = that match {
        case _: this.type => true
        case _: Ordering[_] => false // The only empty Orderings are eq Nil
        case _ => super.equals(that)
      }
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = sb ++= s"${prefix}Ø"
    }

    final case class Tip[+T](ord: Int, value: T) extends Ordering[T] {
      def withValue[S](s: S) =
        if (s.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[Tip[S]]
        else Tip(ord, s)
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = sb ++= s"${prefix}Tip(${toBinaryString(ord)} -> $value)\n"
    }

    final case class Bin[+T](prefix: Int, mask: Int, left: Ordering[T], var right: Ordering[T] @scala.annotation.unchecked.uncheckedVariance) extends Ordering[T] {
      def bin[S](left: Ordering[S], right: Ordering[S]): Ordering[S] = {
        if ((this.left eq left) && (this.right eq right)) this.asInstanceOf[Bin[S]]
        else Bin[S](prefix, mask, left, right)
      }
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = {
        sb ++= s"${prefix}Bin(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        left.format(sb, subPrefix + "├── ", subPrefix + "│   ")
        right.format(sb, subPrefix + "└── ", subPrefix + "    ")
      }
    }

    private def branchMask(i: Int, j: Int) = highestOneBit(i ^ j)

    private def join[T](p1: Int, t1: Ordering[T], p2: Int, t2: Ordering[T]): Ordering[T] = {
      val m = branchMask(p1, p2)
      val p = mask(p1, m)
      if (zero(p1, m)) Bin(p, m, t1, t2)
      else Bin(p, m, t2, t1)
    }

    private def bin[T](prefix: Int, mask: Int, left: Ordering[T], right: Ordering[T]): Ordering[T] = (left, right) match {
      case (l, Zero) => l
      case (Zero, r) => r
      case (l, r) => Bin(prefix, mask, l, r)
    }

    implicit def `TreeSeqMap.Ordering RefTree`[T : ToRefTree]: ToRefTree[TreeSeqMap.Ordering[T]] = { ordering: TreeSeqMap.Ordering[T] =>
      ordering match {
        case Zero =>
          RefTree.Ref(ordering, Seq.empty).rename("∅")
        case Tip(o, v) =>
          RefTree.Ref(ordering, Seq(
            o.refTree.toField.withName("ordinal"),
            v.refTree.toField.withName("value"))
          ).rename("Tip")
        case Bin(prefix, mask, left, right) =>
          RefTree.Ref(ordering, Seq(
            prefix.toBinaryString.toLong.refTree.toField.withName("prefix"),
            mask.toBinaryString.toLong.refTree.toField.withName("mask"),
            left.refTree.toField.withName("left"),
            right.refTree.toField.withName("right"))
          ).rename("Bin")
      }
    }
  }
  implicit def `TreeSeqMap RefTree`[K : ToRefTree, V : ToRefTree]: ToRefTree[TreeSeqMap[K, V]] = { map: TreeSeqMap[K, V] =>
    RefTree.Ref(map, Seq(
      map.ordering.refTree.toField.withName("ordering"),
      mappingStub[K, (Int, V)].refTree(map.mapping).toField.withName("mapping"))
    ).rename("TreeSeqMap")
  }
}
