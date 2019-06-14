/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

/** This class implements an immutable map that preserves order using
  * a hash map for the key to value mapping to provide efficient lookup,
  * and a tree for the ordering of the keys to provide efficient
  * insertion/modification order traversal and destructuring.
  *
  * By default insertion order (`TreeSeqMapO.OrderBy.Insertion`)
  * is used, but modification order (`TreeSeqMapO.OrderBy.Modification`)
  * can be used instead if so specified at creation.
  *
  * The `orderingBy(orderBy: TreeSeqMapO.OrderBy): TreeSeqMapO[K, V]` method
  * can be used to switch to the specified ordering for the returned map.
  *
  * A key can be manually refreshed (i.e. placed at the end) via the
  * `refresh(key: K): TreeSeqMapO[K, V]` method (regardless of the ordering in
  * use).
  *
  * Internally, an ordinal counter is increased for each insertion/modification
  * and then the current ordinal is used as key in the tree map. After 2^32^
  * insertions/modifications the entire map is copied (thus resetting the ordinal
  * counter).
  *
  *  @tparam K the type of the keys contained in this map.
  *  @tparam V the type of the values associated with the keys in this map.
  * @version 2.13
  * @since 2.13
  * @define coll immutable tree seq map
  * @define Coll `immutable.TreeSeqMapO`
  */
final class TreeSeqMapO[K, +V] private (
        private val ordering: TreeSeqMapO.Ordering[K],
        private val mapping: TreeSeqMapO.Mapping[K, V],
        private val ordinal: Int,
        val orderedBy: TreeSeqMapO.OrderBy)
    extends AbstractMap[K, V]
        with SeqMap[K, V]
        with MapOps[K, V, TreeSeqMapO, TreeSeqMapO[K, V]]
        with StrictOptimizedIterableOps[(K, V), Iterable, TreeSeqMapO[K, V]]
        with StrictOptimizedMapOps[K, V, TreeSeqMapO, TreeSeqMapO[K, V]]
        with MapFactoryDefaults[K, V, TreeSeqMapO, Iterable] {

  import TreeSeqMapO._

  override protected[this] def className: String = "TreeSeqMapO"

  override def mapFactory: MapFactory[TreeSeqMapO] = TreeSeqMapO

  override val size = mapping.size

  override def knownSize: Int = size

  override def isEmpty = size == 0

  def orderingBy(orderBy: OrderBy): TreeSeqMapO[K, V] = {
    if (orderBy == this.orderedBy) this
    else new TreeSeqMapO(ordering, mapping, ordinal, orderBy)
  }

  def updated[V1 >: V](key: K, value: V1): TreeSeqMapO[K, V1] = {
    mapping.get(key) match {
      case e if ordinal == -1 && (orderedBy == OrderBy.Modification || e.isEmpty) =>
        // Reinsert into fresh instance to restart ordinal counting, expensive but only done after 2^32 updates.
        TreeSeqMapO.empty[K, V](orderedBy) ++ this + (key -> value)
      case Some((o, _)) if orderedBy == OrderBy.Insertion =>
        new TreeSeqMapO(
          ordering.include(o, key),
          mapping.updated[(Int, V1)](key, (o, value)),
          ordinal, // Do not increment the ordinal since the key is already present, i.e. o <= ordinal.
          orderedBy)
      case Some((o, _)) =>
        val o1 = increment(ordinal)
        new TreeSeqMapO(
          ordering.exclude(o).append(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
      case None =>
        val o1 = increment(ordinal)
        new TreeSeqMapO(
          ordering.append(o1, key),
          mapping.updated[(Int, V1)](key, (o1, value)),
          o1,
          orderedBy)
    }
  }

  def removed(key: K): TreeSeqMapO[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) =>
        new TreeSeqMapO(
          ordering.exclude(o),
          mapping.removed(key),
          ordinal,
          orderedBy)
      case None =>
        this
    }
  }

  override def removedAll(keys: IterableOnce[K]): TreeSeqMapO[K, V] = {
    var ong = ordering
    var mng = mapping
    val iter = keys.iterator
    while (iter.hasNext) {
      val key = iter.next()
      mapping.get(key) match {
        case Some((o, _)) =>
          ong = ong.exclude(o)
          mng = mng.removed(key)
        case None =>
      }
    }
    new TreeSeqMapO(ong, mng, ordinal, orderedBy)
  }

  def refresh(key: K): TreeSeqMapO[K, V] = {
    mapping.get(key) match {
      case Some((o, _)) =>
        val o1 = increment(ordinal)
        new TreeSeqMapO(
          ordering.exclude(o).append(o1, key),
          mapping,
          o1,
          orderedBy)
      case None =>
        this
    }
  }

  def get(key: K): Option[V] = mapping.get(key).map(value)

  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): (K, V) = binding(iter.next())
  }

  override def keysIterator: Iterator[K] = new AbstractIterator[K] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): K = iter.next()
  }

  override def valuesIterator: Iterator[V] = new AbstractIterator[V] {
    private[this] val iter = ordering.iterator

    override def hasNext: Boolean = iter.hasNext

    override def next(): V = value(binding(iter.next()))
  }

  override def contains(key: K): Boolean = mapping.contains(key)

  override def head: (K, V) = binding(ordering.head)

  override def headOption = ordering.headOption.map(binding)

  override def last: (K, V) = binding(ordering.last)

  override def lastOption: Option[(K, V)] = ordering.lastOption.map(binding)

  override def tail: TreeSeqMapO[K, V] = {
    val (head, tail) = ordering.headTail
    new TreeSeqMapO(tail, mapping.removed(head), ordinal, orderedBy)
  }

  override def init: TreeSeqMapO[K, V] = {
    val (init, last) = ordering.initLast
    new TreeSeqMapO(init, mapping.removed(last), ordinal, orderedBy)
  }

  override def slice(from: Int, until: Int): TreeSeqMapO[K, V] = {
    val sz = size
    if (sz == 0 || from >= until) TreeSeqMapO.empty
    else {
      val sz = size
      val f = if (from >= 0) from else 0
      val u = if (until <= sz) until else sz
      val l = u - f
      if (l <= 0) TreeSeqMapO.empty
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
        new TreeSeqMapO(ong, mng, ordinal, orderedBy)
      } else {
        // Populate with builder otherwise
        val bdr = newBuilder[K, V](orderedBy)
        val iter = ordering.iterator
        var i = 0
        while (i < f) {
          iter.next()
          i += 1
        }
        while (i < u) {
          val k = iter.next()
          bdr.addOne(k, mapping(k)._2)
          i += 1
        }
        bdr.result()
      }
    }
  }

  override def map[K2, V2](f: ((K, V)) => (K2, V2)): TreeSeqMapO[K2, V2] = {
    val bdr = newBuilder[K2, V2](orderedBy)
    val iter = ordering.iterator
    while (iter.hasNext) {
      val k = iter.next()
      val (_, v) = mapping(k)
      val (k2, v2) = f((k, v))
      bdr.addOne(k2, v2)
    }
    bdr.result()
  }

  override def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): TreeSeqMapO[K2, V2] = {
    val bdr = newBuilder[K2, V2](orderedBy)
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

  override def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)]): TreeSeqMapO[K2, V2] = {
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

  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]): TreeSeqMapO[K, V2] = {
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
    new TreeSeqMapO[K, V2](ong, mng, ord, orderedBy)
  }

  @`inline` private[this] def value(p: (_, V)) = p._2
  @`inline` private[this] def binding(k: K) = mapping(k).copy(_1 = k)
}
object TreeSeqMapO extends MapFactory[TreeSeqMapO] {
  sealed trait OrderBy
  final object OrderBy {
    final case object Insertion extends OrderBy
    final case object Modification extends OrderBy
  }

  val Empty = new TreeSeqMapO[Nothing, Nothing](Ordering.empty, HashMap.empty, 0, OrderBy.Insertion)
  def empty[K, V]: TreeSeqMapO[K, V] = empty(OrderBy.Insertion)
  def empty[K, V](orderBy: OrderBy): TreeSeqMapO[K, V] = Empty.asInstanceOf[TreeSeqMapO[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): TreeSeqMapO[K, V] =
    it match {
      case om: TreeSeqMapO[K, V] => om
      case _ => (newBuilder[K, V] ++= it).result()
    }

  @inline private def increment(ord: Int) = if (ord == Int.MaxValue) Int.MinValue else ord + 1

  def newBuilder[K, V]: mutable.Builder[(K, V), TreeSeqMapO[K, V]] = newBuilder(OrderBy.Insertion)
  def newBuilder[K, V](orderedBy: OrderBy): mutable.Builder[(K, V), TreeSeqMapO[K, V]] = new Builder[K, V](orderedBy)

  final class Builder[K, V](orderedBy: OrderBy) extends mutable.Builder[(K, V), TreeSeqMapO[K, V]] {
    private[this] val bdr = new MapBuilderImpl[K, (Int, V)]
    private[this] var ong = Ordering.empty[K]
    private[this] var ord = 0
    private[this] var aliased: TreeSeqMapO[K, V] = _

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

    override def result(): TreeSeqMapO[K, V] = {
      if (aliased eq null) {
        aliased = new TreeSeqMapO(ong.cleanInPlace(), bdr.result(), ord, orderedBy)
      }
      aliased
    }
  }

  private type Mapping[K, +V] = Map[K, (Int, V)]
  private val Mapping = Map

  /* The ordering implementation below is an adapted version of immutable.IntMap. */
  sealed abstract class Ordering[+T] {
    import Ordering._
    import scala.annotation.tailrec
    import scala.collection.generic.BitOperations.Int._

    override final def toString: String = format
    final def format: String = {
      val sb = new StringBuilder
      format(sb, "", "")
      sb.toString()
    }
    protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit

    def ordinal: Int
    /*final def ordinal: Int = this match {
      case Zero => 0
      case Tip(ord, _) => ord
      case TipTip(_, _, _, _, ro, _) => ro
      case TipBin(_, _, _, _, r) => r.ordinal
      case BinTip(_, _, _, ro, _) => ro
      case BinBin(_, _, _, r) => r.ordinal
    }*/

    def head: T
    /*@tailrec
    final def head: T = this match {
      case Zero => throw new NoSuchElementException("head of empty map")
      case Tip(_, v) => v
      case TipTip(_, _, _, lv, _, _) => lv
      case TipBin(_, _, _, lv, _) => lv
      case BinTip(_, _, l, _, _) => l.head
      case BinBin(_, _, l, _) => l.head
    }*/

    def headOption: Option[T]
    /*@tailrec
    final def headOption: Option[T] = this match {
      case Zero => None
      case Tip(_, v) => Some(v)
      case TipTip(_, _, _, lv, _, _) => Some(lv)
      case TipBin(_, _, _, lv, _) => Some(lv)
      case BinTip(_, _, l, _, _) => l.headOption
      case BinBin(_, _, l, _) => l.headOption
    }*/

    def last: T
    /*@tailrec
    final def last: T = this match {
      case Zero => throw new NoSuchElementException("last of empty map")
      case Tip(_, v) => v
      case TipTip(_, _, _, _, _, rv) => rv
      case TipBin(_, _, _, _, r) => r.last
      case BinTip(_, _, _, _, rv) => rv
      case BinBin(_, _, _, r) => r.last
    }*/

    def lastOption: Option[T]
    /*@tailrec
    final def lastOption: Option[T] = this match {
      case Zero => None
      case Tip(_, v) => Some(v)
      case TipTip(_, _, _, _, _, rv) => Some(rv)
      case TipBin(_, _, _, _, r) => r.lastOption
      case BinTip(_, _, _, _, rv) => Some(rv)
      case BinBin(_, _, _, r) => r.lastOption
    }*/

    def tail: Ordering[T]
    /*final def tail: Ordering[T] = this match {
      case Zero => throw new NoSuchElementException("tail of empty map")
      case Tip(_, _) => Zero
      case TipTip(_, _, _, _, ro, rv) => Tip(ro, rv)
      case TipBin(_, _, _, _, r) => r
      case BinTip(p, m, l, ro, rv) => bin(p, m, l.tail, ro, rv)
      case BinBin(p, m, l, r) => bin(p, m, l.tail, r)
    }*/

    def headTail: (T, Ordering[T])
    /*final def headTail: (T, Ordering[T]) = this match {
      case Zero => throw new NoSuchElementException("headTail of empty map")
      case Tip(_, v) => (v, Zero)
      case TipTip(_, _, _, lv, ro, rv) => (lv, Tip(ro, rv))
      case TipBin(_, _, _, lv, r) => (lv, r)
      case BinTip(p, m, l, ro, rv) =>
        val (head, tail) = l.headTail
        (head, bin(p, m, tail, ro, rv))
      case BinBin(p, m, l, r) =>
        val (head, tail) = l.headTail
        (head, bin(p, m, tail, r))
    }*/

    def init: Ordering[T]
    /*final def init: Ordering[T] = this match {
      case Zero => throw new NoSuchElementException("init of empty map")
      case Tip(_, _) => Zero
      case TipTip(_, _, lo, lv, _, _) => Tip(lo, lv)
      case TipBin(p, m, lo, lv, r) => bin(p, m, lo, lv, r.init)
      case BinTip(_, _, l, _, _) => l
      case BinBin(p, m, l, r) => bin(p, m, l, r.init)
    }*/

    def initLast: (Ordering[T], T)
    /*final def initLast: (Ordering[T], T) = this match {
      case Zero => throw new NoSuchElementException("initLast of empty map")
      case Tip(_, v) => (Zero, v)
      case TipTip(_, _, lo, lv, _, rv) => (Tip(lo, lv), rv)
      case TipBin(p, m, lo, lv, r) =>
        val (init, last) = r.initLast
        (bin(p, m, bin(p, m, lo, lv, init), r), last)
      case BinTip(_, _, l, _, rv) => (l, rv)
      case BinBin(p, m, l, r) =>
        val (init, last) = r.initLast
        (bin(p, m, l, init), last)
    }*/

    final def iterator: Iterator[T] = this match {
      case Zero => Iterator.empty
      case _ => new Iterator(this)
    }

    def include[S >: T](ordinal: Int, value: S): Ordering[S]
    /*final def include[S >: T](ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) =>
        if (ordinal == o) Tip(ordinal, value)
        else join(ordinal, value, this)
      case TipTip(p, m, lo, lv, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m)) BinTip(p, m, join(ordinal, value, lo, lv), ro, rv)
        else TipBin(p, m, lo, lv, join(ordinal, value, ro, rv))
      case TipBin(p, m, lo, lv, r) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m)) bin(p, m, join(ordinal, value, lo, lv), r)
        else TipBin(p, m, lo, lv, r.include(ordinal, value))
      case BinTip(p, m, l, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m)) BinTip(p, m, l.include(ordinal, value), ro, rv)
        else bin(p, m, l, join(ordinal, value, ro, rv))
      case BinBin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m))  {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m)) BinBin(p, m, l.include(ordinal, value), r)
        else BinBin(p, m, l, r.include(ordinal, value))
    }*/

    def append[S >: T](ordinal: Int, value: S): Ordering[S]
    /*final def append[S >: T](ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        Tip(ordinal, value)
      case Tip(o, _) if o >= ordinal =>
        throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
      case Tip(o, _) =>
        if (ordinal == o) Tip(ordinal, value)
        else join(ordinal, value, this)
      case TipTip(p, m, lo, lv, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else TipBin(p, m, lo, lv, join(ordinal, value, ro, rv))
      case TipBin(p, m, lo, lv, r) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else TipBin(p, m, lo, lv, r.append(ordinal, value))
      case BinTip(p, m, l, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m))  throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else bin(p, m, l, join(ordinal, value, ro, rv))
      case BinBin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          bin(p2, m, this, ordinal, value)
        } else if (zero(ordinal, m))  throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else BinBin(p, m, l, r.append(ordinal, value))
    }*/

    @inline private[collection] final def cleanInPlace[S >: T](): Ordering[S] = cleanInPlace(null)
    private[collection] def cleanInPlace[S >: T](parent: RightBinned[S]): Ordering[S]
    /*private[collection] final def cleanInPlace[S >: T](parent: BinBin[S]): Ordering[S] = this match {
      case Zero =>
        Zero
      case BinBin(p, m, l, TipTip(p2, 0, lo2, _, ro, rv)) if p2 == lo2 && ro == lo2 =>
        val b2 = BinTip(p, m, l, ro, rv)
        if (parent != null) {
          parent.right = b2
          parent
        } else b2
      case b @ BinBin(_, _, _, r) =>
        r.cleanInPlace(b)
        this
      case TipTip(p, 0, lo, lv, ro, _) if p == lo && ro == lo =>
        Tip(lo, lv)
      case t: Tip[S] =>
        this
      case o =>
        this
    }*/

    @inline private[collection] final def appendInPlace[S >: T](ordinal: Int, value: S): Ordering[S] = appendInPlace(null, ordinal, value)
    private[collection] def appendInPlace[S >: T](parent: RightBinned[S], ordinal: Int, value: S): Ordering[S]
    /*private[collection] final def appendInPlace[S >: T](parent: BinBin[S], ordinal: Int, value: S): Ordering[S] = this match {
      case Zero =>
        TipTip(ordinal, 0, ordinal, value, ordinal, value)
      case Tip(o, _) if o >= ordinal =>
        throw new IllegalArgumentException(s"Append called with ordinal out of range: $o is not greater than current max ordinal ${this.ordinal}")
      case Tip(o, v) if parent == null =>
        if (ordinal == o) Tip(ordinal, value)
        else {
          val m = branchMask(ordinal, o)
          val p = maskBits(ordinal, m)
          if (zero(ordinal, m)) TipTip(p, m, ordinal, value, o, v)
          else TipTip(p, m, o, v, ordinal, value)
        }
      case Tip(o, v) =>
        val m = branchMask(ordinal, o)
        val p = maskBits(ordinal, m)
        parent.right =
            if (zero(ordinal, m)) TipTip(p, m, ordinal, value, o, v)
            else TipTip(p, m, o, v, ordinal, value)
        parent
      case t @ TipTip(p, 0, lo, _, ro, _) if p == lo && ro == lo =>
        val m = branchMask(lo, ordinal)
        val p = maskBits(lo, m)
        val s = t.asInstanceOf[TipTip[S]]
        s.prefix = p
        s.mask = m
        s.rightOrd = ordinal
        s.rightValue = value
        val b2 = s
        if (parent != null) {
          parent.right = b2
          parent
        } else b2
      case TipTip(p, m, lo, lv, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          //val b2 = BinTip(p2, m, this, ordinal, value)
          val b2 = BinBin(p2, m, this, TipTip(ordinal, 0, ordinal, value, ordinal, value))
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          val b2 = TipBin(p, m, lo, lv, join(ordinal, value, ro, rv)) // Could be simplified since we know that ordinal is greater than ro
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        }
      case TipBin(p, m, lo, lv, r) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          val b2 = BinTip(p2, m, this, ordinal, value)
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else TipBin(p, m, lo, lv, r.appendInPlace(ordinal, value))
      case BinTip(p, m, l, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          val b2 = BinTip(p2, m, this, ordinal, value)
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          val b2 = BinBin(p, m, l, join(ordinal, value, ro, rv)) // Could be simplified since we know that ordinal is greater than ro
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        }
      case b @ BinBin(p, m, _, r) =>
        if (!hasMatch(ordinal, p, m)) {
          val m = branchMask(ordinal, p)
          val p2 = maskBits(ordinal, m)
          val b2 = BinTip(p2, m, this, ordinal, value)
          if (parent != null) {
            parent.right = b2
            parent
          } else b2
        } else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          r.appendInPlace(b, ordinal, value)
          this
        }
    }*/

    def exclude(ordinal: Int): Ordering[T]
    /*final def exclude(ordinal: Int): Ordering[T] = this match {
      case Tip(o, _) =>
        if (ordinal == o) Zero
        else this
      case Zero =>
        Zero
      case Tip(o, _) =>
        if (ordinal == o) Zero
        else this
      case TipBin(p, m, lo, lv, r) =>
        if (!hasMatch(ordinal, p, m)) this
        else if (ordinal == lo) r
        else if (zero(ordinal, m)) this // It is already absent
        else bin(p, m, lo, lv, r.exclude(ordinal))
      case BinTip(p, m, l, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) this
        else if (ordinal == ro) l
        else if (zero(ordinal, m)) bin(p, m, l.exclude(ordinal), ro, rv)
        else this // It is already absent
      case TipTip(p, m, lo, lv, ro, rv) =>
        if (!hasMatch(ordinal, p, m)) this
        else if (ordinal == lo) Tip(ro, rv)
        else if (ordinal == ro) Tip(lo, lv)
        else this // It is already absent
      case BinBin(p, m, l, r) =>
        if (!hasMatch(ordinal, p, m)) this
        else if (zero(ordinal, m)) bin(p, m, l.exclude(ordinal), r)
        else bin(p, m, l, r.exclude(ordinal))
    }*/

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
      }, rear.cleanInPlace())
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
    def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S]
    /*final def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = this match {
      case Zero => Zero
      case Tip(o, v) =>
        f(o, v) match {
          case None => Zero
          case Some(v2) =>
            // hack to preserve sharing
            if (v.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef]) this.asInstanceOf[Ordering[S]]
            else Tip(o, v2)
        }
      case TipBin(p, m, lo, lv, r) =>
        (f(lo, lv), r.modifyOrRemove(f)) match {
          case (None, Zero) => Zero
          case (Some(lv2), Zero) => Tip(lo, lv2)
          case (None, r2) => r2
          case (Some(lv2), r2) =>
            // hack to preserve sharing
            if ((lv.asInstanceOf[AnyRef] eq lv2.asInstanceOf[AnyRef]) && (r.asInstanceOf[AnyRef] eq r2.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else TipBin(p, m, lo, lv2, r2)
        }
      case BinTip(p, m, l, ro, rv) =>
        (l.modifyOrRemove(f), f(ro, rv)) match {
          case (Zero, None) => Zero
          case (Zero, Some(rv2)) => Tip(ro, rv2)
          case (l2, None) => l2
          case (l2, Some(rv2)) =>
            // hack to preserve sharing
            if ((l.asInstanceOf[AnyRef] eq l2.asInstanceOf[AnyRef]) && (rv2.asInstanceOf[AnyRef] eq rv2.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else BinTip(p, m, l2, ro, rv2)
        }
      case TipTip(p, m, lo, lv, ro, rv) =>
        (f(lo, lv), f(ro, rv)) match {
          case (None, None) => Zero
          case (None, Some(rv2)) => Tip(ro, rv2)
          case (Some(lv2), None) => Tip(lo, lv2)
          case (Some(lv2), Some(rv2)) =>
            // hack to preserve sharing
            if ((lv.asInstanceOf[AnyRef] eq lv2.asInstanceOf[AnyRef]) && (rv2.asInstanceOf[AnyRef] eq rv2.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else TipTip(p, m, lo, lv2, ro, rv2)
        }
      case BinBin(p, m, l, r) =>
        (l.modifyOrRemove(f), r.modifyOrRemove(f)) match {
          case (Zero, Zero) => Zero
          case (Zero, r2) => r2
          case (l2, Zero) => l2
          case (l2, r2) =>
            // hack to preserve sharing
            if ((l.asInstanceOf[AnyRef] eq l2.asInstanceOf[AnyRef]) && (r.asInstanceOf[AnyRef] eq r2.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else bin(p, m, l2, r2)
        }
    }*/
  }
  private[immutable] final object Ordering {
    import scala.collection.generic.BitOperations.Int._
    import scala.collection.generic.BitOperations.Int.{mask => maskBits}

    @inline private[immutable] def toBinaryString(i: Int): String = s"$i/${i.toBinaryString}"

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
      private[this] val buffer = new Array[Any](33)

      @inline private[this] def pop = {
        index -= 1
        buffer(index)
      }

      @inline private[this] def push(x: Any): Unit = {
        buffer(index) = x
        index += 1
      }

      if (it != Zero) push(it)

      def hasNext = index != 0
      @tailrec
      def next(): V =
        pop match {
          case Tip(_, v: V) =>
            v
          case TipTip(_, _, _, lv: V, _, rv) =>
            push(rv)
            lv
          case TipBin(_,_, _, lv: V, r) =>
            push(r)
            lv
          case BinTip(_,_, l, _, rv) =>
            push(rv)
            push(l)
            next()
          case BinBin(_,_, l, r) =>
            push(r)
            push(l)
            next()
          // This should never happen. We don't allow Ordering.Zero in subtrees of the Ordering
          // and don't return an Ordering.Iterator for Ordering.Zero.
          case Zero => throw new IllegalStateException("empty subtree not allowed")
          case v: V => v
        }
    }

    object Iterator {
      val Empty = new Iterator[Nothing](Ordering.empty[Nothing])
      def empty[V]: Iterator[V] = Empty.asInstanceOf[Iterator[V]]
    }

    sealed trait RightBinned[T] extends Ordering[T] {
      var right: Ordering[T]
    }
    final case object Zero extends Ordering[Nothing] {
      def ordinal: Int = 0
      def head: Nothing = throw new NoSuchElementException("head of empty map")
      def headOption: Option[Nothing] = None
      def last: Nothing = throw new NoSuchElementException("last of empty map")
      def lastOption: Option[Nothing] = None
      def tail: Nothing = throw new NoSuchElementException("tail of empty map")
      def headTail: Nothing = throw new NoSuchElementException("headTail of empty map")
      def init: Nothing = throw new NoSuchElementException("init of empty map")
      def initLast: Nothing = throw new NoSuchElementException("initLast of empty map")
      def include[S >: Nothing](ordinal: Int, value: S): Ordering[S] = Tip(ordinal, value)
      def append[S >: Nothing](ordinal: Int, value: S): Ordering[S] = Tip(ordinal, value)
      private[collection] def cleanInPlace[S >: Nothing](parent: RightBinned[S]): Ordering[S] = Zero
      private[collection] def appendInPlace[S >: Nothing](parent: RightBinned[S], ordinal: Int, value: S): Ordering[S] = TipTip(ordinal, 0, ordinal, value, ordinal, value)
      def exclude(ordinal: Int): Ordering[Nothing] = this
      def modifyOrRemove[S](f: (Int, Nothing) => Option[S]): Zero.type = this

      // Important! Without this equals method in place, an infinite
      // loop from Map.equals => size => pattern-match-on-Nil => equals
      // develops.  Case objects and custom equality don't mix without
      // careful handling.
      override def equals(that : Any): Boolean = that match {
        case _: this.type => true
        case _: Ordering[_] => false // The only empty Orderings are eq Nil
        case _ => super.equals(that)
      }
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = sb ++= s"${prefix}Ø\n"
    }

    final case class Tip[+T](ordinal: Int, value: T) extends Ordering[T] {
      //println(this)
      def head: T = value
      def headOption: Option[T] = Some(value)
      def last: T = value
      def lastOption: Option[T] = Some(value)
      def tail: Zero.type = Zero
      def headTail: (T, Ordering[T]) = (value, Zero)
      def init: Zero.type = Zero
      def initLast: (Ordering[T], T) = (Zero, value)
      def include[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (ordinal == this.ordinal) {
          if (value.asInstanceOf[AnyRef] eq this.value.asInstanceOf[AnyRef]) this
          else this.copy(value = value)
        } else {
          val m = branchMask(ordinal, this.ordinal)
          val p = maskBits(ordinal, m)
          if (zero(ordinal, m)) TipTip(p, m, ordinal, value, this.ordinal, this.value)
          else TipTip(p, m, this.ordinal, this.value, ordinal, value)
        }
      }
      def append[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (this.ordinal >= ordinal) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          val m = branchMask(ordinal, this.ordinal)
          val p = maskBits(ordinal, m)
          TipTip(p, m, this.ordinal, this.value, ordinal, value) // TODO: Verify correctness
        }
      }
      private[collection] def cleanInPlace[S >: T](parent: RightBinned[S]): Ordering[S] = this
      private[collection] def appendInPlace[S >: T](parent: RightBinned[S], ordinal: Int, value: S): Ordering[S] = {
        if (this.ordinal >= ordinal) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          val m = branchMask(ordinal, this.ordinal)
          val p = maskBits(ordinal, m)
          val o = TipTip(p, m, this.ordinal, this.value, ordinal, value)
          if (parent != null) {
            parent.right = o
            parent
          } else o
        }
      }
      def exclude(ordinal: Int): Ordering[T] = {
        if (ordinal == this.ordinal) Zero
        else this
      }
      def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = {
        f(ordinal, value) match {
          case None => Zero
          case Some(v) =>
            // hack to preserve sharing
            if (value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef]) this.asInstanceOf[Ordering[S]]
            else copy(value = v)
        }
      }
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = {
        sb ++= s"${prefix}Tip(${toBinaryString(ordinal)} -> $value)\n"
      }
    }
    final case class TipTip[+T](var prefix: Int, var mask: Int, leftOrdinal: Int, leftValue: T, var rightOrdinal: Int, var rightValue: T @uncheckedVariance) extends Ordering[T] {
      //require(leftOrd <= rightOrd, s"TipTip: left ordinal must be lower than right ordinal: $leftOrd <= $rightOrd")
      def ordinal: Int = rightOrdinal
      def head: T = leftValue
      def headOption: Option[T] = Some(leftValue)
      def last: T = rightValue
      def lastOption: Option[T] = Some(rightValue)
      def tail: Ordering[T] = Tip(rightOrdinal, rightValue)
      def headTail: (T, Ordering[T]) = (leftValue, Tip(rightOrdinal, rightValue))
      def init: Ordering[T] = Tip(leftOrdinal, leftValue)
      def initLast: (Ordering[T], T) = (Tip(leftOrdinal, leftValue), rightValue)
      def include[S >: T](ordinal: Int, value: S): Ordering[S] = {
        val m = branchMask(ordinal, prefix)
        val p = maskBits(ordinal, m)
        if (!hasMatch(ordinal, prefix, mask)) BinTip(p, m, this, ordinal, value)
        else if (zero(ordinal, m)) BinTip(p, m, join(ordinal, value, leftOrdinal, leftValue), rightOrdinal, rightValue)
        else TipBin(p, m, leftOrdinal, leftValue, join(ordinal, value, rightOrdinal, rightValue))
      }
      def append[S >: T](ordinal: Int, value: S): Ordering[S] = {
         if (this.ordinal >= ordinal) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
         else {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          if (!hasMatch(ordinal, prefix, mask)) BinTip(p, m, this, ordinal, value)
          else if (zero(ordinal, m)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
          else TipBin(p, m, leftOrdinal, leftValue, join(ordinal, value, rightOrdinal, rightValue))
        }
      }
      private[collection] def cleanInPlace[S >: T](parent: RightBinned[S]): Ordering[S] = {
        if (mask == 0 && prefix == leftOrdinal && rightOrdinal == leftOrdinal) Tip(leftOrdinal, leftValue)
        else this
      }
      private[collection] def appendInPlace[S >: T](parent: RightBinned[S], ordinal: Int, value: S): Ordering[S] = {
        if (mask == 0 && prefix == leftOrdinal && rightOrdinal == leftOrdinal) {
          val m = branchMask(leftOrdinal, ordinal)
          val p = maskBits(leftOrdinal, m)
          val s = this.asInstanceOf[TipTip[S]]
          s.prefix = p
          s.mask = m
          s.rightOrdinal = ordinal
          s.rightValue = value
          val o = s
          if (parent != null) {
            parent.right = o
            parent
          } else o
        } else {
          if (!hasMatch(ordinal, prefix, mask)) {
            val m = branchMask(ordinal, prefix)
            val p = maskBits(ordinal, m)
            val o = BinBin(p, m, this, TipTip(ordinal, 0, ordinal, value, ordinal, value))
            if (parent != null) {
              parent.right = o
              parent
            } else o
          } else if (zero(ordinal, mask)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
          else {
            val o = TipBin(prefix, mask, leftOrdinal, leftValue, join(ordinal, value, rightOrdinal, rightValue)) // Could be simplified since we know that ordinal is greater than rightValue
            if (parent != null) {
              parent.right = o
              parent
            } else o
          }
        }
      }
      def exclude(ordinal: Int): Ordering[T] = {
        if (!hasMatch(ordinal, prefix, mask)) this
        else if (ordinal == leftOrdinal) Tip(rightOrdinal, rightValue)
        else if (ordinal == rightOrdinal) Tip(leftOrdinal, leftValue)
        else this
      }

      def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = {
        (f(leftOrdinal, leftValue), f(rightOrdinal, rightValue)) match {
          case (None, None) => Zero
          case (None, Some(rv)) => Tip(rightOrdinal, rv)
          case (Some(lv), None) => Tip(leftOrdinal, lv)
          case (Some(lv), Some(rv)) =>
            // hack to preserve sharing
            if ((leftValue.asInstanceOf[AnyRef] eq lv.asInstanceOf[AnyRef]) && (rv.asInstanceOf[AnyRef] eq rv.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else copy(leftValue = lv, rightValue = rv)
        }
      }

      /*
      def bin[S](lo: Int, lv: S, ro: Int, rv: S): Ordering[S] = {
        if (leftOrd == lo && rightOrd == ro && (leftValue.asInstanceOf[AnyRef] eq lv.asInstanceOf[AnyRef]) && (rightValue.asInstanceOf[AnyRef] eq rv.asInstanceOf[AnyRef])) this.asInstanceOf[TipTip[S]]
        else TipTip[S](prefix, mask, lo, lv, ro, rv)
      }*/
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = {
        //sb ++= s"${prefix}BinBothTips(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        sb ++= s"${prefix}Bin(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        sb ++= s"${subPrefix}├── Tip(${toBinaryString(leftOrdinal)} -> $leftValue)\n"
        sb ++= s"${subPrefix}└── Tip(${toBinaryString(rightOrdinal)} -> $rightValue)\n"
      }
    }
    final case class TipBin[+T](prefix: Int, mask: Int, leftOrdinal: Int, leftValue: T, var right: Ordering[T] @scala.annotation.unchecked.uncheckedVariance) extends RightBinned[T @scala.annotation.unchecked.uncheckedVariance] {
      //require(leftOrd <= right.ordinal, s"TipBin: left ordinal must be lower than right ordinal: $leftOrd <= ${right.ordinal}")
      def ordinal: Int = right.ordinal
      def head: T = leftValue
      def headOption: Option[T] = Some(leftValue)
      def last: T = right.last
      def lastOption: Option[T] = right.lastOption
      def tail: Ordering[T] = right
      def headTail: (T, Ordering[T]) = (leftValue, right)
      def init: Ordering[T] = bin(prefix, mask, leftOrdinal, leftValue, right.init)
      def initLast: (Ordering[T], T) = {
        right match {
          case TipTip(_, _, lo, lv, _, rv) =>
            (TipTip(prefix, mask, leftOrdinal, leftValue, lo, lv), rv)
          case TipBin(p, m, lo, lv, r) =>
            val (init, last) = r.initLast
            (bin(prefix, mask, leftOrdinal, leftValue, bin(p, m, lo, lv, init)), last)
          case BinTip(_, _, l, _, rv) =>
            (bin(prefix, mask, leftOrdinal, leftValue, l), rv)
          case BinBin(p, m, l, r) =>
            val (init, last) = r.initLast
            (bin(prefix, mask, leftOrdinal, leftValue, bin(p, m, l, init)), last)
          case r =>
            r.initLast
        }
      }
      def include[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          BinTip(p, m, this, ordinal, value)
        } else if (zero(ordinal, mask)) bin(prefix, mask, join(ordinal, value, leftOrdinal, leftValue), right)
        else TipBin(prefix, mask, leftOrdinal, leftValue, right.include(ordinal, value))
      }
      def append[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          BinTip(p, m, this, ordinal, value)
        } else if (zero(ordinal, mask)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else TipBin(prefix, mask, leftOrdinal, leftValue, right.append(ordinal, value))
      }
      private[collection] def cleanInPlace[S >: T](parent: RightBinned[S]): Ordering[S] = this
      private[collection] def appendInPlace[S >: T](parent: RightBinned[S], ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          val o = BinTip(p, m, this, ordinal, value)
          if (parent != null) {
            parent.right = o
            parent
          } else o
        } else if (zero(ordinal, mask)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          right.appendInPlace(this.asInstanceOf[TipBin[S]], ordinal, value)
        }
      }
      def exclude(ordinal: Int): Ordering[T] = {
        if (!hasMatch(ordinal, prefix, mask)) this
        else if (ordinal == leftOrdinal) right
        else if (zero(ordinal, mask)) this // It is already absent // TODO: Is this correct?
        else {
          //bin(prefix, mask, leftOrdinal, leftValue, right.exclude(ordinal))
          right match {
            case TipTip(_, _, lo, _, ro, rv) if lo == ordinal => join(leftOrdinal, leftValue, ro, rv)
            case TipTip(_, _, lo, lv, ro, _) if ro == ordinal => join(leftOrdinal, leftValue, lo, lv)
            case r => bin(prefix, mask, leftOrdinal, leftValue, right.exclude(ordinal))
          }
        }
      }
      def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = {
        (f(leftOrdinal, leftValue), right.modifyOrRemove(f)) match {
          case (None, Zero) => Zero
          case (Some(lv), Zero) => Tip(leftOrdinal, lv)
          case (None, r) => r
          case (Some(lv), r) =>
            // hack to preserve sharing
            if ((leftValue.asInstanceOf[AnyRef] eq lv.asInstanceOf[AnyRef]) && (right.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else TipBin(prefix, mask, leftOrdinal, lv, r)
        }
      }

      /*
      def bin[S](lo: Int, lv: S, r: Ordering[S]): Ordering[S] = {
        if ((leftOrd == lo && (leftValue.asInstanceOf[AnyRef] eq lv.asInstanceOf[AnyRef])) && (right eq r)) this.asInstanceOf[TipBin[S]]
        else TipBin[S](prefix, mask, lo, lv, r)
      }*/
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = {
        //sb ++= s"${prefix}BinLeftTip(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        sb ++= s"${prefix}Bin(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        //left.format(sb, subPrefix + "├── ", subPrefix + "│   ")
        sb ++= s"${subPrefix}├── Tip(${toBinaryString(leftOrdinal)} -> $leftValue)\n"
        right.format(sb, subPrefix + "└── ", subPrefix + "    ")
      }
    }
    final case class BinTip[+T](prefix: Int, mask: Int, left: Ordering[T], rightOrdinal: Int, rightValue: T) extends Ordering[T] {
      //require(left.ordinal <= rightOrd, s"BinTip: left ordinal must be lower than right ordinal: ${left.ordinal} <= ${rightOrd}")
      def ordinal: Int = rightOrdinal
      def head: T = left.head
      def headOption: Option[T] = left.headOption
      def last: T = rightValue
      def lastOption: Option[T] = Some(rightValue)
      def tail: Ordering[T] = bin(prefix, mask, left.tail, rightOrdinal, rightValue)
      def headTail: (T, Ordering[T]) = {
        left match {
          case TipTip(_, _, _, lv, ro, rv) =>
            (lv, TipTip(prefix, mask, ro, rv, rightOrdinal, rightValue))
          case TipBin(_, _, _, lv, r) =>
            (lv, bin(prefix, mask, r, rightOrdinal, rightValue))
          case BinTip(p, m, l, ro, rv) =>
            val (head, tail) = l.headTail
            (head, bin(prefix, mask, bin(p, m, tail, ro, rv), rightOrdinal, rightValue))
          case BinBin(p, m, l, r) =>
            val (head, tail) = l.headTail
            (head, bin(prefix, mask, bin(p, m, tail, r), rightOrdinal, rightValue))
          case l =>
            l.headTail
        }
      }
      def init: Ordering[T] = left
      def initLast: (Ordering[T], T) = (left, rightValue)
      def include[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          BinTip(p, m, this, ordinal, value)
        } else if (zero(ordinal, mask)) copy(left = left.include(ordinal, value))
        else bin(prefix, mask, left, join(ordinal, value, rightOrdinal, rightValue))
      }
      def append[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          BinTip(p, m, this, ordinal, value)
        } else if (zero(ordinal, mask))  throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else bin(prefix, mask, left, join(ordinal, value, rightOrdinal, rightValue))
      }
      private[collection] def cleanInPlace[S >: T](parent: RightBinned[S]): Ordering[S] = this
      private[collection] def appendInPlace[S >: T](parent: RightBinned[S], ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          val o = BinTip(p, m, this, ordinal, value)
          if (parent != null) {
            parent.right = o
            parent
          } else o
        } else if (zero(ordinal, mask)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          val o = BinBin(prefix, mask, left, join(ordinal, value, rightOrdinal, rightValue)) // Could be simplified since we know that ordinal is greater than ro
          if (parent != null) {
            parent.right = o
            parent
          } else o
        }
      }
      def exclude(ordinal: Int): Ordering[T] = {
        if (!hasMatch(ordinal, prefix, mask)) this
        else if (ordinal == rightOrdinal) left
        else if (zero(ordinal, mask)) {
          left match {
            case TipTip(_, _, lo, _, ro, rv) if lo == ordinal => join(ro, rv, rightOrdinal, rightValue)
            case TipTip(_, _, lo, lv, ro, _) if ro == ordinal => join(lo, lv, rightOrdinal, rightValue)
            case l => bin(prefix, mask, left.exclude(ordinal), rightOrdinal, rightValue)
          }
        } else this
      }
      def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = {
        (left.modifyOrRemove(f), f(rightOrdinal, rightValue)) match {
          case (Zero, None) => Zero
          case (Zero, Some(rv)) => Tip(rightOrdinal, rv)
          case (l, None) => l
          case (l, Some(rv)) =>
            // hack to preserve sharing
            if ((left.asInstanceOf[AnyRef] eq l.asInstanceOf[AnyRef]) && (rv.asInstanceOf[AnyRef] eq rv.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else BinTip(prefix, mask, l, rightOrdinal, rv)
        }
      }

      /*
      def bin[S](l: Ordering[S], ro: Int, rv: S): Ordering[S] = {
        if ((rightOrd == ro && (rightValue.asInstanceOf[AnyRef] eq rv.asInstanceOf[AnyRef])) && (left eq l)) this.asInstanceOf[BinTip[S]]
        else BinTip[S](prefix, mask, l, ro, rv)
      }*/
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = {
        //sb ++= s"${prefix}BinRightTip(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        sb ++= s"${prefix}Bin(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        left.format(sb, subPrefix + "├── ", subPrefix + "│   ")
        sb ++= s"${subPrefix}└── Tip(${toBinaryString(rightOrdinal)} -> $rightValue)\n"
      }
    }
    final case class BinBin[+T](prefix: Int, mask: Int, left: Ordering[T], var right: Ordering[T] @scala.annotation.unchecked.uncheckedVariance) extends RightBinned[T @scala.annotation.unchecked.uncheckedVariance] {
      //require(left.ordinal <= right.ordinal, s"BinBin: left ordinal must be lower than right ordinal: ${left.ordinal} <= ${right.ordinal}")
      def ordinal: Int = right.ordinal
      def head: T = left.head
      def headOption: Option[T] = left.headOption
      def last: T = right.last
      def lastOption: Option[T] = right.lastOption
      def tail: Ordering[T] = bin(prefix, mask, left.tail, right)
      def headTail: (T, Ordering[T]) = {
        left match {
          case TipTip(_, _, _, lv, ro, rv) =>
            (lv, bin(prefix, mask, ro, rv, right))
          case TipBin(_, _, _, lv, r) =>
            (lv, bin(prefix, mask, r, right))
          case BinTip(p, m, l, ro, rv) =>
            val (head, tail) = l.headTail
            (head, bin(prefix, mask, bin(p, m, tail, ro, rv), right))
          case BinBin(p, m, l, r) =>
            val (head, tail) = l.headTail
            (head, bin(prefix, mask, bin(p, m, tail, r), right))
          case l =>
            l.headTail
        }
      }
      def init: Ordering[T] = bin(prefix, mask, left, right.init)
      def initLast: (Ordering[T], T) = {
        right match {
          case TipTip(_, _, lo, lv, _, rv) =>
            (bin(prefix, mask, left, lo, lv), rv)
          case TipBin(p, m, lo, lv, r) =>
            val (init, last) = r.initLast
            (bin(prefix, mask, left, bin(p, m, lo, lv, init)), last)
          case BinTip(_, _, l, _, rv) =>
            (bin(prefix, mask, left, l), rv)
          case BinBin(p, m, l, r) =>
            val (init, last) = r.initLast
            (bin(prefix, mask, left, bin(p, m, l, init)), last)
          case r =>
            r.initLast
        }
      }
      def include[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask))  {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          bin(p, m, this, ordinal, value)
        } else if (zero(ordinal, mask)) BinBin(prefix, mask, left.include(ordinal, value), right)
        else BinBin(prefix, mask, left, right.include(ordinal, value))
      }
      def append[S >: T](ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          bin(p, m, this, ordinal, value)
        } else if (zero(ordinal, mask))  throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else BinBin(prefix, mask, left, right.append(ordinal, value))
      }
      private[collection] def cleanInPlace[S >: T](parent: RightBinned[S]): Ordering[S] = right match {
        case TipTip(p2, 0, lo2, _, ro, rv) if p2 == lo2 && ro == lo2 =>
          val o = BinTip(prefix, mask, left, ro, rv)
          if (parent != null) {
            parent.right = o
            parent
          } else o
        case r =>
          r.cleanInPlace(this)
          this
      }
      private[collection] def appendInPlace[S >: T](parent: RightBinned[S], ordinal: Int, value: S): Ordering[S] = {
        if (!hasMatch(ordinal, prefix, mask)) {
          val m = branchMask(ordinal, prefix)
          val p = maskBits(ordinal, m)
          val o = BinTip(p, m, this, ordinal, value)
          if (parent != null) {
            parent.right = o
            parent
          } else o
        } else if (zero(ordinal, mask)) throw new IllegalArgumentException(s"Append called with ordinal out of range: $ordinal is not greater than current max ordinal ${this.ordinal}")
        else {
          right.appendInPlace(this.asInstanceOf[BinBin[S]], ordinal, value)
          this
        }
      }
      def exclude(ordinal: Int): Ordering[T] = {
        if (!hasMatch(ordinal, prefix, mask)) this
        else if (zero(ordinal, mask)) {
          left match {
            case TipTip(_, _, lo, _, ro, rv) if lo == ordinal => bin(prefix, mask, ro, rv, right)
            case TipTip(_, _, lo, lv, ro, _) if ro == ordinal => bin(prefix, mask, lo, lv, right)
            case l => bin(prefix, mask, left.exclude(ordinal), right)
          }
        } else {
          right match {
            case TipTip(_, _, lo, _, ro, rv) if lo == ordinal => bin(prefix, mask, left, ro, rv)
            case TipTip(_, _, lo, lv, ro, _) if ro == ordinal => bin(prefix, mask, left, lo, lv)
            case l => bin(prefix, mask, left, right.exclude(ordinal))
          }
        }
      }
      def modifyOrRemove[S](f: (Int, T) => Option[S]): Ordering[S] = {
        (left.modifyOrRemove(f), right.modifyOrRemove(f)) match {
          case (Zero, Zero) => Zero
          case (Zero, r) => r
          case (l, Zero) => l
          case (l, r) =>
            // hack to preserve sharing
            if ((left.asInstanceOf[AnyRef] eq l.asInstanceOf[AnyRef]) && (right.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef])) this.asInstanceOf[Ordering[S]]
            else bin(prefix, mask, l, r)
        }
      }

      /*
      def bin[S](l: Ordering[S], r: Ordering[S]): Ordering[S] = {
        if ((right.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef]) && (left.asInstanceOf[AnyRef] eq l.asInstanceOf[AnyRef])) this.asInstanceOf[BinBin[S]]
        else BinBin[S](prefix, mask, l, r)
      }*/
      protected def format(sb: StringBuilder, prefix: String, subPrefix: String): Unit = {
        sb ++= s"${prefix}Bin(${toBinaryString(this.prefix)}:${toBinaryString(mask)})\n"
        left.format(sb, subPrefix + "├── ", subPrefix + "│   ")
        right.format(sb, subPrefix + "└── ", subPrefix + "    ")
      }
    }

    @inline private def branchMask(i: Int, j: Int) = highestOneBit(i ^ j)

    @inline private def join[T](o1: Int, v1: T, o2: Int, v2: T): Ordering[T] = {
      val m = branchMask(o1, o2)
      val p = maskBits(o1, m)
      if (zero(o1, m)) TipTip(p, m, o1, v1, o2, v2)
      else TipTip(p, m, o2, v2, o1, v1)
    }

    /*
    @inline private def join[T](o1: Int, v1: T, t: Ordering[T]): Ordering[T] = t match {
      case Zero =>
        TipTip(o1, 0, o1, v1, o1, v1)
      case Tip(o2, v2) =>
        val m = branchMask(o1, o2)
        val p = maskBits(o1, m)
        if (zero(o1, m)) TipTip(p, m, o1, v1, o2, v2)
        else TipTip(p, m, o2, v2, o1, v1)
      case b @ TipTip(p, _, lo, lv, ro, rv) =>
        val m2 = branchMask(o1, p)
        val p2 = maskBits(o1, m2)
        if (zero(o1, m2)) BinTip(p2, m2, TipTip(maskBits(o1, m2), m2, o1, v1, lo, lv), ro, rv)
        else BinTip(p2, m2, b, o1, v1)
      case b @ TipBin(p, _, lo, lv, r) =>
        val m2 = branchMask(o1, p)
        val p2 = maskBits(o1, m2)
        if (zero(o1, m2)) BinBin(p2, m2, TipTip(maskBits(o1, m2), m2, o1, v1, lo, lv), r)
        else BinTip(p2, m2, b, o1, v1)
      case BinTip(p, m, l, ro, rv) =>
        val m2 = branchMask(o1, p)
        val p2 = maskBits(o1, m2)
        if (zero(o1, m2)) BinTip(p2, m2, join(o1, v1, l), ro, rv)
        else BinBin(p2, m2, l, TipTip(maskBits(o1, m2), m2, o1, v1, ro, rv))
      case b @ BinBin(p, _, l, r) =>
        val m2 = branchMask(o1, p)
        val p2 = maskBits(o1, m2)
        if (zero(o1, m2)) BinBin(p2, m2, join(o1, v1, l), r)
        else BinTip(p2, m2, b, o1, v1)
    }
    */

    @inline private def bin[T](prefix: Int, mask: Int, left: Ordering[T], right: Ordering[T]): Ordering[T] = (left, right) match {
      case (Zero, Zero) => Zero
      case (l, Zero) => l
      case (Zero, r) => r
      case (Tip(lo, lv), Tip(ro, rv)) => TipTip(prefix, mask, lo, lv, ro, rv)
      case (l, Tip(ro, rv)) => BinTip(prefix, mask, l, ro, rv)
      case (Tip(lo, lv), r) => TipBin(prefix, mask, lo, lv, r)
      case (l, r) => BinBin(prefix, mask, l, r)
    }
    @inline private def bin[T](prefix: Int, mask: Int, leftOrdinal: Int, leftValue: T, right: Ordering[T]): Ordering[T] = right match {
      case Zero =>
        Tip(leftOrdinal, leftValue)
      case Tip(ro, rv) =>
        join(leftOrdinal, leftValue, ro, rv)
      case r => TipBin(prefix, mask, leftOrdinal, leftValue, r)
    }
    @inline private def bin[T](prefix: Int, mask: Int, left: Ordering[T], rightOrdinal: Int, rightValue: T): Ordering[T] = left match {
      case Zero =>
        Tip(rightOrdinal, rightValue)
      case Tip(lo, lv) =>
        //TipTip(prefix, mask, lo, lv, rightOrd, rightValue)
        join(lo, lv, rightOrdinal, rightValue)
      case l => BinTip(prefix, mask, l, rightOrdinal, rightValue)
    }
  }
}