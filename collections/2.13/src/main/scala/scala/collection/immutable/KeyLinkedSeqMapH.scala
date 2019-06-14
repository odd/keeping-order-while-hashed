package scala
package collection
package immutable

import scala.annotation.unchecked.uncheckedVariance

class KeyLinkedSeqMapH[K, +V] private(
      private val f: Any = KeyLinkedSeqMapH.Ø,
      private val l: Any = KeyLinkedSeqMapH.Ø,
      private val mapping: HashMap[K, KeyLinkedSeqMapH.Entry[K, V]] = HashMap.empty[K, KeyLinkedSeqMapH.Entry[K, V]])
    extends AbstractMap[K, V]
        with SeqMap[K, V]
        with MapOps[K, V, KeyLinkedSeqMapH, KeyLinkedSeqMapH[K, V]]
        with StrictOptimizedIterableOps[(K, V), Iterable, KeyLinkedSeqMapH[K, V]]
        with StrictOptimizedMapOps[K, V, KeyLinkedSeqMapH, KeyLinkedSeqMapH[K, V]]
        with MapFactoryDefaults[K, V, KeyLinkedSeqMapH, Iterable] {

  import KeyLinkedSeqMapH._

  override protected[this] def className: String = "KeyLinkedSeqMapH"

  override def mapFactory: MapFactory[KeyLinkedSeqMapH] = KeyLinkedSeqMapH

  override def isEmpty = mapping.isEmpty
  override def size: Int = mapping.size

  @inline private def isSingular = mapping.size == 1
  @inline private def firstKey: K = as[K](f)
  @inline private def lastKey: K = as[K](l)

  override def updated[V1 >: V](key: K, value: V1): KeyLinkedSeqMapH[K, V1] = {
    if (isEmpty) new KeyLinkedSeqMapH(key, key, HashMap(key -> Entry[K, V1](value)))
    else {
      val hash = key.##
      mapping.get(key) match {
        case None =>
          val e: Entry[K, V] = mapping(lastKey).copy(next = key, nextHash = hash)
          val m = mapping.updated(lastKey, e).updated(key, Entry[K, V1](lastKey, value))
          new KeyLinkedSeqMapH[K, V1](f, key, m)
        case Some(e: Entry[K, V]) =>
          new KeyLinkedSeqMapH[K, V1](f, l, mapping.updated(key, e.copy(value = value)))
      }
    }
  }
  override def removed(key: K): KeyLinkedSeqMapH[K, V] = {
    mapping.get(key) match {
      case None => this
      case Some(Entry(Ø, _, Ø, _)) =>
        KeyLinkedSeqMapH.empty
      case Some(Entry(Ø, _, n, _)) =>
        val nextKey = as[K](n)
        val nextEntry = mapping(nextKey)
        new KeyLinkedSeqMapH(n, l, mapping.removed(key).updated(nextKey, nextEntry.copy(prev = Ø)))
      case Some(Entry(p, _, Ø, _)) =>
        val prevKey = as[K](p)
        val prevEntry = mapping(prevKey)
        new KeyLinkedSeqMapH(f, p, mapping.removed(key).updated(prevKey, prevEntry.copy(next = Ø, nextHash = 0)))
      case Some(Entry(p, _, n, nh)) =>
        val prevKey = as[K](p)
        val prevEntry = mapping(prevKey)
        val nextKey = as[K](n)
        val nextEntry = mapping(nextKey)
        new KeyLinkedSeqMapH(f, l, mapping.removed(key).updated(prevKey, prevEntry.copy(next = nextKey, nextHash = nh)).updated(nextKey, nextEntry.copy(prev = prevKey)))
    }
  }

  override def head: (K, V) = firstKey -> mapping(firstKey).value
  override def last: (K, V) = lastKey -> mapping(lastKey).value

  override def drop(n: Int): KeyLinkedSeqMapH[K, V] = slice(n, size)
  override def dropRight(n: Int): KeyLinkedSeqMapH[K, V] = slice(0, size - n)

  override def take(n: Int): KeyLinkedSeqMapH[K, V] = slice(0, n)
  override def takeRight(n: Int): KeyLinkedSeqMapH[K, V] = slice(size - n, size)

  override def tail: KeyLinkedSeqMapH[K, V] = {
    if (isEmpty) throw new NoSuchElementException("tail on empty")
    else if (isSingular) empty
    else {
      val firstEntry = mapping(firstKey)
      new KeyLinkedSeqMapH(firstEntry.nextKey, l, (mapping - firstKey).updated(firstEntry.nextKey, mapping(firstEntry.nextKey).copy(prev = Ø)))
    }
  }
  override def init: KeyLinkedSeqMapH[K, V] = {
    if (isEmpty) throw new NoSuchElementException("init on empty")
    else if (isSingular) empty
    else {
      val lastEntry = mapping(lastKey)
      new KeyLinkedSeqMapH(f, lastEntry.prevKey, (mapping - lastKey).updated(lastEntry.prevKey, mapping(lastEntry.prevKey).copy(next = Ø, nextHash = 0)))
    }
  }
  //slice(0, size - 1)

  override def slice(from: Int, until: Int): KeyLinkedSeqMapH[K, V] = {
    val sz = size
    val start = math.max(from, 0)
    val stop = math.min(until, sz)
    //println(s"slice: size = $size, start = $start, stop = $stop")
    if ((sz == 0) || (stop <= start)) empty
    else if (start == 0 && until == sz) this
    else if (stop - start > sz / 2) {
      var map = mapping
      var key: K = as[K](f)
      var firstKey: K = key
      var lastKey: K = as[K](l)
      var e: Entry[K, V] = mapping(key)
      var i = 0
      while (i < sz) {
        if (i < start || i > stop -1) {
          map = map - key
        } else if (i == start || i == stop - 1) {
          if (i == start) {
            firstKey = key
            e = e.copy(prev = Ø)
            //println(s"slice#first: key = $key, e = $e, map = $map")
          }
          if (i == stop - 1) {
            lastKey = key
            e = e.copy(next = Ø)
            //println(s"slice#last: key = $key, e = $e, map = $map")
          }
          map = map.updated(key, e)
        }
        if (e.next != Ø) {
          key = e.nextKey
          e = mapping(key)
        }
        i += 1
      }
      //println(s"slice: firstKey = $firstKey, lastKey = $lastKey, map = $map")
      new KeyLinkedSeqMapH(firstKey, lastKey, map)
    } else {
      val bdr = HashMap.newBuilder[K, Entry[K, V]]
      var key: K = as[K](f)
      var firstKey: K = key
      var e: Entry[K, V] = mapping(key)
      var i = 0
      while (i < stop) {
        if (i >= start) {
          if (i == start) {
            firstKey = key
            e = e.copy(prev = Ø)
            //println(s"slice#first: key = $key, e = $e")
          }
          if (i == stop - 1) {
            e = e.copy(next = Ø)
            //println(s"slice#last: key = $key, e = $e")
          }
          bdr.addOne(key, e)
        }
        if (e.next != Ø) {
          key = e.nextKey
          e = mapping(key)
        }
        i += 1
      }
      val map = bdr.result()
      //println(s"slice: firstKey = $firstKey, lastKey = $lastKey, map = $map")
      new KeyLinkedSeqMapH(firstKey, key, map)
    }
  }
  override def get(key: K): Option[V] = mapping.get(key).map(_.value)
  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)]() {
    private[this] var k = f
    override def hasNext = k != Ø
    override def next() = {
      if (!hasNext) throw new IllegalStateException("next called on empty iterator")
      else {
        val key = as[K](k)
        val entry = mapping(key)
        k = entry.next
        key -> entry.value
      }
    }
  }
}
object KeyLinkedSeqMapH extends MapFactory[KeyLinkedSeqMapH] {
  private val Ø = new AnyRef
  @inline private def as[T](k: Any): T = k.asInstanceOf[T]

  val Empty = new KeyLinkedSeqMapH[Nothing, Nothing]()
  override def empty[K, V] = Empty.asInstanceOf[KeyLinkedSeqMapH[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]) = {
    it match {
      case map: KeyLinkedSeqMapH[K, V] => map
      case _ => (newBuilder[K, V] ++= it).result()
    }
  }

  override def newBuilder[K, V] = new Builder[K, V]()

  final case class Entry[K, +V](prev: Any, var value: V @uncheckedVariance, var next: Any, var nextHash: Int) {
    def prevKey = as[K](prev)
    def nextKey = as[K](next)
    override def toString = s"Entry(${if (prev == Ø) "∅" else prev},$value,${if (next == Ø) "∅" else next})"
  }
  object Entry {
    def apply[K, V](value: V) = new Entry[K, V](Ø, value, Ø, 0)
    def apply[K, V](prevKey: K, value: V) = new Entry[K, V](prevKey, value, Ø, 0)
  }
  private[immutable] final class Builder[K, V] extends mutable.Builder[(K, V), KeyLinkedSeqMapH[K, V]] {
    private[this] val builder = new HashMapBuilder[K, Entry[K, V]]
    private[this] var first: Any = Ø
    private[this] var last: Any = Ø
    private[this] var previousEntry: Entry[K, V] = _
    private[this] var aliased: KeyLinkedSeqMapH[K, V] = _

    def isEmpty = first == Ø

    override def clear(): Unit = {
      builder.clear()
      first = Ø
      last = Ø
      previousEntry = null
      aliased = null
    }

    override def result(): KeyLinkedSeqMapH[K, V] = {
      if (aliased eq null) {
        if (isEmpty) {
          aliased = empty
        } else {
          aliased = new KeyLinkedSeqMapH(first, last, builder.result())
        }
      }
      aliased
    }
    def addOne(key: K, value: V): this.type = {
      if (aliased ne null) {
        aliased = aliased.updated(key, value)
      } else {
        builder.getOrElse(key, null) match {
          case e @ Entry(_, v, _, h) if value != v =>
            e.value = v
          case null if isEmpty =>
            first = key
            last = key
            previousEntry = Entry[K, V](value)
            builder.addOne(key, previousEntry)
          case null =>
            val lastKey = as[K](last)
            previousEntry.next = key
            previousEntry.nextHash = key.##
            val entry = Entry[K, V](lastKey, value)
            builder.addOne(key, entry, previousEntry.nextHash)
            last = key
            previousEntry = entry
        }
      }
      this
    }

    override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
  }
}