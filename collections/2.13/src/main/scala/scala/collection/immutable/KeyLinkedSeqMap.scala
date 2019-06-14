package scala
package collection
package immutable

import scala.annotation.unchecked.uncheckedVariance

class KeyLinkedSeqMap[K, +V] private(
      private val f: Any = KeyLinkedSeqMap.Ø,
      private val l: Any = KeyLinkedSeqMap.Ø,
      private val mapping: HashMap[K, KeyLinkedSeqMap.Entry[K, V]] = HashMap.empty[K, KeyLinkedSeqMap.Entry[K, V]])
    extends AbstractMap[K, V]
        with SeqMap[K, V]
        with MapOps[K, V, KeyLinkedSeqMap, KeyLinkedSeqMap[K, V]]
        with StrictOptimizedIterableOps[(K, V), Iterable, KeyLinkedSeqMap[K, V]]
        with StrictOptimizedMapOps[K, V, KeyLinkedSeqMap, KeyLinkedSeqMap[K, V]]
        with MapFactoryDefaults[K, V, KeyLinkedSeqMap, Iterable] {

  import KeyLinkedSeqMap._

  override protected[this] def className: String = "KeyLinkedSeqMap"

  override def mapFactory: MapFactory[KeyLinkedSeqMap] = KeyLinkedSeqMap

  override def isEmpty = mapping.isEmpty
  override def size: Int = mapping.size

  @inline private def isSingular = mapping.size == 1
  @inline private def firstKey: K = as[K](f)
  @inline private def lastKey: K = as[K](l)

  override def updated[V1 >: V](key: K, value: V1): KeyLinkedSeqMap[K, V1] = {
    if (isEmpty) new KeyLinkedSeqMap(key, key, HashMap(key -> Entry[K, V1](value)))
    else {
      val hash = key.##
      mapping.get(key) match {
        case None =>
          val e: Entry[K, V] = mapping(lastKey).copy(next = key)
          val m = mapping.updated(lastKey, e).updated(key, Entry[K, V1](lastKey, value))
          new KeyLinkedSeqMap[K, V1](f, key, m)
        case Some(e: Entry[K, V]) =>
          new KeyLinkedSeqMap[K, V1](f, l, mapping.updated(key, e.copy(value = value)))
      }
    }
  }
  override def removed(key: K): KeyLinkedSeqMap[K, V] = {
    mapping.get(key) match {
      case None => this
      case Some(Entry(Ø, _, Ø)) =>
        KeyLinkedSeqMap.empty
      case Some(Entry(Ø, _, n)) =>
        val nextKey = as[K](n)
        val nextEntry = mapping(nextKey)
        new KeyLinkedSeqMap(n, l, mapping.removed(key).updated(nextKey, nextEntry.copy(prev = Ø)))
      case Some(Entry(p, _, Ø)) =>
        val prevKey = as[K](p)
        val prevEntry = mapping(prevKey)
        new KeyLinkedSeqMap(f, p, mapping.removed(key).updated(prevKey, prevEntry.copy(next = Ø)))
      case Some(Entry(p, _, n)) =>
        val prevKey = as[K](p)
        val prevEntry = mapping(prevKey)
        val nextKey = as[K](n)
        val nextEntry = mapping(nextKey)
        new KeyLinkedSeqMap(f, l, mapping.removed(key).updated(prevKey, prevEntry.copy(next = nextKey)).updated(nextKey, nextEntry.copy(prev = prevKey)))
    }
  }

  override def head: (K, V) = firstKey -> mapping(firstKey).value
  override def last: (K, V) = lastKey -> mapping(lastKey).value

  override def drop(n: Int): KeyLinkedSeqMap[K, V] = slice(n, size)
  override def dropRight(n: Int): KeyLinkedSeqMap[K, V] = slice(0, size - n)

  override def take(n: Int): KeyLinkedSeqMap[K, V] = slice(0, n)
  override def takeRight(n: Int): KeyLinkedSeqMap[K, V] = slice(size - n, size)

  override def tail: KeyLinkedSeqMap[K, V] = {
    if (isEmpty) throw new NoSuchElementException("tail on empty")
    else if (isSingular) empty
    //else slice(1, size)
    else {
      val firstEntry = mapping(firstKey)
      new KeyLinkedSeqMap(firstEntry.nextKey, l, (mapping - firstKey).updated(firstEntry.nextKey, mapping(firstEntry.nextKey).copy(prev = Ø)))
    }
  }

  override def init: KeyLinkedSeqMap[K, V] = {
    if (isEmpty) throw new NoSuchElementException("init on empty")
    else if (isSingular) empty
    //else slice(0, size - 1)
    else {
      val lastEntry = mapping(lastKey)
      new KeyLinkedSeqMap(f, lastEntry.prevKey, (mapping - lastKey).updated(lastEntry.prevKey, mapping(lastEntry.prevKey).copy(next = Ø)))
    }
  }

  /*
  override def slice(from: Int, until: Int): KeyLinkedSeqMap[K, V] = {
    val sz = size
    val start = math.max(from, 0)
    val stop = math.min(until, sz)
    println(s"slice: start = $start, stop = $stop, size = $sz")
    if (sz == 0 || start >= stop) empty
    else if (start == 0 && stop == sz) this
    else {
      var mng = mapping
      var fk: K = as[K](f)
      var lk: K = as[K](l)
      var k = fk
      var e = mng(fk)
      var i = 0
      while (i < start) {
        e = mng(k)
        mng = mng - k
        if (e.next != Ø) k = e.nextKey
        i += 1
      }
      fk = k
      mng = mng.updated(fk, e.copy(prev = Ø))
      //if (e.next != Ø) k = e.nextKey
      //i += 1
      while (i < stop) {
        e = mng(k)
        if (e.next != Ø) k = e.nextKey
        i += 1
      }
      lk = k
      mng = mng.updated(fk, e.copy(next = Ø))
      //if (e.next != Ø) k = e.nextKey
      //i += 1
      while (i < sz) {
        e = mng(k)
        mng = mng - k
        if (e.next != Ø) k = e.nextKey
        i += 1
      }
      new KeyLinkedSeqMap[K, V](fk, lk, mng)
    }
  }
  */
  /*
  override def slice(from: Int, until: Int): KeyLinkedSeqMap[K, V] = {
    val sz = size
    val start = math.max(from, 0)
    val stop = math.min(until, sz)
    //rintln(s"slice: start = $start, stop = $stop, size = $sz")
    if (sz == 0 || start >= stop) empty
    else if (start == 0 && stop == sz) this
    else if (stop - start > sz / 2) {
      //rintln(s"slice-off: start = $start, stop = $stop, size = $sz")
      var mng = mapping
      var fk = firstKey
      var lk = lastKey
      var k: K = fk
      var e: Entry[K, V] = null
      var i = 0
      while (i < stop) {
        e = mapping(as[K](k))
        if (i == start) {
          fk = k
          e = e.copy(prev = Ø)
          //rintln(s"slice-off-first: i = $i, key = $fk, entry = $e")
        }
        if (i >= start) {
          if (i == stop - 1) {
            lk = k
            e = e.copy(next = Ø)
            //rintln(s"slice-off-last: i = $i, key = $lk, entry = $e")
          }
        } else {
          mng = mng - k
          //rintln(s"slice-off-skip: i = $i, key = $k, mapping = $mng")
        }
        if (e.next != Ø) k = e.nextKey
        i += 1
      }
      //rintln(s"slice-off: firstKey = $fk, lastKey = $lk, mapping = $mng")
      new KeyLinkedSeqMap[K, V](fk, lk, mng)
    } else {
      val bdr = new HashMapBuilder[K, Entry[K, V]]()
      var fk = firstKey
      var lk = lastKey
      var k: K = fk
      var e: Entry[K, V] = null
      var i = 0
      while (i < stop) {
        e = mapping(as[K](k))
        if (i == start) {
          fk = k
          e = e.copy(prev = Ø)
        }
        if (i >= start) {
          if (i == stop - 1) {
            lk = k
            e = e.copy(next = Ø)
          }
          bdr.addOne(k, e)
        }
        if (e.next != Ø) k = e.nextKey
        i += 1
      }
      new KeyLinkedSeqMap[K, V](fk, lk, bdr.result())
    }
  }*/

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
object KeyLinkedSeqMap extends MapFactory[KeyLinkedSeqMap] {
  private val Ø = new AnyRef
  @inline private def as[T](k: Any): T = k.asInstanceOf[T]

  val Empty = new KeyLinkedSeqMap[Nothing, Nothing]()
  override def empty[K, V] = Empty.asInstanceOf[KeyLinkedSeqMap[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]) = {
    it match {
      case map: KeyLinkedSeqMap[K, V] => map
      case _ => (newBuilder[K, V] ++= it).result()
    }
  }

  override def newBuilder[K, V] = new Builder[K, V]()

  final case class Entry[K, +V](prev: Any, var value: V @uncheckedVariance, var next: Any) {
    def prevKey = as[K](prev)
    def nextKey = as[K](next)
    override def toString = s"Entry(${if (prev == Ø) "∅" else prev},$value,${if (next == Ø) "∅" else next})"
  }
  object Entry {
    def apply[K, V](value: V) = new Entry[K, V](Ø, value, Ø)
    def apply[K, V](prevKey: K, value: V) = new Entry[K, V](prevKey, value, Ø)
  }
  private[immutable] final class Builder[K, V] extends mutable.Builder[(K, V), KeyLinkedSeqMap[K, V]] {
    private[this] val builder = new HashMapBuilder[K, Entry[K, V]]
    private[this] var first: Any = Ø
    private[this] var last: Any = Ø
    private[this] var previousEntry: Entry[K, V] = _
    private[this] var aliased: KeyLinkedSeqMap[K, V] = _

    def isEmpty = first == Ø

    override def clear(): Unit = {
      builder.clear()
      first = Ø
      last = Ø
      previousEntry = null
      aliased = null
    }

    override def result(): KeyLinkedSeqMap[K, V] = {
      if (aliased eq null) {
        if (isEmpty) {
          aliased = empty
        } else {
          aliased = new KeyLinkedSeqMap(first, last, builder.result())
        }
      }
      aliased
    }
    def addOne(key: K, value: V): this.type = {
      if (aliased ne null) {
        aliased = aliased.updated(key, value)
      } else {
        builder.getOrElse(key, null) match {
          case e @ Entry(_, v, _) if value != v =>
            e.value = value
          case null if isEmpty =>
            first = key
            last = key
            previousEntry = Entry[K, V](value)
            builder.addOne(key, previousEntry)
          case null =>
            val lastKey = as[K](last)
            previousEntry.next = key
            val entry = Entry[K, V](lastKey, value)
            builder.addOne(key, entry)
            last = key
            previousEntry = entry
        }
      }
      this
    }

    override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
  }
}