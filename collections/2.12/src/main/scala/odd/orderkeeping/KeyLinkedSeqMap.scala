package odd
package orderkeeping

import scala.collection.immutable._

class KeyLinkedSeqMap[K, +V] private(
      private val f: Any = KeyLinkedSeqMap.Ø,
      private val l: Any = KeyLinkedSeqMap.Ø,
      private val mapping: HashMap[K, KeyLinkedSeqMap.Entry[K, V]] = HashMap.empty[K, KeyLinkedSeqMap.Entry[K, V]])
    extends Map[K, V] {
  import KeyLinkedSeqMap._
  override def isEmpty = l == Ø && mapping.isEmpty
  @inline private def isSingular = mapping.size == 1
  @inline private def firstKey: K = asKey(f)
  @inline private def lastKey: K = asKey(l)
  override def +[V1 >: V](kv: (K, V1)): KeyLinkedSeqMap[K, V1] = {
    val (k, v) = kv
    if (isEmpty) new KeyLinkedSeqMap(k, k, HashMap(k -> Entry[K, V1](Ø, v, Ø)))
    else mapping.get(k) match {
      case None =>
        new KeyLinkedSeqMap[K, V1](f, k, mapping.updated(lastKey, mapping(lastKey).copy(next = k)) + (k -> Entry(lastKey, v, Ø)))
      case Some(Entry(Ø, _, Ø)) =>
        new KeyLinkedSeqMap[K, V1](k, k, mapping.updated(k, Entry(Ø, v, Ø)))
      case Some(Entry(Ø, _, n)) =>
        val nextKey = asKey[K](n)
        val nextEntry = mapping(nextKey)
        new KeyLinkedSeqMap[K, V1](k, l, mapping.updated(nextKey, nextEntry.copy(prev = k)).updated(k, Entry(Ø, v, nextKey)))
      case Some(Entry(p, _, Ø)) =>
        val prevKey = asKey[K](p)
        val prevEntry = mapping(prevKey)
        new KeyLinkedSeqMap[K, V1](f, k, mapping.updated(prevKey, prevEntry.copy(next = k)).updated(k, Entry(prevKey, v, Ø)))
      case Some(Entry(p, _, n)) =>
        new KeyLinkedSeqMap[K, V1](f, l, mapping + (k -> Entry(p, v, n)))
    }
  }
  override def -(key: K): KeyLinkedSeqMap[K, V] = {
    mapping.get(key) match {
      case None => this
      case Some(Entry(Ø, _, Ø)) =>
        KeyLinkedSeqMap.empty
      case Some(Entry(Ø, _, n)) =>
        val nextKey = asKey[K](n)
        val nextEntry = mapping(nextKey)
        new KeyLinkedSeqMap(n, l, mapping.updated(nextKey, nextEntry.copy(prev = Ø)))
      case Some(Entry(p, _, Ø)) =>
        val prevKey = asKey[K](p)
        val prevEntry = mapping(prevKey)
        new KeyLinkedSeqMap(f, p, mapping.updated(prevKey, prevEntry.copy(next = Ø)))
      case Some(Entry(p, _, n)) =>
        val prevKey = asKey[K](p)
        val prevEntry = mapping(prevKey)
        val nextKey = asKey[K](n)
        val nextEntry = mapping(nextKey)
        new KeyLinkedSeqMap(f, l, mapping.updated(prevKey, prevEntry.copy(next = nextKey)).updated(nextKey, nextEntry.copy(prev = prevKey)))
    }
  }
  override def get(key: K): Option[V] = mapping.get(key).map(_.value)
  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)]() {
    private[this] var k = f
    override def hasNext = k != Ø
    override def next() = {
      if (!hasNext) throw new IllegalStateException("next called on empty iterator")
      else {
        val key = asKey[K](k)
        val entry = mapping(key)
        k = entry.next
        key -> entry.value
      }
    }
  }
  override def stringPrefix = "KeyLinkedSeqMap"
}
object KeyLinkedSeqMap {
  private val Ø = new AnyRef
  @inline private def asKey[K](k: Any): K = k.asInstanceOf[K]
  val Empty = new KeyLinkedSeqMap[Nothing, Nothing]()
  def empty[K, V] = Empty.asInstanceOf[KeyLinkedSeqMap[K, V]]
  def apply[K, V](entries: (K, V)*): KeyLinkedSeqMap[K, V] = {
    var map = new KeyLinkedSeqMap[K, V]()
    val iter = entries.iterator
    while (iter.hasNext) {
      map += iter.next()
    }
    map
  }
  import reftree.core._
  import reftree.contrib.SimplifiedInstances.{map => simpleMap}
  final case class Entry[K, +V](prev: Any, value: V, next: Any) {
    def prevKey = asKey[K](prev)
    def nextKey = asKey[K](next)
    override def toString = s"Entry(${if (prev == Ø) "∅" else prev},$value,${if (next == Ø) "∅" else next})"
  }
  object Entry {
    implicit def `Entry RefTree`[K : ToRefTree, V : ToRefTree]: ToRefTree[Entry[K, V]] = { entry: Entry[K, V] =>
      val z = RefTree.Ref(Ø, Seq.empty).rename("∅")
      entry match {
          case Entry(Ø, v, Ø) =>
            RefTree.Ref(entry, Seq(
              z.toField,
              v.refTree.toField,
              z.toField)
            ).rename("Entry")
          case Entry(Ø, v, _) =>
            RefTree.Ref(entry, Seq(
              z.toField,
              v.refTree.toField,
              entry.nextKey.refTree.toField)
            ).rename("Entry")
          case Entry(_, v, Ø) =>
            RefTree.Ref(entry, Seq(
              entry.prevKey.refTree.toField,
              v.refTree.toField,
              z.toField)
            ).rename("Entry")
          case Entry(_, v, _) =>
            RefTree.Ref(entry, Seq(
              entry.prevKey.refTree.toField,
              v.refTree.toField,
              entry.nextKey.refTree.toField)
            ).rename("Entry")

        }
    }
  }
  implicit def `KeyLinkedSeqMap RefTree`[K : ToRefTree, V : ToRefTree]: ToRefTree[KeyLinkedSeqMap[K, V]] = { map: KeyLinkedSeqMap[K, V] =>
    map.f match {
      case Ø =>
        RefTree.Ref(KeyLinkedSeqMap.empty[K, V], Seq.empty).rename("Ø")
      case _ =>
        RefTree.Ref(map, Seq(
          map.firstKey.refTree.toField.withName("firstKey"),
          map.lastKey.refTree.toField.withName("lastKey"),
          mapping[K, Entry[K, V]].refTree(map.mapping).toField.withName("mapping"))
        ).rename("KeyLinkedSeqMap")
    }
  }
}