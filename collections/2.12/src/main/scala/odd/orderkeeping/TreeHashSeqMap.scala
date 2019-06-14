package odd
package orderkeeping

import scala.collection.immutable

class TreeHashSeqMap[K, +V] private (
      private val ordering: immutable.TreeMap[Int, K] = immutable.TreeMap.empty[Int, K],
      private val mapping: immutable.HashMap[K, (Int, V)] = immutable.HashMap.empty[K, (Int, V)],
      private val ordinal: Int = 0)
    extends Map[K, V] {

  override def +[V1 >: V](kv: (K, V1)): TreeHashSeqMap[K, V1] = {
    if (ordinal == Int.MaxValue) {
      if (mapping.size == Int.MaxValue) throw new IllegalStateException(s"Map is full.")
      else { // Insert all current entries in a new instance (which restarts the ordinals from Int.MinValue)
        var copy = new TreeHashSeqMap[K, V]()
        for ((k, v) <- this) {
          copy += (k -> v)
        }
        copy
      }
    } else new TreeHashSeqMap(ordering.updated(ordinal, kv._1), mapping.updated(kv._1, ordinal -> kv._2), ordinal + 1)
  }
  override def -(key: K): TreeHashSeqMap[K, V] = {
    mapping.get(key) match {
      case None => this
      case Some((o, _)) => new TreeHashSeqMap(ordering - o, mapping - key, ordinal)
    }
  }
  override def get(key: K): Option[V] = mapping.get(key).map(_._2)
  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)]() {
    private[this] val iter = ordering.iterator
    override def hasNext = iter.hasNext
    override def next() = {
      val (_, k) = iter.next()
      k -> mapping(k)._2
    }
  }
  override def stringPrefix = "TreeHashSeqMap"
}
object TreeHashSeqMap {
  val Empty = TreeHashSeqMap()
  def empty[K, V] = Empty.asInstanceOf[TreeHashSeqMap[K, V]]
  def apply[K, V](entries: (K, V)*): TreeHashSeqMap[K, V] = {
    var map = new TreeHashSeqMap[K, V]()
    val iter = entries.iterator
    while (iter.hasNext) {
      map += iter.next()
    }
    map
  }
  import reftree.core._
  implicit def `TreeHashSeqMap RefTree`[K : ToRefTree, V : ToRefTree]: ToRefTree[TreeHashSeqMap[K, V]] = { map: TreeHashSeqMap[K, V] =>
    RefTree.Ref(map, Seq(
      map.ordinal.refTree.toField.withName("ordinal"),
      map.ordering.refTree.toField.withName("ordering"),
      mappingStub[K, (Int, V)].refTree(map.mapping).toField.withName("mapping"))
    ).rename("TreeHashSeqMap")
  }
}