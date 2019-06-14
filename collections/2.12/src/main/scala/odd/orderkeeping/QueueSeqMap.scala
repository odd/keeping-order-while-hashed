package odd
package orderkeeping

import scala.collection.immutable
import scala.collection.immutable.Queue

class QueueSeqMap[K, +V] private (
      private val mapping: immutable.HashMap[K, V] = immutable.HashMap.empty[K, V],
      private val ordering: immutable.Queue[K] = immutable.Queue.empty[K])
    extends Map[K, V] {

  override def +[V1 >: V](kv: (K, V1)): QueueSeqMap[K, V1] = {
    val (k, v) = kv
    mapping.get(k) match {
      case None =>
        new QueueSeqMap(mapping.updated(k, v), ordering :+ k)
      case Some(_) =>
        new QueueSeqMap(mapping.updated(k, v), ordering)
    }
  }
  override def -(key: K): QueueSeqMap[K, V] = {
    mapping.get(key) match {
      case None => this
      case Some(_) => new QueueSeqMap(mapping - key, ordering.filterNot(_ == key))
    }
  }
  override def get(key: K): Option[V] = mapping.get(key)
  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)]() {
    private[this] val iter = ordering.iterator
    override def hasNext = iter.hasNext
    override def next() = {
      val k = iter.next()
      k -> mapping(k)
    }
  }
  override def stringPrefix = "QueueSeqMap"
}
object QueueSeqMap {
  val Empty = QueueSeqMap()
  def empty[K, V] = Empty.asInstanceOf[QueueSeqMap[K, V]]
  def apply[K, V](entries: (K, V)*): QueueSeqMap[K, V] = {
    var map = new QueueSeqMap[K, V]()
    val iter = entries.iterator
    while (iter.hasNext) {
      map += iter.next()
    }
    map
  }
  import reftree.core._
  import reftree.contrib.SimplifiedInstances.{map => simpleMap, list => simpleList}
  import reftree.util.Reflection.PrivateFields
  implicit def `Queue RefTree`[A: ToRefTree](implicit list: ToRefTree[List[A]]): ToRefTree[Queue[A]] =
    ToRefTree[Queue[A]] { value ⇒
      val front = simpleList[A].refTree(value.privateField[List[A]]("out")).toField.withName("front")
      val back = simpleList[A].refTree(value.privateField[List[A]]("in")).toField.withName("back")
      RefTree.Ref(value, Seq(front, back))
    }
  implicit def `QueueSeqMap RefTree`[K : ToRefTree, V : ToRefTree]: ToRefTree[QueueSeqMap[K, V]] = { map: QueueSeqMap[K, V] =>
    RefTree.Ref(map, Seq(
      map.ordering.refTree.toField.withName("ordering"),
      mappingStub[K, V].refTree(map.mapping).toField.withName("mapping"))
    ).rename("QueueSeqMap")
  }
}