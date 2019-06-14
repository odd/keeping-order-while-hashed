package scala
package collection
package immutable

class QueueSeqMap[K, +V] private (
      private val mapping: immutable.Map[K, V] = immutable.HashMap.empty[K, V],
      private val ordering: immutable.Queue[K] = immutable.Queue.empty[K])
    extends AbstractMap[K, V]
        with SeqMap[K, V]
        with MapOps[K, V, QueueSeqMap, QueueSeqMap[K, V]]
        with StrictOptimizedIterableOps[(K, V), Iterable, QueueSeqMap[K, V]]
        with StrictOptimizedMapOps[K, V, QueueSeqMap, QueueSeqMap[K, V]]
        with MapFactoryDefaults[K, V, QueueSeqMap, Iterable] {

  override protected[this] def className: String = "QueueSeqMap"
  override def mapFactory: MapFactory[QueueSeqMap] = QueueSeqMap

  override def isEmpty = mapping.isEmpty

  override def updated[V1 >: V](key: K, value: V1): QueueSeqMap[K, V1] = {
    mapping.get(key) match {
      case None =>
        new QueueSeqMap(mapping.updated(key, value), ordering :+ key)
      case Some(_) =>
        new QueueSeqMap(mapping.updated(key, value), ordering)
    }
  }
  override def removed(key: K): QueueSeqMap[K, V] = {
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
}
object QueueSeqMap extends MapFactory[QueueSeqMap] {
  val Empty = new QueueSeqMap(HashMap.empty, Queue.empty)
  def empty[K, V] = Empty.asInstanceOf[QueueSeqMap[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]) = {
    it match {
      case map: QueueSeqMap[K, V] => map
      case _ => (newBuilder[K, V] ++= it).result()
    }
  }

  override def newBuilder[K, V] = new Builder[K, V]()

  private[immutable] final class Builder[K, V] extends mutable.Builder[(K, V), QueueSeqMap[K, V]] {
    private[this] val mappingBuilder = new MapBuilderImpl[K, V]
    private[this] var orderingBuilder: mutable.Builder[K, Queue[K]] = _
    private[this] var aliased: QueueSeqMap[K, V] = _

    def isEmpty = orderingBuilder == null

    override def clear(): Unit = {
      mappingBuilder.clear()
      orderingBuilder = null
      aliased = null
    }

    override def result(): QueueSeqMap[K, V] = {
      if (aliased eq null) {
        if (isEmpty) {
          aliased = empty
        } else {
          aliased = new QueueSeqMap(mappingBuilder.result(), orderingBuilder.result())
        }
      }
      aliased
    }
    def addOne(key: K, value: V): this.type = {
      if (aliased ne null) {
        aliased = aliased.updated(key, value)
      } else {
        if (isEmpty) orderingBuilder = Queue.newBuilder[K]
        mappingBuilder.getOrElse(key, null) match {
          case null =>
            orderingBuilder += key
            mappingBuilder.addOne(key, value)
          case v if value != v =>
            mappingBuilder.addOne(key, value)
        }
      }
      this
    }

    override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
  }
}