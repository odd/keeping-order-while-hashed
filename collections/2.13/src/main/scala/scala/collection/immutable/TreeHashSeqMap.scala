package scala
package collection
package immutable

class TreeHashSeqMap[K, +V] private (
      private val mapping: immutable.Map[K, (Int, V)] = immutable.HashMap.empty[K, (Int, V)],
      private val ordering: immutable.TreeMap[Int, K] = immutable.TreeMap.empty[Int, K],
      private val ordinal: Int = 0)
    extends AbstractMap[K, V]
        with SeqMap[K, V]
        with MapOps[K, V, TreeHashSeqMap, TreeHashSeqMap[K, V]]
        with StrictOptimizedIterableOps[(K, V), Iterable, TreeHashSeqMap[K, V]]
        with StrictOptimizedMapOps[K, V, TreeHashSeqMap, TreeHashSeqMap[K, V]]
        with MapFactoryDefaults[K, V, TreeHashSeqMap, Iterable] {

  override protected[this] def className = "TreeHashSeqMap"
  override def mapFactory: MapFactory[TreeHashSeqMap] = TreeHashSeqMap
  override def isEmpty = mapping.isEmpty

  override def updated[V1 >: V](key: K, value: V1): TreeHashSeqMap[K, V1] = {
    if (ordinal == Int.MaxValue) {
      if (mapping.size == Int.MaxValue) throw new IllegalStateException(s"Map is full.")
      else { // Insert all current entries in a new instance (which restarts the ordinals from Int.MinValue)
        var copy = new TreeHashSeqMap[K, V]()
        for ((k, v) <- this) {
          copy = copy.updated(k, v)
        }
        copy.updated(key, value)
      }
    } else {
      mapping.get(key) match {
        case None =>
          new TreeHashSeqMap(mapping.updated(key, ordinal -> value), ordering.updated(ordinal, key), ordinal + 1)
        case Some((_, v)) if v == value =>
          this
        case Some((o, _)) =>
          new TreeHashSeqMap(mapping.updated(key, o -> value), ordering, ordinal)
      }
    }
  }
  override def removed(key: K): TreeHashSeqMap[K, V] = {
    mapping.get(key) match {
      case None => this
      case Some((o, _)) => new TreeHashSeqMap(mapping - key, ordering - o, ordinal)
    }
  }
  override def tail: TreeHashSeqMap[K, V] = drop(1)
  override def init: TreeHashSeqMap[K, V] = dropRight(1)
  override def drop(n: Int) = {
    if (isEmpty) throw new NoSuchElementException("drop on empty")
    //else slice(n, size)
    else {
      var mng = mapping
      var ong = ordering
      val iter = ordering.iterator
      var i = 0
      while (i < n && iter.hasNext) {
        val (o, k) = iter.next()
        val (_, v) = mapping(k)
        ong -= o
        mng -= k
        i += 1
      }
      new TreeHashSeqMap[K, V](mng, ong, ordinal)
    }
  }
  override def dropRight(n: Int) = {
    if (isEmpty) throw new NoSuchElementException("drop right on empty")
    //else slice(0, size - n)
    else {
      val (front, rear) = ordering.splitAt(size - n)
      val mng = mapping -- rear.map(_._2)
      new TreeHashSeqMap[K, V](mng, front, ordinal)
    }
  }
  def dropRight2(n: Int) = {
    if (isEmpty) throw new NoSuchElementException("drop right on empty")
    //else slice(0, size - n)
    else {
      val m = size - n
      var mng = mapping
      var ong = ordering
      val iter = ordering.iterator
      var i = 0
      while (iter.hasNext) {
        if (i >= m) {
          val (o, k) = iter.next()
          val (_, v) = mapping(k)
          ong -= o
          mng -= k
        }
        i += 1
      }
      new TreeHashSeqMap[K, V](mng, ong, ordinal)
    }
  }
  override def dropWhile(p: ((K, V)) => Boolean): TreeHashSeqMap[K, V] = {
    if (isEmpty) throw new NoSuchElementException("drop on empty")
    else {
      var mng: Map[K, (Int, V)] = mapping
      var ong: TreeMap[Int, K] = ordering
      val iter = ordering.iterator
      while (iter.hasNext) {
        val (o, k) = iter.next()
        val (_, v) = mapping(k)
        if (p(k -> v)) {
          ong -= o
          mng -= k
        } else return new TreeHashSeqMap[K, V](mng, ong, ordinal)
      }
      empty
    }
  }
  override def take(n: Int) = slice(0, n)
  override def takeRight(n: Int) = slice(size - n, size)
  override def get(key: K): Option[V] = mapping.get(key).map(_._2)
  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)]() {
    private[this] val iter = ordering.iterator
    override def hasNext = iter.hasNext
    override def next() = {
      val (_, k) = iter.next()
      k -> mapping(k)._2
    }
  }
}
object TreeHashSeqMap extends MapFactory[TreeHashSeqMap] {
  val Empty = new TreeHashSeqMap[Nothing, Nothing](HashMap.empty, TreeMap.empty)
  def empty[K, V] = Empty.asInstanceOf[TreeHashSeqMap[K, V]]
  override def from[K, V](it: IterableOnce[(K, V)]) = {
    it match {
      case map: TreeHashSeqMap[K, V] => map
      case _ => (newBuilder[K, V] ++= it).result()
    }
  }

  override def newBuilder[K, V] = new Builder[K, V]()

  private[immutable] final class Builder[K, V] extends mutable.Builder[(K, V), TreeHashSeqMap[K, V]] {
    private[this] val mappingBuilder = new MapBuilderImpl[K, (Int, V)]
    private[this] var orderingBuilder = TreeMap.newBuilder[Int, K]
    private[this] var ordinal = 0
    private[this] var aliased: TreeHashSeqMap[K, V] = _

    def isEmpty = ordinal == 0

    override def clear(): Unit = {
      mappingBuilder.clear()
      orderingBuilder.clear()
      ordinal = 0
      aliased = null
    }

    override def result(): TreeHashSeqMap[K, V] = {
      if (aliased eq null) {
        if (isEmpty) {
          aliased = empty
        } else {
          aliased = new TreeHashSeqMap(mappingBuilder.result(), orderingBuilder.result(), ordinal)
        }
      }
      aliased
    }
    def addOne(key: K, value: V): this.type = {
      if (aliased ne null) {
        aliased = aliased.updated(key, value)
      } else {
        mappingBuilder.getOrElse(key, null) match {
          case (o, v) if value != v =>
            mappingBuilder.addOne(key, o -> value)
          case null =>
            mappingBuilder.addOne(key, ordinal -> value)
            orderingBuilder += ordinal -> key
            ordinal += 1
        }
      }
      this
    }

    override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
  }
}