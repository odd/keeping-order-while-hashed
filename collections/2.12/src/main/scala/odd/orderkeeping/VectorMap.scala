package odd
package orderkeeping

import scala.annotation.tailrec
import scala.collection.generic.ImmutableMapFactory
import scala.collection.immutable.{AbstractMap, HashMap, MapLike, VectorBuilder}
import scala.collection.mutable

final class VectorMap[K, +V] private[orderkeeping] (
        private val fields: Vector[Any],
        private val underlying: Map[K, (Int, V)], dummy: Boolean)
    extends AbstractMap[K, V]
        with MapLike[K, V, VectorMap[K, V]]
        with Serializable {

  import VectorMap._

  override def stringPrefix: String = "VectorMap"

  private def this(fields: Vector[K], underlying: Map[K, (Int, V)]) = {
    this(fields, underlying, false)
  }

  override val size = underlying.size

  override def isEmpty: Boolean = size == 0
  override def empty = VectorMap.empty[K, V]
  override def updated[V1 >: V](key: K, value: V1): VectorMap[K, V1] = {
    underlying.get(key) match {
      case Some((slot, _)) =>
        new VectorMap(fields, underlying.updated[(Int, V1)](key, (slot, value)), false)
      case None =>
        new VectorMap(fields :+ key, underlying.updated[(Int, V1)](key, (fields.length, value)), false)
    }
  }
  override def +[V1 >: V](kv: (K, V1)) = updated(kv._1, kv._2)

  override def withDefault[V1 >: V](d: K => V1): Map[K, V1] =
    new Map.WithDefault(this, d)

  override def withDefaultValue[V1 >: V](d: V1): Map[K, V1] =
    new Map.WithDefault[K, V1](this, _ => d)

  def get(key: K): Option[V] = underlying.get(key) match {
    case Some(v) => Some(v._2)
    case None    => None
  }

  @tailrec
  private def field(slot: Int): (Int, K) = {
    fields(slot) match {
      case Tombstone.Kinless =>
        (-1, null.asInstanceOf[K])
      case Tombstone.NextOfKin(distance) =>
        field(slot + distance)
      case k =>
        (slot, k.asInstanceOf[K])
    }
  }

  def removed(key: K): VectorMap[K, V] = {
    if (isEmpty) VectorMap.empty
    else {
      var fs = fields
      val sz = fs.size
      underlying.get(key) match {
        case Some(_) if size == 1 => VectorMap.empty
        case Some((slot, _)) =>
          val s = field(slot)._1
          // Calculate distance to next of kin
          val d =
            if (s < sz - 1) fs(s + 1) match {
              case Tombstone.Kinless => 0
              case Tombstone.NextOfKin(d) => d + 1
              case _ => 1
            } else 0
          fs = fs.updated(s, Tombstone(d))
          if (s > 0) {
            // Adjust distance to next of kin for all preceding tombstones
            var t = s - 1
            var prev = fs(t)
            while (t >= 0 && prev.isInstanceOf[Tombstone]) {
              fs = prev match {
                case Tombstone.Kinless => throw new IllegalStateException("kinless tombstone found in prefix: " + key)
                case Tombstone.NextOfKin(_) if d == 0 => fs.updated(t, Tombstone.Kinless)
                case Tombstone.NextOfKin(d) => fs.updated(t, Tombstone(d + 1))
                case _ => fs
              }
              t -= 1
              if (t >= 0) prev = fs(t)
            }
          }
          new VectorMap(fs, underlying - key, false)
        case _ =>
          this
      }
    }
  }
  override def -(key: K) = removed(key)

  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    private[this] val fieldsLength = fields.length
    private[this] var slot = -1
    private[this] var key: K = null.asInstanceOf[K]

    private[this] def advance(): Unit = {
      val nextSlot = slot + 1
      if (nextSlot >= fieldsLength) {
        slot = fieldsLength
        key = null.asInstanceOf[K]
      } else {
        field(nextSlot) match {
          case (-1, _) =>
            slot = fieldsLength
            key = null.asInstanceOf[K]
          case (s, k) =>
            slot = s
            key = k
        }
      }
    }

    advance()

    override def hasNext: Boolean = slot < fieldsLength

    override def next(): (K, V) = {
      if (!hasNext) throw new NoSuchElementException("next called on depleted iterator")
      val result = (key, underlying(key)._2)
      advance()
      result
    }
  }

  override def contains(key: K): Boolean = underlying.contains(key)

  override def head: (K, V) = iterator.next()

  override def last: (K, V) = {
    val last = fields
        .reverseIterator
        .find(!_.isInstanceOf[Tombstone])
        .get
        .asInstanceOf[K]
    (last, underlying(last)._2)
  }

  override def lastOption: Option[(K, V)] = {
    fields
        .reverseIterator
        .find(!_.isInstanceOf[Tombstone])
        .map { f =>
          val last = f.asInstanceOf[K]
          (last, underlying(last)._2)
        }
  }

  override def tail: VectorMap[K, V] = {
    val (slot, key) = field(0)
    new VectorMap(fields.drop(slot + 1), underlying - key, false)
  }

  override def init: VectorMap[K, V] = {
    val lastSlot = size - 1
    val (slot, key) = field(lastSlot)
    new VectorMap(fields.dropRight(slot - lastSlot + 1), underlying - key, false)
  }

  override def keys: Vector[K] = keysIterator.toVector

  override def values: Iterable[V] = new Iterable[V] {
    override def iterator: Iterator[V] = keysIterator.map(underlying(_)._2)
  }
}

object VectorMap extends ImmutableMapFactory[VectorMap] {
  private[VectorMap] sealed trait Tombstone
  private[VectorMap] object Tombstone {
    case object Kinless extends Tombstone {
      override def toString = "⤞"
    }
    final case class NextOfKin private[Tombstone] (distance: Int) extends Tombstone {
      override def toString = "⥅" + distance
    }
    def apply(distance: Int): Tombstone =
      if (distance <= 0) Kinless
      else NextOfKin(distance)
  }

  private[this] final val EmptyMap: VectorMap[Nothing, Nothing] =
    new VectorMap[Nothing, Nothing](Vector.empty[Nothing], HashMap.empty[Nothing, (Int, Nothing)])

  def empty[K, V]: VectorMap[K, V] = EmptyMap.asInstanceOf[VectorMap[K, V]]

  /*
  def from[K, V](it: collection.IterableOnce[(K, V)]): VectorMap[K, V] =
    it match {
      case vm: VectorMap[K, V] => vm
      case _                   => (newBuilder[K, V] ++= it).result()
    }
  */

  override def newBuilder[K, V]: mutable.Builder[(K, V), VectorMap[K, V]] = new mutable.MapBuilder[K, V, VectorMap[K, V]](VectorMap.empty)

  import reftree.core._
  /*
  implicit def `CharVector RefTree`: ToRefTree[Vector[Char]] = { v: Vector[Char] =>
    RefTree.Ref(v, Seq(
      v.start
      //simpleSeq[Int, K].refTree(ordering).toField.withName("ordering"),
      ordering.refTree.toField.withName("ordering"),
      ordinalMap[K, V].refTree(mapping).toField.withName("mapping"))
    ).rename("VectorMap")
  }
  */
  implicit def `VectorMap RefTree`[K : ToRefTree, V : ToRefTree]: ToRefTree[VectorMap[K, V]] = { map: VectorMap[K, V] =>
    RefTree.Ref(map, Seq(
      collection.immutable.`Vector RefTree`[Char].refTree(map.fields.asInstanceOf[Vector[Char]]).toField.withName("ordering"),
      mappingStub[K, (Int, V)].refTree(map.underlying).toField.withName("mapping"))
    ).rename("VectorMap")
  }
}

/*
private final class VectorMapBuilder[K, V] extends mutable.Builder[(K, V), VectorMap[K, V]] {
  private[this] val vectorBuilder = new VectorBuilder[K]
  private[this] val mapBuilder = new mutable.MapBuilder[K, (Int, V), HashMap[K, (Int, V)]](HashMap.empty)
  private[this] var aliased: VectorMap[K, V] = _

  override def clear(): Unit = {
    vectorBuilder.clear()
    mapBuilder.clear()
    aliased = null
  }

  override def result(): VectorMap[K, V] = {
    if (aliased eq null) {
      aliased = new VectorMap[K, V](vectorBuilder.result(), mapBuilder.result(), true)
    }
    aliased
  }
  def +=(key: K, value: V): this.type = {
    if (aliased ne null) {
      aliased = aliased.updated(key, value)
    } else {
      mapBuilder.getOrElse(key, null) match {
        case (slot, _) =>
          mapBuilder.addOne(key, (slot, value))
        case null =>
          val vectorSize = vectorBuilder.size
          vectorBuilder.addOne(key)
          mapBuilder.addOne(key, (vectorSize, value))
      }
    }
    this
  }

  override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
}
*/
