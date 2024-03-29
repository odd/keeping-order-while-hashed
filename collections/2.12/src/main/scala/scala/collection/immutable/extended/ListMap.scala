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
package extended

import generic._
import scala.annotation.tailrec
import reftree.core._

/**
 * $factoryInfo
 *
 * Note that each element insertion takes O(n) time, which means that creating a list map with
 * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
 * elements.
 *
 * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#list-maps "Scala's Collection Library overview"]]
 * section on `List Maps` for more information.
 * @since 1
 * @define Coll ListMap
 * @define coll list map
 */
object ListMap extends ImmutableMapFactory[ListMap] {

  /**
   * $mapCanBuildFromInfo
   */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ListMap[A, B]] =
    new MapCanBuildFrom[A, B]

  def empty[A, B]: ListMap[A, B] = EmptyListMap.asInstanceOf[ListMap[A, B]]

  @SerialVersionUID(-8256686706655863282L)
  private object EmptyListMap extends ListMap[Any, Nothing]

  implicit def `ListMap RefTree`[A: ToRefTree, B: ToRefTree]: ToRefTree[ListMap[A, B]] =
    new ToRefTree[ListMap[A, B]] {
      def refTree(map: ListMap[A, B]): RefTree = map match {
        case EmptyListMap ⇒ RefTree.Ref(ListMap.empty[A, B], Seq.empty).rename("Ø")
        case _ ⇒ RefTree.Ref(ListMap.empty[A, B], Seq.empty).rename("Ø")
          RefTree.Ref(map, Seq(map.key.refTree.toField, map.value.refTree.toField, map.next.refTree.toField)).rename("Node")
      }
    }
}

/**
 * This class implements immutable maps using a list-based data structure. List map iterators and
 * traversal methods visit key-value pairs in the order whey were first inserted.
 *
 * Entries are stored internally in reversed insertion order, which means the newest key is at the
 * head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and `init`
 * are O(1). Other operations, such as inserting or removing entries, are also O(n), which makes
 * this collection suitable only for a small number of elements.
 *
 * Instances of `ListMap` represent empty maps; they can be either created by calling the
 * constructor directly, or by applying the function `ListMap.empty`.
 *
 * @tparam A the type of the keys contained in this list map
 * @tparam B the type of the values associated with the keys
 *
 * @author Matthias Zenger
 * @author Martin Odersky
 * @since 1
 * @define Coll ListMap
 * @define coll list map
 * @define mayNotTerminateInf
 * @define willNotTerminateInf
 */
@SerialVersionUID(301002838095710379L)
sealed class ListMap[A, +B] extends AbstractMap[A, B]
    with Map[A, B]
    with MapLike[A, B, ListMap[A, B]]
    with Serializable {

  override def empty = ListMap.empty

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def get(key: A): Option[B] = None

  override def updated[B1 >: B](key: A, value: B1): ListMap[A, B1] = new Node[B1](key, value)

  def +[B1 >: B](kv: (A, B1)): ListMap[A, B1] = new Node[B1](kv._1, kv._2)
  def -(key: A): ListMap[A, B] = this

  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): ListMap[A, B1] =
    if (xs.isEmpty) this
    else ((repr: ListMap[A, B1]) /: xs) (_ + _)

  def iterator: Iterator[(A, B)] = {
    def reverseList = {
      var curr: ListMap[A, B] = this
      var res: List[(A, B)] = Nil
      while (!curr.isEmpty) {
        res = (curr.key, curr.value) :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }

  def key: A = throw new NoSuchElementException("key of empty map")
  def value: B = throw new NoSuchElementException("value of empty map")
  def next: ListMap[A, B] = throw new NoSuchElementException("next of empty map")

  override def stringPrefix = "ListMap"

  /**
   * Represents an entry in the `ListMap`.
   */
  @SerialVersionUID(-6453056603889598734L)
  protected class Node[B1 >: B](override val key: A,
                                override val value: B1) extends ListMap[A, B1] with Serializable {

    override def size: Int = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(cur: ListMap[A, B1], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    override def isEmpty: Boolean = false

    override def apply(k: A): B1 = applyInternal(this, k)

    @tailrec private[this] def applyInternal(cur: ListMap[A, B1], k: A): B1 =
      if (cur.isEmpty) throw new NoSuchElementException("key not found: " + k)
      else if (k == cur.key) cur.value
      else applyInternal(cur.next, k)

    override def get(k: A): Option[B1] = getInternal(this, k)

    @tailrec private[this] def getInternal(cur: ListMap[A, B1], k: A): Option[B1] =
      if (cur.isEmpty) None
      else if (k == cur.key) Some(cur.value)
      else getInternal(cur.next, k)

    override def contains(k: A): Boolean = containsInternal(this, k)

    @tailrec private[this] def containsInternal(cur: ListMap[A, B1], k: A): Boolean =
      if(cur.isEmpty) false
      else if (k == cur.key) true
      else containsInternal(cur.next, k)

    override def updated[B2 >: B1](k: A, v: B2): ListMap[A, B2] = {
      val m = this - k
      new m.Node[B2](k, v)
    }

    override def +[B2 >: B1](kv: (A, B2)): ListMap[A, B2] = {
      val m = this - kv._1
      new m.Node[B2](kv._1, kv._2)
    }

    override def -(k: A): ListMap[A, B1] = removeInternal(k, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListMap[A, B1], acc: List[ListMap[A, B1]]): ListMap[A, B1] =
      if (cur.isEmpty) acc.last
      else if (k == cur.key) (cur.next /: acc) { case (t, h) => new t.Node(h.key, h.value) }
      else removeInternal(k, cur.next, cur :: acc)

    override def next: ListMap[A, B1] = ListMap.this

    override def last: (A, B1) = (key, value)
    override def init: ListMap[A, B1] = next
  }
}
