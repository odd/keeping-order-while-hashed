package scala
package collection

import reftree.core._
import reftree.util.Reflection.PrivateFields

package object immutable {
  private def vectorArrayRefTree[A: ToRefTree](value: Array[AnyRef], depth: Int): RefTree = {
    val arr = (value takeWhile(x => x != null)) ++ Array(null, '.', '.', '.', null)
    RefTree.Ref(arr, arr.map { x ⇒
      if (x == null) RefTree.Null()
      else if (depth > 0) vectorArrayRefTree[A](x.asInstanceOf[Array[AnyRef]].takeWhile(_ != null), depth - 1)
      else x.asInstanceOf[A].refTree
    } map (_.toField)).rename("Array")
  }

  implicit def `Vector RefTree`[A: ToRefTree]: ToRefTree[Vector[A]] = ToRefTree[Vector[A]] { value ⇒
    val start = value.startIndex.refTree.toField.withName("start")
    val end = value.endIndex.refTree.toField.withName("end")
    val focus = RefTree.Val.formatted(value.privateField[Int]("focus"))(_.toBinaryString).toField.withName("focus")
    val depth = value.depth.refTree.toField.withName("depth")
    val layers = Seq(
      value.display0, value.display1,
      value.display2, value.display3,
      value.display4, value.display5
    ).zipWithIndex.map {
      case (layer, d) if d < value.depth ⇒ vectorArrayRefTree[A](layer, d)
      case (layer, _) ⇒ RefTree.Null()
    }.map(_.toField)
    RefTree.Ref(
      value,
      Seq(start, end, focus, depth) ++ layers
    )
  }
}

