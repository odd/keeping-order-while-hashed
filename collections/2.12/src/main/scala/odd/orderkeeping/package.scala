package odd

import scala.collection.immutable.ListMap

package object orderkeeping {
  import reftree.core._
  import reftree.contrib.SimplifiedInstances.{map => simpleMap}
  implicit def tuple2[A: ToRefTree, B: ToRefTree]: ToRefTree[(A, B)] = ToRefTree[(A, B)] { t ⇒
    RefTree.Ref(t, Seq()).rename(s"(${t._1}, ${t._2})")
  }
  implicit def tuple3[A: ToRefTree, B: ToRefTree, C: ToRefTree]: ToRefTree[(A, B, C)] = ToRefTree[(A, B, C)] { t ⇒
    RefTree.Ref(t, Seq()).rename(s"(${t._1}, ${t._2}, ${t._3})")
  }
  implicit def tuple4[A: ToRefTree, B: ToRefTree, C: ToRefTree, D : ToRefTree]: ToRefTree[(A, B, C, D)] = ToRefTree[(A, B, C, D)] { t ⇒
    RefTree.Ref(t, Seq()).rename(s"(${t._1}, ${t._2}, ${t._3}, ${t._4})")
  }
  implicit def tuple9[A: ToRefTree, B: ToRefTree, C: ToRefTree, D : ToRefTree, E : ToRefTree, F : ToRefTree, G : ToRefTree, H : ToRefTree, I : ToRefTree]: ToRefTree[(A, B, C, D, E, F, G, H, I)] = ToRefTree[(A, B, C, D, E, F, G, H, I)] { t ⇒
    RefTree.Ref(t, Seq()).rename(s"(${t._1}, ${t._2}, ${t._3}, ${t._4}, ${t._5}, ${t._6}, ${t._7}, ${t._8}, ${t._9}")
  }
  implicit def tuple10[A: ToRefTree, B: ToRefTree, C: ToRefTree, D : ToRefTree, E : ToRefTree, F : ToRefTree, G : ToRefTree, H : ToRefTree, I : ToRefTree, J : ToRefTree]: ToRefTree[(A, B, C, D, E, F, G, H, I, J)] = ToRefTree[(A, B, C, D, E, F, G, H, I, J)] { t ⇒
    RefTree.Ref(t, Seq()).rename(s"(${t._1}, ${t._2}, ${t._3}, ${t._4}, ${t._5}, ${t._6}, ${t._7}, ${t._8}, ${t._9}, ${t._10}")
  }
  implicit def tuple11[A: ToRefTree, B: ToRefTree, C: ToRefTree, D : ToRefTree, E : ToRefTree, F : ToRefTree, G : ToRefTree, H : ToRefTree, I : ToRefTree, J : ToRefTree, K : ToRefTree]: ToRefTree[(A, B, C, D, E, F, G, H, I, J, K)] = ToRefTree[(A, B, C, D, E, F, G, H, I, J, K)] { t ⇒
    RefTree.Ref(t, Seq()).rename(s"(${t._1}, ${t._2}, ${t._3}, ${t._4}, ${t._5}, ${t._6}, ${t._7}, ${t._8}, ${t._9}, ${t._10}, ${t._11}")
  }
  implicit def tuple12[A: ToRefTree, B: ToRefTree, C: ToRefTree, D : ToRefTree, E : ToRefTree, F : ToRefTree, G : ToRefTree, H : ToRefTree, I : ToRefTree, J : ToRefTree, K : ToRefTree, L : ToRefTree]: ToRefTree[(A, B, C, D, E, F, G, H, I, J, K, L)] = ToRefTree[(A, B, C, D, E, F, G, H, I, J, K, L)] { t ⇒
    RefTree.Ref(t, Seq()).rename(s"(${t._1}, ${t._2}, ${t._3}, ${t._4}, ${t._5}, ${t._6}, ${t._7}, ${t._8}, ${t._9}, ${t._10}, ${t._11}, ${t._12}")
  }
  implicit def ordinalMap[K: ToRefTree, V: ToRefTree]: ToRefTree[Map[K, (Int, V)]] = ToRefTree[Map[K, (Int, V)]] { map ⇒
    if (map.isEmpty) RefTree.Null() else {
      RefTree.Ref(map, map.toSeq map {
        case entry @ (k, pair @ (o, v)) ⇒
          RefTree.Ref(entry, Seq(
            k.refTree.toField,
            RefTree.Ref(pair, Seq()).rename(s"($o, $v)").toField)
          ).rename("MapEntry").toField
      }).rename("Map")
    }
  }
  val stub = Map.empty
  implicit def mappingStub[A: ToRefTree, B: ToRefTree]: ToRefTree[Map[A, B]] = ToRefTree[Map[A, B]] { map ⇒
    if (map.isEmpty) RefTree.Null() else {
      RefTree.Ref(stub, Seq.empty).rename("<<HashMap[K, (Int, V)]>>")
    }
  }
  implicit def mapping[A: ToRefTree, B: ToRefTree]: ToRefTree[Map[A, B]] = ToRefTree[Map[A, B]] { map ⇒
    if (map.isEmpty) RefTree.Null() else {
      RefTree.Ref(map, map.toSeq map {
        case (k, v) ⇒
          v.refTree.toField.withName(k.toString)
      }).rename("HashMap")
    }
  }
}