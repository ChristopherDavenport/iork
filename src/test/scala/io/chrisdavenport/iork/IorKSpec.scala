package io.chrisdavenport.iork

import org.specs2._
import cats._
import cats.data._
import cats.implicits._

object IorKSpec extends mutable.Specification with ScalaCheck {

  "IorK" should {
    "fold correctly on left" >> prop { l: List[Int] => 
      val f : List ~> Option = λ[List ~> Option](x => x.headOption)
      val g : Id ~> Option = λ[Id ~> Option](x => Some(x))
      val s : Tuple2K[List, Id, ?] ~> Option = λ[Tuple2K[List, Id, ?] ~> Option](x => None)

      IorK.leftc[List, Id, Int](l).fold(f, g, s) must_=== l.headOption
    }
    "fold correctly on right" >> prop { i: Int => 
      val f : List ~> Option = λ[List ~> Option](x => x.headOption)
      val g : Id ~> Option = λ[Id ~> Option](x => Some(x))
      val s : Tuple2K[List, Id, ?] ~> Option = λ[Tuple2K[List, Id, ?] ~> Option](x => None)

      IorK.rightc[List, Id, Int](i).fold(f, g, s) must_=== i.some
    }
    "fold correctly on both" >> prop { (l: List[Int], i: Int) => 
      val f : List ~> Option = λ[List ~> Option](x => x.headOption)
      val g : Id ~> Option = λ[Id ~> Option](x => Some(x))
      val s : Tuple2K[List, Id, ?] ~> Option = λ[Tuple2K[List, Id, ?] ~> Option](x => None)

      IorK.bothc[List, Id, Int](l, i).fold(f, g, s) must_=== None
    }
  }

}