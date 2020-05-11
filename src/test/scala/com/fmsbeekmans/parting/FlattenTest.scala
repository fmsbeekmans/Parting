package com.fmsbeekmans.parting

import com.fmsbeekmans.parting.Flatten._
import shapeless.{Witness, test => _}
import utest._

object FlattenTest extends TestSuite {
  override def tests: Tests =
    Tests {
      test("Flatten String") {
        implicit val prefix = Witness.mkWitness('prefix)

        val actual =
          Flatten[Root / Field[prefix.T], String].apply(prefix = Root/Field[prefix.T] , a = "value")
        val expected =
          List(FlatKeyValue(flatKey = Root/Field[prefix.T], value = "value"))

        assert(actual == expected)
      }

      test("Flatten Generic") {
        case class Nested(s: String)
        println(Flatten[Root, Nested].apply(Root, Nested("aoeu")))


//        val actual =
//          Flatten[Nested].apply(
//            prefix = List("prefix"),
//            a = Nested("B", "C")
//          )
//        val expected =
//          List(
//            FlatKeyValue(flatKey = List("prefix", "first").reverse, value = "A"),
//            FlatKeyValue(flatKey = List("prefix", "second", "a").reverse, value = "B"),
//            FlatKeyValue(flatKey = List("prefix", "second", "b").reverse, value = "C"),
//          )
//
//        assert(actual == expected)
      }

//      test("Flatten Nested") {
//        case class Example(first: String, second: Nested)
//        case class Nested(a: String, b: String)
//
//        val actual =
//          Flatten[Example].apply(
//            prefix = List("prefix"),
//            a = Example("A", Nested("B", "C")))
//        val expected =
//          List(
//            FlatKeyValue(flatKey = List("prefix", "first").reverse, value = "A"),
//            FlatKeyValue(flatKey = List("prefix", "second", "a").reverse, value = "B"),
//            FlatKeyValue(flatKey = List("prefix", "second", "b").reverse, value = "C"),
//          )
//
//        assert(actual == expected)
//      }
    }
}
