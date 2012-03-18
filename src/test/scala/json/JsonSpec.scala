/** Copyright 2011-2012 Mike Dreves
  *
  * All rights reserved. This program and the accompanying materials
  * are made available under the terms of the Eclipse Public License v1.0
  * which accompanies this distribution, and is available at:
  *
  *     http://opensource.org/licenses/eclipse-1.0.php
  * 
  * By using this software in any fashion, you are agreeing to be bound
  * by the terms of this license. You must not remove this notice, or any
  * other, from this software. Unless required by applicable law or agreed 
  * to in writing, software distributed under the License is distributed 
  * on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
  * either express or implied.
  *
  * @author Mike Dreves
  */
package test.scalafy.json

import org.specs2.mutable.Specification

import scalafy.casing._
import scalafy.json._
import scalafy.types._

/** Test specification for json package */
object JsonSpec extends Specification {

  "The fromJson function" should {
    "support extracting JSON passed as quoted Strings" in {
      fromJson[String]("\"foo\"").mustEqual(Some("foo"))
      fromJson[String]("\"foo bar\"").mustEqual(Some("foo bar"))
      fromJson[String]("\"foo \\\"bar\\\"\"").mustEqual(Some("foo \"bar\""))
      fromJson[String]("\"foo \bbar\"").mustEqual(Some("foo \bbar"))
      fromJson[String]("\"foo \fbar\"").mustEqual(Some("foo \fbar"))
      fromJson[String]("\"foo \nbar\"").mustEqual(Some("foo \nbar"))
      fromJson[String]("\"foo \rbar\"").mustEqual(Some("foo \rbar"))
      fromJson[String]("\"foo \tbar\"").mustEqual(Some("foo \tbar"))
      fromJson[String]("\"foo \u0023bar\"").mustEqual(Some("foo \u0023bar"))
      fromJson[String]("\"1234\"").mustEqual(Some("1234"))
      // Failure cases
      fromJson[String]("foo").mustEqual(None) // need " "
      fromJson[Int]("\"foo\"").mustEqual(None)
      fromJson[Long]("\"foo\"").mustEqual(None)
      fromJson[Double]("\"foo\"").mustEqual(None)
      fromJson[Boolean]("\"foo\"").mustEqual(None)
      fromJson[List[String]]("\"foo\"").mustEqual(None)
    }
 
    "support extracting JSON as Shorts" in {
      fromJson[Short]("1234").mustEqual(Some(1234))
      fromJson[Short]("1234").mustEqual(Some(1234))
      fromJson[Short]("-1234").mustEqual(Some(-1234))
      // Failure cases
      fromJson[Short]("2147483647").mustEqual(None)
      fromJson[Short]("-2147483648").mustEqual(None)
    }
   
    "support extracting JSON as Ints" in {
      fromJson[Int]("1234").mustEqual(Some(1234))
      fromJson[Int]("1234").mustEqual(Some(1234))
      fromJson[Int]("-1234").mustEqual(Some(-1234))
      fromJson[Int]("2147483647").mustEqual(Some(2147483647))
      fromJson[Int]("-2147483648").mustEqual(Some(-2147483648))
      // Failure cases
      fromJson[Int]("9223372036854775807").mustEqual(None)
      fromJson[Int]("-9223372036854775808").mustEqual(None)
      fromJson[Boolean]("1234").mustEqual(None)
      fromJson[List[Int]]("1234").mustEqual(None)
   }
    
    "support extracting JSON as Longs" in {
      fromJson[Long]("1234").mustEqual(Some(1234L))
      fromJson[Long]("9223372036854775807")
        .mustEqual(Some(9223372036854775807L))
      fromJson[Long]("-9223372036854775808")
        .mustEqual(Some(-9223372036854775808L))
      // Failure cases
      fromJson[String]("9223372036854775807").mustEqual(None)
    }
 
    "support extracting JSON as Floats" in {
      fromJson[Float]("1.").mustEqual(Some(1.f))
      fromJson[Float]("-1.").mustEqual(Some(-1.f))
      fromJson[Float]("1.0").mustEqual(Some(1.0f))
      fromJson[Float]("-1.0").mustEqual(Some(-1.0f))
      fromJson[Float]("1.79769313486E10")
        .mustEqual(Some(1.79769313486E10f))
      fromJson[Float]("-1.7976931348E10")
        .mustEqual(Some(-1.7976931348E10f))
      fromJson[Float]("1.0e2").mustEqual(Some(1.0e2f))
      fromJson[Float]("-1.0e2").mustEqual(Some(-1.0e2f))
      fromJson[Float]("1.e2").mustEqual(Some(1.e2f))
      fromJson[Float]("-1.e2").mustEqual(Some(-1.e2f))
      fromJson[Float]("1.e+2").mustEqual(Some(1.e+2f))
      fromJson[Float]("-1.e+2").mustEqual(Some(-1.e+2f))
      fromJson[Float]("1.e-2").mustEqual(Some(1.e-2f))
      fromJson[Float]("-1.e-2").mustEqual(Some(-1.e-2f))
      fromJson[Float]("1.0E2").mustEqual(Some(1.0E2f))
      fromJson[Float]("-1.0E2").mustEqual(Some(-1.0E2f))
      fromJson[Float]("1.E2").mustEqual(Some(1.E2f))
      fromJson[Float]("-1.E2").mustEqual(Some(-1.E2f))
      fromJson[Float]("1.E+2").mustEqual(Some(1.E+2f))
      fromJson[Float]("-1.E+2").mustEqual(Some(-1.E+2f))
      fromJson[Float]("1.E-2").mustEqual(Some(1.E-2f))
      fromJson[Float]("-1.E-2").mustEqual(Some(-1.E-2f))
      fromJson[Float]("1234").mustEqual(Some(1234.0f))
      // Failure cases
      fromJson[String]("1.0").mustEqual(None)
    }
  
    "support extracting JSON as Doubles" in {
      fromJson[Double]("1.").mustEqual(Some(1.))
      fromJson[Double]("-1.").mustEqual(Some(-1.))
      fromJson[Double]("1.0").mustEqual(Some(1.0))
      fromJson[Double]("-1.0").mustEqual(Some(-1.0))
      fromJson[Double]("1.7976931348623157E308")
        .mustEqual(Some(1.7976931348623157E308))
      fromJson[Double]("-1.7976931348623157E308")
        .mustEqual(Some(-1.7976931348623157E308))
      fromJson[Double]("1.0e2").mustEqual(Some(1.0e2))
      fromJson[Double]("-1.0e2").mustEqual(Some(-1.0e2))
      fromJson[Double]("1.e2").mustEqual(Some(1.e2))
      fromJson[Double]("-1.e2").mustEqual(Some(-1.e2))
      fromJson[Double]("1.e+2").mustEqual(Some(1.e+2))
      fromJson[Double]("-1.e+2").mustEqual(Some(-1.e+2))
      fromJson[Double]("1.e-2").mustEqual(Some(1.e-2))
      fromJson[Double]("-1.e-2").mustEqual(Some(-1.e-2))
      fromJson[Double]("1.0E2").mustEqual(Some(1.0E2))
      fromJson[Double]("-1.0E2").mustEqual(Some(-1.0E2))
      fromJson[Double]("1.E2").mustEqual(Some(1.E2))
      fromJson[Double]("-1.E2").mustEqual(Some(-1.E2))
      fromJson[Double]("1.E+2").mustEqual(Some(1.E+2))
      fromJson[Double]("-1.E+2").mustEqual(Some(-1.E+2))
      fromJson[Double]("1.E-2").mustEqual(Some(1.E-2))
      fromJson[Double]("-1.E-2").mustEqual(Some(-1.E-2))
      fromJson[Double]("1234").mustEqual(Some(1234.0))
      // Failure cases
      fromJson[String]("1.0").mustEqual(None)
    }
    
    "support extracting JSON passed as Booleans" in {
      fromJson[Boolean]("true").mustEqual(Some(true))
      fromJson[Boolean]("false").mustEqual(Some(false))
      // Failure cases
      fromJson[String]("true").mustEqual(None)
    }
 
    "support extracting JSON passed as Chars" in {
      fromJson[Char]("\"a\"").mustEqual(Some('a'))
      fromJson[Char]("\"b\"").mustEqual(Some('b'))
      // Failure cases
      fromJson[Char]("\"ab\"").mustEqual(None)
    }
   
    "support extracting JSON passed as Bytes" in {
      fromJson[Byte]("123").mustEqual(Some(123))
      fromJson[Byte]("1234").mustEqual(None)
    }

    "support extracting JSON passed as null" in {
      fromJson[Any]("null").mustEqual(Some(null))
      fromJson[String]("null").mustEqual(Some(null))
      fromJson[List[String]]("null").mustEqual(Some(null))
      fromJson[Map[String,String]]("null").mustEqual(Some(null))
      // Failure cases
      fromJson[Int]("null").mustEqual(None)
      fromJson[Long]("null").mustEqual(None)
      fromJson[Double]("null").mustEqual(None)
      fromJson[Boolean]("null").mustEqual(None)
    }

    "support extracting JSON passed as List of primitives" in {
      fromJson[List[Int]]("[1, 2, 3]").mustEqual(Some(List(1,2,3)))
      fromJson[List[Short]]("[1, 2, 3]").mustEqual(Some(List[Short](1,2,3)))
      fromJson[List[Long]]("[2147483648, 2147483649]")
        .mustEqual(Some(List(2147483648L, 2147483649L)))
      fromJson[List[Double]]("[1.0, 2.0, 3.0]")
        .mustEqual(Some(List(1.0, 2.0, 3.0)))
      fromJson[List[Float]]("[1.0, 2.0, 3.0]")
        .mustEqual(Some(List(1.0f, 2.0f, 3.0f)))
      fromJson[List[Boolean]]("[true, false, true]")
        .mustEqual(Some(List(true, false, true)))
      fromJson[List[String]]("[\"foo\", \"bar\"]")
        .mustEqual(Some(List("foo", "bar")))
      fromJson[List[Char]]("[\"a\", \"b\"]")
        .mustEqual(Some(List('a','b')))
      // Failure cases
      fromJson[List[Int]]("[\"foo\", \"bar\"]")
        .mustEqual(None)
    }
    
    "support extracting JSON passed as Map of primitives" in {
      fromJson[Map[String,Int]]("{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(Map("name1" -> 1, "name2" -> 2)))
      fromJson[Map[String,Short]]("{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(Map[String,Short]("name1" -> 1, "name2" -> 2)))
      fromJson[Map[String,Long]](
          "{\"name1\": 2147483648, \"name2\": 2147483649}")
        .mustEqual(
          Some(Map("name1" -> 2147483648L, "name2" -> 2147483649L)))
      fromJson[Map[String,Double]]("{\"name1\": 1.0, \"name2\": 2.0}")
        .mustEqual(
          Some(Map("name1" -> 1.0, "name2" -> 2.0)))
      fromJson[Map[String,Float]]("{\"name1\": 1.0, \"name2\": 2.0}")
        .mustEqual(
          Some(Map("name1" -> 1.0f, "name2" -> 2.0f)))
      fromJson[Map[String,Boolean]]("{\"name1\": true, \"name2\": false}")
        .mustEqual(
          Some(Map("name1" -> true, "name2" -> false)))
      fromJson[Map[String,String]]("{\"name1\": \"foo\", \"name2\": \"bar\"}")
        .mustEqual(
          Some(Map("name1" -> "foo", "name2" -> "bar")))
      fromJson[Map[String,Char]]("{\"name1\": \"a\", \"name2\": \"b\"}")
        .mustEqual(
          Some(Map("name1" -> 'a', "name2" -> 'b')))
      // Failure cases
      fromJson[Map[String,Int]]("{\"name1\": \"foo\", \"name2\": \"bar\"}")
        .mustEqual(None)
    }

    "support extracting JSON passed as Lists of Lists" in {
      fromJson[List[Any]]("[[\"foo\", \"bar\"], [1, 2]]")
        .mustEqual(
          Some(List(List("foo", "bar"), List(1,2))))
      fromJson[List[Any]]("[[\"foo\", \"bar\"], [1, [1.0, 2.0], 2], null]")
        .mustEqual(Some(
          List(List("foo", "bar"), 
          List(1,List(1.0,2.0),2), null)))
      // Failure cases
      fromJson[List[Int]]("[[\"foo\", \"bar\"], [1, 2]]")
        .mustEqual(None)
    }
    
    "support extracting JSON passed as Lists of Maps" in {
      fromJson[List[Map[String,String]]](
          "[{\"name1\": \"foo\", \"name2\": \"bar\"}, {\"name1\": \"foo2\", \"name2\": \"bar2\"}]")
        .mustEqual(Some(
          List(Map("name1" -> "foo", "name2" -> "bar"),
            Map("name1" -> "foo2", "name2" -> "bar2"))))
      // Failure cases
      fromJson[List[Map[String,Int]]](
          "[{\"name1\": \"foo\", \"name2\": \"bar\"}, {\"name1\": \"foo2\", \"name2\": \"bar2\"}]")
        .mustEqual(None)
    }

    "support extracting JSON passed as Map of Lists" in {
      fromJson[Map[String,List[String]]](
          "{\"name1\": [\"foo\", \"bar\"], \"name2\": [\"bar2\"]}")
        .mustEqual(Some(
          Map("name1" -> List("foo","bar"), 
            "name2" -> List("bar2"))))
      // Failure cases
      fromJson[Map[String,List[Int]]](
          "{\"name1\": [\"foo\", \"bar\"], \"name2\": [\"bar2\"]}")
        .mustEqual(None)
    }

    "support extracting JSON passed as Map of Maps" in {
      fromJson[Map[String, Map[String,String]]](
          "{\"name1\": {\"name3\": \"bar\"}, \"name2\": {\"name4\": null}}")
        .mustEqual(Some(
          Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> null))))
      // Failure cases
      fromJson[Map[String, Map[String,Int]]](
          "{\"name1\": {\"name3\": \"bar\"}, \"name2\": {\"name4\": null}}")
        .mustEqual(None)
    }

    "support extracting JSON passed with Any type" in {
      fromJson[List[Any]]("[1,2,3]").mustEqual(Some(List(1,2,3)))
      fromJson[Map[String,Map[String,Any]]](
          "{\"name1\": {\"name3\": \"bar\"}, \"name2\": {\"name4\": null}}")
        .mustEqual(Some(
          Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> null))))
    }

    "support extracting JSON passed with Map of non-Symbol primitive keys" in {
      fromJson[Map[Symbol,String]]("{\"name1\": \"foo\", \"name2\": \"bar\"}")
        .mustEqual(
          Some(Map('name1 -> "foo", 'name2 -> "bar")))
      fromJson[Map[String,String]]("{\"name1\": \"foo\", \"name2\": \"bar\"}")
        .mustEqual(
          Some(Map("name1" -> "foo", "name2" -> "bar")))
      fromJson[Map[Int,String]]("{\"1\": \"foo\", \"2\": \"bar\"}")
        .mustEqual(Some(Map(1 -> "foo", 2 -> "bar")))
      fromJson[Map[Short,String]]("{\"1\": \"foo\", \"2\": \"bar\"}")
        .mustEqual(Some(Map(1.toShort -> "foo", 2.toShort -> "bar")))
      fromJson[Map[Long,String]]("{\"1\": \"foo\", \"2\": \"bar\"}")
        .mustEqual(Some(Map(1L -> "foo", 2L -> "bar")))
      fromJson[Map[Byte,String]]("{\"1\": \"foo\", \"2\": \"bar\"}")
        .mustEqual(Some(Map(1.toByte -> "foo", 2.toByte -> "bar")))
      fromJson[Map[Float,String]]("{\"1.0\": \"foo\", \"2.0\": \"bar\"}")
        .mustEqual(Some(Map(1.0f -> "foo", 2.0f -> "bar")))
      fromJson[Map[Double,String]]("{\"1.0\": \"foo\", \"2.0\": \"bar\"}")
        .mustEqual(Some(Map(1.0 -> "foo", 2.0 -> "bar")))
      fromJson[Map[Boolean,String]]("{\"true\": \"foo\", \"false\": \"bar\"}")
        .mustEqual(Some(Map(true -> "foo", false -> "bar")))
      fromJson[Map[Char,String]]("{\"a\": \"foo\", \"b\": \"bar\"}")
        .mustEqual(Some(Map('a' -> "foo", 'b' -> "bar")))
      fromJson[Map[Int,Int]]("{\"1\": 3, \"2\": 4}")
        .mustEqual(Some(Map(1 -> 3, 2 -> 4)))
    }

    "support extracting JSON passed with Any type using Reifiable" in {
      implicit val reifiableSettings = ReifiableSettings(true)

      (fromJson[Any]("[1,2,3]") match {
        case Some(xs) if (xs.isTypeOf[List[Int]]) => "match"
        case _ => "no match"
      }).mustEqual("match")
      (fromJson[Any]("{\"name1\": 2}") match {
        case Some(xm) if (xm.isTypeOf[Map[Symbol, Int]]) => "match"
        case _ => "no match"
      }).mustEqual("match")
      // Failure cases 
      (fromJson[Any]("{\"name1\": 2}") match {
        case Some(xm) if (xm.isTypeOf[Map[Symbol, String]]) => "match"
        case _ => "no match"
      }).mustEqual("no match")
    }

    "support extracting JSON using reflection" in {
      fromJson[Test1]("{\"s\": \"foo\"}").mustEqual(Some(Test1("foo")))
      fromJson[Test2](
          "{\"str\": \"bar\", \"num\": 1, \"sht\": 2, \"lng\": 3, \"flt\": 1.0, \"dbl\": 2.0, \"ch\": \"a\", \"bool\": true, \"byt\": 3}")
        .mustEqual(
          Some(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))
    }

    "support extracting JSON using List of objects from reflection" in {
      fromJson[List[Test1]]("[{\"s\": \"foo\"}, {\"s\": \"bar\"}]")
        .mustEqual(Some(List(Test1("foo"), Test1("bar"))))
    }

    "support extracting JSON using Maps of objects from reflection" in {
      fromJson[Map[Int,Test1]](
          "{\"10\": {\"s\": \"foo\"}, \"15\": {\"s\": \"bar\"}}")
        .mustEqual(Some(Map(10 -> Test1("foo"), 15 -> Test1("bar"))))
    }

    "support extracting JSON using reflection with embedded List types" in {
      // Only Lists of type String, Int (Long), Double, and Boolean possible
      fromJson[Test3]("{\"xs\": [\"foo\", \"bar\"]}").mustEqual(
        Some(Test3(List("foo", "bar"))))
      fromJson[Test4]("{\"xs1\": [\"foo\", \"bar\"], \"xs2\": [1, 2]}")
        .mustEqual(Some(Test4(List("foo", "bar"), List(1, 2))))
      // As expected, won't work due to type erasure... (returns Map not Test1)
      //fromJson[Test5]("{\"xs\": [{\"s\": \"foo\"},{\"s\": \"bar\"}]}")
      //  .mustEqual(Some(Test5(List(Test1("foo"),Test1("bar")))))
    }

    "support extracting JSON using reflection with embedded Map types" in {
      // Only Maps of String to String, Int (Long), Double, and Boolean possible
      fromJson[Test6]("{\"xm\": {\"foo\": \"bar\"}}")
        .mustEqual(Some(Test6(Map("foo" -> "bar"))))
      fromJson[Test7]("{\"xm1\": {\"foo\": \"bar\"}, \"xm2\": {\"bat\": 2}}")
        .mustEqual(Some(Test7(Map("foo" -> "bar"), Map("bat" -> 2))))
    }

    "support extracting JSON using reflection with embedded objects" in {
      fromJson[Test8](
          "{\"t\": {\"s\": \"foo\"}, \"t2\": {\"str\": \"bar\", \"num\": 1, \"sht\": 2, \"lng\": 3, \"flt\": 1.0, \"dbl\": 2.0, \"ch\": \"a\", \"bool\": true, \"byt\": 3}}")
        .mustEqual(Some(Test8(
            Test1("foo"), 
            Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3))))
    }

    "support extracting JSON using reflection with non-case class types" in {
      fromJson[Test9]("{\"s\": \"foo\", \"i\": 2}")
        .mustEqual(Some(new Test9("foo", 2)))
    }

    "support extracting JSON using reflection with multiple ctors" in {
      fromJson[Test10]("{\"s\": \"foo\"}").mustEqual(Some(new Test10("foo", 1)))
      fromJson[Test10]("{\"i\": 2}").mustEqual(Some(new Test10("foo", 2)))
      // This tests setting a field after construction
      val test11 = new Test11("foo")
      test11.b = true
      fromJson[Test11]("{\"s\": \"foo\", \"b\": true}").mustEqual(Some(test11))
    }

    "support lazy conversion to JSON using iterable/iterator" in {
      val iterable = fromJson[Iterable[Int]]("[1, 2, 3]").get
      val iterator1 = iterable.iterator
      var data1 = List[Int]()
      while (iterator1.hasNext) data1 :+= iterator1.next()
      data1.mustEqual(List(1,2,3))
      
      val iterator2 = fromJson[Iterator[Int]]("[1, 2, 3]").get 
      var data2 = List[Int]()
      while (iterator2.hasNext) data2 :+= iterator2.next()
      data2.mustEqual(List(1,2,3))

      val iterator3 = fromJson[Iterator[Test1]](
          "[{\"s\":\"foo\"},{\"s\":\"bar\"}]").get 
      var data3 = List[Test1]()
      while (iterator3.hasNext) data3 :+= iterator3.next()
      data3.mustEqual(List(Test1("foo"),Test1("bar")))
    }

    "support lazy conversion to JSON using Stream" in {
      fromJson[collection.immutable.Stream[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.Stream(1,2,3)))
    }

    "support conversion to JSON using other sequence types" in {
      fromJson[::[Int]]("[1, 2, 3]").mustEqual(Some(1 :: 2 :: 3 :: Nil))
      fromJson[Vector[Int]]("[1, 2, 3]").mustEqual(Some(Vector(1,2,3)))
      fromJson[Seq[Int]]("[1, 2, 3]").mustEqual(Some(Seq(1,2,3)))
      fromJson[IndexedSeq[Int]]("[1, 2, 3]").mustEqual(Some(IndexedSeq(1,2,3)))
      fromJson[collection.immutable.LinearSeq[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.LinearSeq(1,2,3)))
      fromJson[collection.immutable.HashSet[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.HashSet(1,2,3)))
      fromJson[collection.immutable.SortedSet[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.SortedSet(1,2,3)))
      fromJson[collection.immutable.TreeSet[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.TreeSet(1,2,3)))
      fromJson[collection.immutable.ListSet[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.ListSet(1,2,3)))
      fromJson[Set[Int]]("[1, 2, 3]").mustEqual(Some(Set(1,2,3)))
      fromJson[collection.immutable.Stack[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.Stack(1,2,3)))
      fromJson[collection.immutable.Queue[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.immutable.Queue(1,2,3)))
      fromJson[collection.mutable.ListBuffer[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.ListBuffer(1,2,3)))
      // Priority queue's don't compare equal....
      fromJson[collection.mutable.PriorityQueue[Int]]("[1, 2, 3]").get.toList
        .mustEqual(collection.mutable.PriorityQueue(1,2,3).toList)
      fromJson[collection.mutable.Queue[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.Queue(1,2,3)))
      fromJson[collection.mutable.HashSet[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.HashSet(1,2,3)))
      fromJson[collection.mutable.LinkedHashSet[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.Set(1,2,3)))
      fromJson[collection.mutable.Set[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.Set(1,2,3)))
      fromJson[collection.mutable.ArrayBuffer[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.ArrayBuffer(1,2,3)))
      fromJson[collection.mutable.ResizableArray[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.ResizableArray(1,2,3)))
      // NOTE: Bug in scala, third item is null
      fromJson[collection.mutable.ArrayStack[Int]]("[1, 2]")
        .mustEqual(Some(collection.mutable.ArrayStack(1,2)))
      fromJson[collection.mutable.Stack[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.Stack(1,2,3)))
      fromJson[collection.mutable.LinkedList[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.LinkedList(1,2,3)))
      fromJson[collection.mutable.DoubleLinkedList[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.DoubleLinkedList(1,2,3)))
      fromJson[collection.mutable.MutableList[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.MutableList(1,2,3)))
      fromJson[collection.mutable.ArraySeq[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.ArraySeq(1,2,3)))
      fromJson[collection.mutable.IndexedSeq[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.IndexedSeq(1,2,3)))
      fromJson[collection.mutable.LinearSeq[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.LinearSeq(1,2,3)))
      fromJson[collection.mutable.Seq[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.Seq(1,2,3)))
      fromJson[collection.mutable.UnrolledBuffer[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.UnrolledBuffer(1,2,3)))
      fromJson[collection.mutable.Buffer[Int]]("[1, 2, 3]")
        .mustEqual(Some(collection.mutable.Buffer(1,2,3)))
    }

    "support conversion to JSON using other Map types" in {
      fromJson[collection.immutable.HashMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.immutable.HashMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.immutable.TreeMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.immutable.TreeMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.immutable.SortedMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.immutable.SortedMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.immutable.ListMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.immutable.ListMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.mutable.Map[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.mutable.Map(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.mutable.HashMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.mutable.HashMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.mutable.WeakHashMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.mutable.WeakHashMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.mutable.LinkedHashMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.mutable.LinkedHashMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.mutable.OpenHashMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.mutable.OpenHashMap(
          "name1" -> 1, "name2" -> 2)))
      fromJson[collection.mutable.ListMap[String,Int]](
          "{\"name1\": 1, \"name2\": 2}")
        .mustEqual(Some(collection.mutable.ListMap(
          "name1" -> 1, "name2" -> 2)))
    }

    "support conversion of field casing while extracting JSON" in {
      // Default is IgnoreCasing
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"NamePart3\": null}")
        .mustEqual(Some(Map(
          'NameOne -> Map('NamePartTwo -> 2),
          'NamePart3 -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"NamePart3\": null}",
        LowerCamelCase).mustEqual(Some(Map(
          'nameOne -> Map('namePartTwo -> 2),
          'namePart3 -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        UpperCamelCase).mustEqual(Some(Map(
          'NameOne -> Map('NamePartTwo -> 2), 
          'NamePart3 -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        LowerSnakeCase).mustEqual(Some(Map(
          'name_one -> Map('name_part_two -> 2), 
          'name_part_3 -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        UpperSnakeCase).mustEqual(Some(Map(
          'NAME_ONE -> Map('NAME_PART_TWO -> 2), 
          'NAME_PART_3 -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        LowerCase).mustEqual(Some(Map(
          'nameone -> Map('nameparttwo -> 2), 
          'namepart3 -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        UpperCase).mustEqual(Some(Map(
          'NAMEONE -> Map('NAMEPARTTWO -> 2), 
          'NAMEPART3 -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        LowerDashCase).mustEqual(Some(Map(
          Symbol("name-one") -> Map(Symbol("name-part-two") -> 2), 
          Symbol("name-part-3") -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        UpperDashCase).mustEqual(Some(Map(
          Symbol("Name-One") -> Map(Symbol("Name-Part-Two") -> 2), 
          Symbol("Name-Part-3") -> null)))
      fromJson[Any]("{\"NameOne\": {\"NamePartTwo\": 2}, \"namePart3\": null}",
        IgnoreCasing).mustEqual(Some(Map(
          'NameOne -> Map('NamePartTwo -> 2), 
          'namePart3 -> null)))
    }
  }

  "The jsonIterator function" should {
    "support iteration for any type" in {
      val iterator = jsonIterator[Int]("[1,2,3]").map(_ + 1)
      var data = List[Int]()
      while (iterator.hasNext) data :+= iterator.next()
      data.mustEqual(List(2,3,4))
    }
  }

  "The toJson function" should {
    "support conversion from primitives" in {
      toJson("foo bar").mustEqual("\"foo bar\"")
      toJson(3).mustEqual("3")
      toJson(3: Short).mustEqual("3")
      toJson(3L).mustEqual("3")
      toJson(1.0f).mustEqual("1.0")
      toJson(1.0).mustEqual("1.0")
      toJson(true).mustEqual("true")
      toJson(false).mustEqual("false")
      toJson('a').mustEqual("\"a\"")
      toJson(3: Byte).mustEqual("3")
    }

    "support conversion from Lists" in {
      toJson(List("foo", "bar")).mustEqual("[\"foo\",\"bar\"]")
      toJson(List(1,2,3)).mustEqual("[1,2,3]")
    }

    "support conversion from Maps" in {
      toJson(Map("foo" -> "bar")).mustEqual("{\"foo\":\"bar\"}")
      toJson(Map("foo" -> 3)).mustEqual("{\"foo\":3}")
      toJson(Map("foo" -> List(1,2,3))).mustEqual("{\"foo\":[1,2,3]}")
      toJson(Map("foo" -> Map("bar" -> 3))).mustEqual("{\"foo\":{\"bar\":3}}")
    }

   "support conversion using reflection" in {
      toJson(Test1("foo")).mustEqual("{\"s\":\"foo\"}")
      toJson(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)).mustEqual(
          "{\"str\":\"bar\",\"num\":1,\"sht\":2,\"lng\":3,\"flt\":1.0,\"dbl\":2.0,\"ch\":\"a\",\"bool\":true,\"byt\":3}")
    }

    "support conversion using List of objects from reflection" in {
      toJson(List(Test1("foo"), Test1("bar"))).mustEqual(
        "[{\"s\":\"foo\"},{\"s\":\"bar\"}]")
    }

    "support conversion using Maps of objects from reflection" in {
      toJson(Map(10 -> Test1("foo"), 15 -> Test1("bar"))).mustEqual(
          "{\"10\":{\"s\":\"foo\"},\"15\":{\"s\":\"bar\"}}")
    }

    "support conversion using reflection with embedded List types" in {
      // Only Lists of type String, Int (Long), Double, and Boolean possible
      toJson(Test3(List("foo", "bar"))).mustEqual("{\"xs\":[\"foo\",\"bar\"]}")
      toJson(Test4(List("foo", "bar"), List(1, 2))).mustEqual(
          "{\"xs1\":[\"foo\",\"bar\"],\"xs2\":[1,2]}")
      // This only works in 'to' direction, not reverse
      toJson(Test5(List(Test1("foo"),Test1("bar")))).mustEqual(
          "{\"xs\":[{\"s\":\"foo\"},{\"s\":\"bar\"}]}")
    }

    "support conversion using reflection with embedded Map types" in {
      // Only Maps of String to String, Int (Long), Double, and Boolean possible
      toJson(Test6(Map("foo" -> "bar"))).mustEqual(
          "{\"xm\":{\"foo\":\"bar\"}}")
      toJson(Test7(Map("foo" -> "bar"), Map("bat" -> 2))).mustEqual(
          "{\"xm1\":{\"foo\":\"bar\"},\"xm2\":{\"bat\":2}}")
    }

    "support conversion using reflection with embedded objects" in {
      toJson(Test8(
          Test1("foo"), 
          Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))
        .mustEqual(
          "{\"t\":{\"s\":\"foo\"},\"t2\":{\"str\":\"bar\",\"num\":1,\"sht\":2,\"lng\":3,\"flt\":1.0,\"dbl\":2.0,\"ch\":\"a\",\"bool\":true,\"byt\":3}}")
    }

    "support conversion using reflection with non-case class types" in {
      toJson(new Test9("foo", 2)).mustEqual("{\"s\":\"foo\",\"i\":2}")
    }

    "support conversion using reflection with readonly params" in {
      toJson(new Test10("foo", 2)).mustEqual("{\"s\":\"foo\",\"i\":2}")
      // This tests setting a field after construction
      val test11 = new Test11("foo")
      test11.b = true
      toJson(test11).mustEqual("{\"s\":\"foo\",\"l\":3,\"b\":true}")
    }

    "support conversion using Streams" in {
      toJson(collection.immutable.Stream(1,2,3)).mustEqual("[1,2,3]")
    }

    "support conversion using any iterable/iterator type" in {
      val iterable: Iterable[_] = List(1,2,3)
      toJson(iterable).mustEqual("[1,2,3]")
      val iterator: Iterator[_] = List(1,2,3).iterator
      toJson(iterator).mustEqual("[1,2,3]")
    }

    "support conversion using other seq types" in {
      // May need to revisit these, order is not always guarnateed
      toJson(Vector(1,2,3)).mustEqual("[1,2,3]")
      toJson(Seq(1,2,3)).mustEqual("[1,2,3]")
      toJson(IndexedSeq(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.immutable.LinearSeq(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.immutable.HashSet(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.immutable.SortedSet(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.immutable.TreeSet(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.immutable.ListSet(1,2,3)).mustEqual("[3,2,1]")
      toJson(Set(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.immutable.Stack(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.immutable.Queue(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.ListBuffer(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.PriorityQueue(1,2,3)).mustEqual("[3,1,2]")
      toJson(collection.mutable.Queue(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.HashSet(1,2,3)).mustEqual("[2,1,3]")
      toJson(collection.mutable.LinkedHashSet(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.Set(1,2,3)).mustEqual("[2,1,3]")
      toJson(collection.mutable.ArrayBuffer(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.ResizableArray(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.ArrayStack(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.Stack(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.LinkedList(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.DoubleLinkedList(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.MutableList(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.ArraySeq(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.IndexedSeq(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.LinearSeq(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.Seq(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.UnrolledBuffer(1,2,3)).mustEqual("[1,2,3]")
      toJson(collection.mutable.Buffer(1,2,3)).mustEqual("[1,2,3]")
    }

    "support conversion using other map types" in {
      toJson(collection.immutable.HashMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name2\":2,\"name1\":1}")
      toJson(collection.immutable.TreeMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name1\":1,\"name2\":2}")
      toJson(collection.immutable.SortedMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name1\":1,\"name2\":2}")
      toJson(collection.immutable.ListMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name1\":1,\"name2\":2}")
      toJson(collection.mutable.Map("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name2\":2,\"name1\":1}")
      toJson(collection.mutable.HashMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name2\":2,\"name1\":1}")
      toJson(collection.mutable.WeakHashMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name2\":2,\"name1\":1}")
      toJson(collection.mutable.LinkedHashMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name1\":1,\"name2\":2}")
      toJson(collection.mutable.OpenHashMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name1\":1,\"name2\":2}")
      toJson(collection.mutable.ListMap("name1" -> 1, "name2" -> 2))
        .mustEqual("{\"name1\":1,\"name2\":2}")
    }

    "support field casing conversion" in {
      toJson(Map("SomeName" -> 1), LowerCamelCase).mustEqual(
        "{\"someName\":1}")
      toJson(Map("SomeName" -> 1), LowerSnakeCase).mustEqual(
        "{\"some_name\":1}")
    }

    "support pretty printing" in {
      toJson(Map("SomeName" -> 1), true).mustEqual(
        "{\n  \"SomeName\": 1\n}")
      toJson(Map("foo" -> Map("bar" -> 3)), true).mustEqual(
        "{\n  \"foo\": {\n    \"bar\": 3\n  }\n}")
      toJson(Map("foo" -> List(1,2,3)), true).mustEqual(
        "{\n  \"foo\": [1, 2, 3]\n}")
      toJson(Map("foo" -> Map("bar" -> List(1,2,3))), true).mustEqual(
        "{\n  \"foo\": {\n    \"bar\": [1, 2, 3]\n  }\n}")
      toJson(List(Map("foo" -> 1), Map("bar" -> 2)), true).mustEqual(
        "[{\n  \"foo\": 1\n}, {\n  \"bar\": 2\n}]") }
  }

  "The Json object" should {
    "support extraction operations" in {
      ("\"foo\"" match {
        case Json(s) => s 
        case _ => "no match"
      }).mustEqual("foo")

      ("{\"foo\": 1}" match {
        case Json(xm) => xm 
        case _ => "no match"
      }).mustEqual(Map('foo -> 1))
    }
  }

  "The json package" should {
    "support pimping any type to support using withJson" in {
      "foo".withJson("\"bar\"") { _ + _ }.mustEqual("foobar")
      1.withJson("2") { _ + _ }.mustEqual(3)
      1L.withJson("9223372036854775806") { _ + _ }
        .mustEqual(9223372036854775807L)
      1.0.withJson("2.0") { _ + _ }.mustEqual(3.0)
      true.withJson("false") { _ || _ }.mustEqual(true)
      true.withJson("false") { _ && _ }.mustEqual(false)
      List(1,2).withJson("[3,4]") { _ ++ _ }.mustEqual(List(1,2,3,4))
      Map("foo" -> "bar").withJson("{\"foo2\": \"bar2\"}") { _ ++ _ }
        .mustEqual(Map("foo" -> "bar", "foo2" -> "bar2"))
    }
  }

  "The fromJson function" should {
    "support examples from json.org" in {
      fromJson[Any](jsonOrgExample1, IgnoreCasing).mustEqual(
        Some(Map('glossary -> Map( 
          'title -> "example glossary",
          'GlossDiv -> Map(
            'title -> "S",
            'GlossList -> Map(
              'GlossEntry -> Map(
                'ID -> "SGML",
                'SortAs -> "SGML",
                'GlossTerm -> "Standard Generalized Markup Language",
                'Acronym -> "SGML",
                'Abbrev -> "ISO 8879:1986",
                'GlossDef -> Map(
                  'para -> "A meta-markup language, used to create markup languages such as DocBook.",
                  'GlossSeeAlso -> List("GML", "XML")),
                'GlossSee -> "markup")))))))
        
      fromJson[Any](jsonOrgExample2, IgnoreCasing).mustEqual(
        Some(Map('menu -> Map(
          'id -> "file",
          'value -> "File",
          'popup -> Map(
            'menuitem -> List(
              Map('value -> "New", 'onclick -> "CreateNewDoc()"),
              Map('value -> "Open", 'onclick -> "OpenDoc()"),
              Map('value -> "Close", 'onclick -> "CloseDoc()")))))))
      
      fromJson[Any](jsonOrgExample3, IgnoreCasing).mustEqual(
        Some(Map('widget -> Map(
          'debug -> "on",
          'window -> Map(
            'title -> "Sample Konfabulator Widget",
            'name -> "main_window",
            'width -> 500,
            'height -> 500),
          'image -> Map(
            'src -> "Images/Sun.png",
            'name -> "sun1",
            'hOffset -> 250,
            'vOffset -> 250,
            'alignment -> "center"),
          'text -> Map(
            'data -> "Click Here",
            'size -> 36,
            'style -> "bold",
            'name -> "text1",
            'hOffset -> 250,
            'vOffset -> 100,
            'alignment -> "center",
            'onMouseUp -> "sun1.opacity = (sun1.opacity / 100) * 90;") ))) )
      
      fromJson[Any](jsonOrgExample4, IgnoreCasing).mustEqual(
        Some(Map(Symbol("web-app") -> Map(
          'servlet -> List(
            Map(
              Symbol("servlet-name") -> "cofaxCDS",
              Symbol("servlet-class") -> "org.cofax.cds.CDSServlet",
              Symbol("init-param") -> Map(
                Symbol("configGlossary:installationAt") -> "Philadelphia, PA",
                Symbol("configGlossary:adminEmail") -> "ksm@pobox.com",
                Symbol("configGlossary:poweredBy") -> "Cofax",
                Symbol("configGlossary:poweredByIcon") -> "/images/cofax.gif",
                Symbol("configGlossary:staticPath") -> "/content/static",
                'templateProcessorClass -> "org.cofax.WysiwygTemplate",
                'templateLoaderClass -> "org.cofax.FilesTemplateLoader",
                'templatePath -> "templates",
                'templateOverridePath -> "",
                'defaultListTemplate -> "listTemplate.htm",
                'defaultFileTemplate -> "articleTemplate.htm",
                'useJSP -> false,
                'jspListTemplate -> "listTemplate.jsp",
                'jspFileTemplate -> "articleTemplate.jsp",
                'cachePackageTagsTrack -> 200,
                'cachePackageTagsStore -> 200,
                'cachePackageTagsRefresh -> 60,
                'cacheTemplatesTrack -> 100,
                'cacheTemplatesStore -> 50,
                'cacheTemplatesRefresh -> 15,
                'cachePagesTrack -> 200,
                'cachePagesStore -> 100,
                'cachePagesRefresh -> 10,
                'cachePagesDirtyRead -> 10,
                'searchEngineListTemplate -> "forSearchEnginesList.htm",
                'searchEngineFileTemplate -> "forSearchEngines.htm",
                'searchEngineRobotsDb -> "WEB-INF/robots.db",
                'useDataStore -> true,
                'dataStoreClass -> "org.cofax.SqlDataStore",
                'redirectionClass -> "org.cofax.SqlRedirection",
                'dataStoreName -> "cofax",
                'dataStoreDriver -> "com.microsoft.jdbc.sqlserver.SQLServerDriver",
                'dataStoreUrl -> "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
                'dataStoreUser -> "sa",
                'dataStorePassword -> "dataStoreTestQuery",
                'dataStoreTestQuery -> "SET NOCOUNT ON;select test='test';",
                'dataStoreLogFile -> "/usr/local/tomcat/logs/datastore.log",
                'dataStoreInitConns -> 10,
                'dataStoreMaxConns -> 100,
                'dataStoreConnUsageLimit -> 100,
                'dataStoreLogLevel -> "debug",
                'maxUrlLength -> 500)),
            Map( 
              Symbol("servlet-name") -> "cofaxEmail",
              Symbol("servlet-class") -> "org.cofax.cds.EmailServlet",
              Symbol("init-param") -> Map( 
                'mailHost -> "mail1",
                'mailHostOverride -> "mail2")),
            Map( 
              Symbol("servlet-name") -> "cofaxAdmin",
              Symbol("servlet-class") -> "org.cofax.cds.AdminServlet"),
            Map( 
              Symbol("servlet-name") -> "fileServlet",
              Symbol("servlet-class") -> "org.cofax.cds.FileServlet"),
            Map( 
              Symbol("servlet-name") -> "cofaxTools",
              Symbol("servlet-class") -> "org.cofax.cms.CofaxToolsServlet",
              Symbol("init-param") -> Map( 
                'templatePath -> "toolstemplates/",
                'log -> 1,
                'logLocation -> "/usr/local/tomcat/logs/CofaxTools.log",
                'logMaxSize -> "",
                'dataLog -> 1,
                'dataLogLocation -> "/usr/local/tomcat/logs/dataLog.log",
                'dataLogMaxSize -> "",
                'removePageCache -> "/content/admin/remove?cache=pages&id=",
                'removeTemplateCache -> "/content/admin/remove?cache=templates&id=",
                'fileTransferFolder -> "/usr/local/tomcat/webapps/content/fileTransferFolder",
                'lookInContext -> 1,
                'adminGroupID -> 4,
                'betaServer -> true))),
          Symbol("servlet-mapping") -> Map( 
            'cofaxCDS -> "/",
            'cofaxEmail -> "/cofaxutil/aemail/*",
            'cofaxAdmin -> "/admin/*",
            'fileServlet -> "/static/*",
            'cofaxTools -> "/tools/*"),
          'taglib -> Map( 
            Symbol("taglib-uri") -> "cofax.tld",
            Symbol("taglib-location") -> "/WEB-INF/tlds/cofax.tld")))))

      fromJson[Any](jsonOrgExample5, IgnoreCasing).mustEqual(
        Some(Map('menu -> Map(
          'header -> "SVG Viewer",
          'items -> List(
              Map('id -> "Open"),
              Map('id -> "OpenNew", 'label -> "Open New"),
              null,
              Map('id -> "ZoomIn", 'label -> "Zoom In"),
              Map('id -> "ZoomOut", 'label -> "Zoom Out"),
              Map('id -> "OriginalView", 'label -> "Original View"),
              null,
              Map('id -> "Quality"),
              Map('id -> "Pause"),
              Map('id -> "Mute"),
              null,
              Map('id -> "Find", 'label -> "Find..."),
              Map('id -> "FindAgain", 'label -> "Find Again"),
              Map('id -> "Copy"),
              Map('id -> "CopyAgain", 'label -> "Copy Again"),
              Map('id -> "CopySVG", 'label -> "Copy SVG"),
              Map('id -> "ViewSVG", 'label -> "View SVG"),
              Map('id -> "ViewSource", 'label -> "View Source"),
              Map('id -> "SaveAs", 'label -> "Save As"),
              null,
              Map('id -> "Help"),
              Map('id -> "About", 'label -> "About Adobe CVG Viewer..."))))))
    }
  }

  private val jsonOrgExample1 = """
{
    "glossary": {
        "title": "example glossary",
		"GlossDiv": {
            "title": "S",
			"GlossList": {
                "GlossEntry": {
                    "ID": "SGML",
					"SortAs": "SGML",
					"GlossTerm": "Standard Generalized Markup Language",
					"Acronym": "SGML",
					"Abbrev": "ISO 8879:1986",
					"GlossDef": {
                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
						"GlossSeeAlso": ["GML", "XML"]
                    },
					"GlossSee": "markup"
                }
            }
        }
    }
}
"""

  private val jsonOrgExample2 = """
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}
"""

  private val jsonOrgExample3 = """
{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": { 
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}  
"""
  
  private val jsonOrgExample4 = """
{"web-app": {
  "servlet": [   
    {
      "servlet-name": "cofaxCDS",
      "servlet-class": "org.cofax.cds.CDSServlet",
      "init-param": {
        "configGlossary:installationAt": "Philadelphia, PA",
        "configGlossary:adminEmail": "ksm@pobox.com",
        "configGlossary:poweredBy": "Cofax",
        "configGlossary:poweredByIcon": "/images/cofax.gif",
        "configGlossary:staticPath": "/content/static",
        "templateProcessorClass": "org.cofax.WysiwygTemplate",
        "templateLoaderClass": "org.cofax.FilesTemplateLoader",
        "templatePath": "templates",
        "templateOverridePath": "",
        "defaultListTemplate": "listTemplate.htm",
        "defaultFileTemplate": "articleTemplate.htm",
        "useJSP": false,
        "jspListTemplate": "listTemplate.jsp",
        "jspFileTemplate": "articleTemplate.jsp",
        "cachePackageTagsTrack": 200,
        "cachePackageTagsStore": 200,
        "cachePackageTagsRefresh": 60,
        "cacheTemplatesTrack": 100,
        "cacheTemplatesStore": 50,
        "cacheTemplatesRefresh": 15,
        "cachePagesTrack": 200,
        "cachePagesStore": 100,
        "cachePagesRefresh": 10,
        "cachePagesDirtyRead": 10,
        "searchEngineListTemplate": "forSearchEnginesList.htm",
        "searchEngineFileTemplate": "forSearchEngines.htm",
        "searchEngineRobotsDb": "WEB-INF/robots.db",
        "useDataStore": true,
        "dataStoreClass": "org.cofax.SqlDataStore",
        "redirectionClass": "org.cofax.SqlRedirection",
        "dataStoreName": "cofax",
        "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
        "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
        "dataStoreUser": "sa",
        "dataStorePassword": "dataStoreTestQuery",
        "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
        "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
        "dataStoreInitConns": 10,
        "dataStoreMaxConns": 100,
        "dataStoreConnUsageLimit": 100,
        "dataStoreLogLevel": "debug",
        "maxUrlLength": 500}},
    {
      "servlet-name": "cofaxEmail",
      "servlet-class": "org.cofax.cds.EmailServlet",
      "init-param": {
      "mailHost": "mail1",
      "mailHostOverride": "mail2"}},
    {
      "servlet-name": "cofaxAdmin",
      "servlet-class": "org.cofax.cds.AdminServlet"},
 
    {
      "servlet-name": "fileServlet",
      "servlet-class": "org.cofax.cds.FileServlet"},
    {
      "servlet-name": "cofaxTools",
      "servlet-class": "org.cofax.cms.CofaxToolsServlet",
      "init-param": {
        "templatePath": "toolstemplates/",
        "log": 1,
        "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
        "logMaxSize": "",
        "dataLog": 1,
        "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
        "dataLogMaxSize": "",
        "removePageCache": "/content/admin/remove?cache=pages&id=",
        "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
        "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
        "lookInContext": 1,
        "adminGroupID": 4,
        "betaServer": true}}],
  "servlet-mapping": {
    "cofaxCDS": "/",
    "cofaxEmail": "/cofaxutil/aemail/*",
    "cofaxAdmin": "/admin/*",
    "fileServlet": "/static/*",
    "cofaxTools": "/tools/*"},
 
  "taglib": {
    "taglib-uri": "cofax.tld",
    "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}
"""
  
  private val jsonOrgExample5 = """
{"menu": {
    "header": "SVG Viewer",
    "items": [
        {"id": "Open"},
        {"id": "OpenNew", "label": "Open New"},
        null,
        {"id": "ZoomIn", "label": "Zoom In"},
        {"id": "ZoomOut", "label": "Zoom Out"},
        {"id": "OriginalView", "label": "Original View"},
        null,
        {"id": "Quality"},
        {"id": "Pause"},
        {"id": "Mute"},
        null,
        {"id": "Find", "label": "Find..."},
        {"id": "FindAgain", "label": "Find Again"},
        {"id": "Copy"},
        {"id": "CopyAgain", "label": "Copy Again"},
        {"id": "CopySVG", "label": "Copy SVG"},
        {"id": "ViewSVG", "label": "View SVG"},
        {"id": "ViewSource", "label": "View Source"},
        {"id": "SaveAs", "label": "Save As"},
        null,
        {"id": "Help"},
        {"id": "About", "label": "About Adobe CVG Viewer..."}
    ]
}}
"""
}

// Test Classes
case class Test1(s: String)
case class Test2(str: String, num: Int, sht: Short, lng: Long, flt: Float, dbl: Double, ch: Char, bool: Boolean, byt: Byte)
case class Test3(xs: List[String])
case class Test4(xs1: List[String], xs2: List[Int])
case class Test5(xs: List[Test1])  // not allowed
case class Test6(xm: Map[String, String])
case class Test7(xm1: Map[String, String], xm2: Map[String, Int])
case class Test8(t: Test1, t2: Test2) 
class Test9(val s: String, var i: Int) {
  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[Test9]) return false
    val o = that.asInstanceOf[Test9]
    o.s == s && o.i == i
  }
}
class Test10(val s: String, var i: Int) {
  def this(s: String) = this(s, 1)
  def this(i: Int) = this("foo", i)

  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[Test10]) return false
    val o = that.asInstanceOf[Test10]
    o.s == s && o.i == i
  }
}
class Test11(val s: String) {
  val l = 3L
  var b = false

  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[Test11]) return false
    val o = that.asInstanceOf[Test11]
    o.s == s && o.b == b
  }
}
