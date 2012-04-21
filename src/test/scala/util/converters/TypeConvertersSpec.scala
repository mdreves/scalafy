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
package test.scalafy.util.converters

import scalafy.types.meta._
import scalafy.util.converters._

import org.specs2.mutable.Specification

/** Test specification for type converters package */
object TypeConvertersSpec extends Specification {

  "The type converters package" should {
    "support primitive type conversion" in {
      to[String]("foo").mustEqual(Some("foo"))
      to[String]('foo).mustEqual(Some("foo"))
      to[String]('f').mustEqual(Some("f"))
      to[String](1.toByte).mustEqual(Some("1"))
      to[String](1.toShort).mustEqual(Some("1"))
      to[String](1).mustEqual(Some("1"))
      to[String](1L).mustEqual(Some("1"))
      to[String](1.0f).mustEqual(Some("1.0"))
      to[String](1.0d).mustEqual(Some("1.0"))
      to[String](true).mustEqual(Some("true"))
      to[String](false).mustEqual(Some("false"))

      to[Symbol]("foo").mustEqual(Some('foo))
      to[Symbol]('foo).mustEqual(Some('foo))
      to[Symbol]('f').mustEqual(Some('f))
      to[Symbol](1.toByte).mustEqual(Some(Symbol("1")))
      to[Symbol](1.toShort).mustEqual(Some(Symbol("1")))
      to[Symbol](1).mustEqual(Some(Symbol("1")))
      to[Symbol](1L).mustEqual(Some(Symbol("1")))
      to[Symbol](1.0f).mustEqual(Some(Symbol("1.0")))
      to[Symbol](1.0d).mustEqual(Some(Symbol("1.0")))
      to[Symbol](true).mustEqual(Some(Symbol("true")))
      to[Symbol](false).mustEqual(Some(Symbol("false")))

      to[Char]("foo").mustEqual(None)
      to[Char]("f").mustEqual(Some('f'))
      to[Char]('f).mustEqual(Some('f'))
      to[Char](1.toByte).mustEqual(Some('1'))
      to[Char](1.toShort).mustEqual(Some('1'))
      to[Char](1).mustEqual(Some('1'))
      to[Char](1L).mustEqual(Some('1'))
      to[Char](1.0f).mustEqual(None)
      to[Char](1.0d).mustEqual(None)
      to[Char](true).mustEqual(Some('T'))
      to[Char](false).mustEqual(Some('F'))

      to[Short]("foo").mustEqual(None)
      to[Short]("1").mustEqual(Some(1))
      to[Short](Symbol("1")).mustEqual(Some(1))
      to[Short]('1').mustEqual(Some(49))
      to[Short](1.toByte).mustEqual(Some(1))
      to[Short](1.toShort).mustEqual(Some(1))
      to[Short](1).mustEqual(Some(1))
      to[Short](9999999991L).mustEqual(None)
      to[Short](1.0f).mustEqual(Some(1))
      to[Short](1.0d).mustEqual(Some(1))
      to[Short](true).mustEqual(Some(1))
      to[Short](false).mustEqual(Some(0)) 

      to[Int]("foo").mustEqual(None)
      to[Int]("1").mustEqual(Some(1))
      to[Int](Symbol("1")).mustEqual(Some(1))
      to[Int]('1').mustEqual(Some(49))
      to[Int](1.toByte).mustEqual(Some(1))
      to[Int](1.toInt).mustEqual(Some(1))
      to[Int](1).mustEqual(Some(1))
      to[Int](1L).mustEqual(Some(1))
      to[Int](9999999991L).mustEqual(None)
      to[Int](1.0f).mustEqual(Some(1))
      to[Int](1.0d).mustEqual(Some(1))
      to[Int](true).mustEqual(Some(1))
      to[Int](false).mustEqual(Some(0)) 

      to[Long]("foo").mustEqual(None)
      to[Long]("1").mustEqual(Some(1))
      to[Long](Symbol("1")).mustEqual(Some(1))
      to[Long]('1').mustEqual(Some(49))
      to[Long](1.toByte).mustEqual(Some(1))
      to[Long](1.toLong).mustEqual(Some(1))
      to[Long](1).mustEqual(Some(1))
      to[Long](1L).mustEqual(Some(1))
      to[Long](1.0f).mustEqual(Some(1))
      to[Long](1.0d).mustEqual(Some(1))
      to[Long](true).mustEqual(Some(1))
      to[Long](false).mustEqual(Some(0)) 

      to[Float]("foo").mustEqual(None)
      to[Float]("1").mustEqual(Some(1.0f))
      to[Float](Symbol("1")).mustEqual(Some(1.0f))
      to[Float]('1').mustEqual(Some(49.0f))
      to[Float](1.toByte).mustEqual(Some(1.0f))
      to[Float](1.toFloat).mustEqual(Some(1.0f))
      to[Float](1).mustEqual(Some(1.0f))
      to[Float](1L).mustEqual(Some(1.0f))
      to[Float](1.0f).mustEqual(Some(1.0f))
      to[Float](1.0d).mustEqual(Some(1.0f))
      to[Float](true).mustEqual(Some(1.0f))
      to[Float](false).mustEqual(Some(0.0f)) 

      to[Double]("foo").mustEqual(None)
      to[Double]("1").mustEqual(Some(1.0f))
      to[Double](Symbol("1")).mustEqual(Some(1.0f))
      to[Double]('1').mustEqual(Some(49.0f))
      to[Double](1.toByte).mustEqual(Some(1.0f))
      to[Double](1.toDouble).mustEqual(Some(1.0f))
      to[Double](1).mustEqual(Some(1.0f))
      to[Double](1L).mustEqual(Some(1.0f))
      to[Double](1.0f).mustEqual(Some(1.0f))
      to[Double](1.0d).mustEqual(Some(1.0f))
      to[Double](true).mustEqual(Some(1.0))
      to[Double](false).mustEqual(Some(0.0)) 

      to[Boolean]("foo").mustEqual(None)
      to[Boolean]("true").mustEqual(Some(true))
      to[Boolean]("F").mustEqual(Some(false))
      to[Boolean]('true).mustEqual(Some(true))
      to[Boolean]('foo).mustEqual(None)
      to[Boolean]('F').mustEqual(Some(false))
      to[Boolean]('a').mustEqual(None)
      to[Boolean](1.toByte).mustEqual(Some(true))
      to[Boolean](1).mustEqual(Some(true))
      to[Boolean](1L).mustEqual(Some(true))
      to[Boolean](1.0f).mustEqual(Some(true))
      to[Boolean](1.0d).mustEqual(Some(true))
      to[Boolean](true).mustEqual(Some(true))
      to[Boolean](false).mustEqual(Some(false)) 
    }

    "support Tuple conversion" in {
      to[Tuple2[String,Int]]('foo -> "1").mustEqual(Some("foo" -> 1))
      to[Tuple2[Symbol,Boolean]]("foo" -> 1).mustEqual(Some('foo -> true))
      to[Tuple2[Char,Double]]("f" -> 1).mustEqual(Some('f' -> 1.0))
      to[Tuple3[String,Int,Boolean]](Tuple3('test,"1",'f')).mustEqual(
        Some(Tuple3("test", 1, false)))
    }

    "support Seq conversion" in {
      // Basic conversion
      to[List[String]](List('foo, 1)).mustEqual(Some(List("foo", "1")))
      to[List[Symbol]](List("foo", 1)).mustEqual(Some(List('foo, Symbol("1"))))
      to[List[Int]](List("1", 1.0)).mustEqual(Some(List(1,1)))
      to[List[Short]](List("1", 1.0)).mustEqual(Some(List(1.toShort,1.toShort)))
      to[List[Byte]](List("1", 1.0)).mustEqual(Some(List(1.toByte,1.toByte)))
      to[List[Long]](List("1", 1.0)).mustEqual(Some(List(1L,1L)))
      to[List[Float]](List("1.0", 1.0)).mustEqual(Some(List(1.0f,1.0f)))
      to[List[Double]](List("2.0", 1.0f)).mustEqual(Some(List(2.0,1.0)))
      to[List[Boolean]](List("true", 1)).mustEqual(Some(List(true,true)))
      to[List[Char]](List("t", 'a)).mustEqual(Some(List('t', 'a')))

      // Using Any to avoid conversion
      to[List[Any]](List("true", 'a', "false", 1))
        .mustEqual(Some(List("true", 'a', "false", 1)))
      
      // Failure cases
      to[List[Int]](List(1, "2.5")).mustEqual(None)
      to[List[Boolean]](List('true, "2.0")).mustEqual(None)

      // Best effort 
      to[List[Int]](List(1, "2.5"), true).mustEqual(Some(List(1)))
      to[List[Boolean]](List('true, "2.0"), true).mustEqual(Some(List(true)))

      // Embedded Lists/Maps/Tuples
      to[List[List[Int]]](List(List("1", "2"))).mustEqual(
        Some(List(List(1,2))))
      to[List[Map[Char,Int]]](List(Map("a" -> "2"))).mustEqual(
        Some(List(Map('a' -> 2))))
      to[List[Tuple2[Char,Int]]](List("a" -> "2", "b" -> 3)).mustEqual(
        Some(List('a' -> 2, 'b' -> 3)))

      // Other types
      to[Vector[Int]](List("1", "2")).mustEqual(Some(Vector(1,2)))
    }

    "support Tuple conversion" in {
      to[Tuple2[String,Int]](List('a, "3")).mustEqual(Some("a" -> 3))
      to[List[Int]](1 -> 2).mustEqual(Some(List(1,2)))

      to[List[Tuple2[String,Int]]](List(List("a", "2"), List("b", 3)))
        .mustEqual(Some(List("a" -> 2, "b" -> 3)))
    }

    "support Map conversion" in {
      // Basic conversion
      to[Map[String, Int]](Map('foo -> "1")).mustEqual(
        Some(Map("foo" -> 1)))
      to[Map[Symbol, Char]](Map("test" -> "a", "test2" -> "b")).mustEqual(
        Some(Map('test -> 'a', 'test2 -> 'b')))
      to[Map[Int, Boolean]](Map(1L -> "t", 3L -> "f")).mustEqual(
        Some(Map(1 -> true, 3 -> false)))
      to[Map[Short, Byte]](Map(1 -> "1", "3" -> "2")).mustEqual(
        Some(Map((1.toShort) -> (1.toByte), (3.toShort) -> (2.toByte))))
      to[Map[String, Long]](Map('a' -> "1", 'b' -> "99999")).mustEqual(
        Some(Map("a" -> 1L, "b" -> 99999L)))
      to[Map[Symbol, Float]](Map("test" -> "1.0", "test2" -> "2.0"))
        .mustEqual(Some(Map('test -> 1.0f, 'test2 -> 2.0f)))
      to[Map[Boolean, Double]](Map("true" -> "1.2", "false" -> "2.5"))
        .mustEqual(Some(Map(true -> 1.2, false -> 2.5)))

      // Using Any to avoid conversion
      to[Map[Any, Char]](Map("true" -> "a", "false" -> "b"))
        .mustEqual(Some(Map("true" -> 'a', "false" -> 'b')))
      to[Map[Symbol, Any]](Map("true" -> "a", "false" -> "b"))
        .mustEqual(Some(Map('true -> "a", 'false -> "b")))

      // Failure cases
      to[Map[String, Boolean]](Map('true -> "1.2", 'false -> "2.5"))
        .mustEqual(None)
      to[Map[String, Int]](Map('true -> "1.0", 'false -> "2.0"))
        .mustEqual(None)

      // Best effort
      to[Map[String, Boolean]](Map('true -> "false", 'false -> "2.5"),true)
        .mustEqual(Some(Map("true" -> false)))
      to[Map[String, Int]](Map('true -> "1", 'false -> "2.0"),true)
        .mustEqual(Some(Map("true" -> 1)))

      // Embedded Maps/Lists
      to[Map[String, Map[Char,Int]]](Map('foo -> Map("a" -> 3))).mustEqual(
        Some(Map("foo" -> Map('a' -> 3))))
      to[Map[String, List[Int]]](Map('foo -> List("3", "4"))).mustEqual(
        Some(Map("foo" -> List(3, 4))))

      // Other types
      to[collection.mutable.Map[String,Int]](Map('test -> "3")).mustEqual(
        Some(collection.mutable.Map("test" -> 3)))
    }

    "support Object conversion" in {
      // From maps
      to[Test1](Map("s" -> "foo")).mustEqual(Some(Test1("foo")))

      to[Test2](Map("str" -> "bar", "num" -> 1, "sht" -> 2, "lng" -> 3,
        "flt" -> 1.0, "dbl" -> 2.0, "ch" -> 'a', "bool" -> true, "byt" -> 3))
        .mustEqual(Some(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))

      to[Test3](Map("xs" -> List("foo", "bar", "baz")))
        .mustEqual(Some(Test3(List("foo", "bar", "baz"))))

      to[Test4](Map("xs1" -> List("foo", "bar"), "xs2" -> List(1,2,3)))
        .mustEqual(Some(Test4(List("foo", "bar"), List(1,2,3))))

      to[Test5](Map("xs" -> List(Test1("test"), Test1("test2"))))
        .mustEqual(Some(Test5(List(Test1("test"), Test1("test2")))))
      
      to[Test6](Map("xm" -> Map("a" -> "b", "c" -> "d")))
        .mustEqual(Some(Test6(Map("a" -> "b", "c" -> "d"))))
      
      to[Test7](Map("xm1" -> Map("a" -> "b"), "xm2" -> Map("c" -> 2)))
        .mustEqual(Some(Test7(Map("a" -> "b"), Map("c" -> 2))))
      
      to[Test8](Map("t" -> Test1("foo"), 
          "t2" -> Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))
        .mustEqual(Some(Test8(Test1("foo"), 
            Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3))))
      
      to[Test9](Map("s" -> "foo", "i" -> 3)).mustEqual(
        Some(new Test9("foo", 3)))

      to[Test10](Map("s" -> "bar")).mustEqual(Some(new Test10("bar", 1)))
      to[Test10](Map("i" -> 3)).mustEqual(Some(new Test10("foo", 3)))
     
      val t11 = new Test11("foo")
      to[Test11](Map("s" -> "foo", "l" -> 4L, "b" -> false)).mustEqual(
        Some(t11))
      t11.b = true
      to[Test11](Map("s" -> "foo", "l" -> 4L, "b" -> true)).mustEqual(
        Some(t11))

      // With symbols
      to[Test2](Map('str -> "bar", 'num -> 1, 'sht -> 2, 'lng -> 3,
        'flt -> 1.0, 'dbl -> 2.0, 'ch -> 'a', 'bool -> true, 'byt -> 3))
        .mustEqual(Some(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))

      // From within other types (list, tuple, ...)
      to[List[Test1]](List(Map("s" -> "foo")))
        .mustEqual(Some(List(Test1("foo"))))
      to[Tuple2[Test1, Test1]](Map('s -> "foo") -> Map('s -> "bar"))
        .mustEqual(Some(Test1("foo") -> Test1("bar")))

      // from objects to Maps
      to[Map[Symbol, Any]](Test1("foo")).mustEqual(Some(Map('s -> "foo")))
      to[Map[Symbol, String]](Test1("foo")).mustEqual(Some(Map('s -> "foo")))
      to[Map[Symbol, Any]](Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3))
        .mustEqual(Some(Map('str -> "bar", 'num -> 1, 'sht -> 2, 'lng -> 3,
          'flt -> 1.0, 'dbl -> 2.0, 'ch -> 'a', 'bool -> true, 'byt -> 3)))

      // Support only converting fields that match type
      to[Map[Symbol, String]](Test2("bar", 1,2,3, 1.0f, 2.0, 'a', true, 3))
        .mustEqual(None)
      to[Map[Symbol, String]](Test2("bar", 1,2,3, 1.0f, 2.0, 'a', true, 3),true)
        .mustEqual(Some(Map('str -> "bar")))
      to[Map[String, Int]](Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3),true)
        .mustEqual(Some(Map("num" -> 1)))

      // Maps/Lists mixed with objects 
      to[List[Test10]](List(Map("i" -> "1")))
        .mustEqual(Some(List(new Test10("foo",1))))
      to[Map[Symbol,Test10]](Map('t -> Map("i" -> "1")))
        .mustEqual(Some(Map('t -> new Test10("foo",1))))
      to[Tuple2[Test1,Test1]](Map('s -> "foo"), Map('s -> "bar"))
        .mustEqual(Some(Test1("foo") -> Test1("bar")))

      // Between objects
      to[Test10](new Test9("foo", 3)).mustEqual(Some(new Test10("foo", 3)))
      to[Test9](new Test10("foo")).mustEqual(Some(new Test9("foo", 1)))
    }

    "support conversions using opaque data" in {
      implicit val testSettings = ConversionSettings(
        false,
        OpaqueDataSettings(true)
      )
      val t1 = to[Test1](Map('s -> "foo", 'i -> 1)).get
      OpaqueData.get(t1).mustEqual(Some(Map('i -> 1)))
      to[Map[Symbol,Any]](t1).mustEqual(Some(Map('s -> "foo", 'i -> 1)))
      val t2 = to[Map[String,Test1]](Map('t1 -> Map('s -> "foo", 'x -> 3),
        't2 -> Map('s -> "bar", 'y -> 7))).get
      OpaqueData.get(t2("t1")).mustEqual(Some(Map('x -> 3)))
      OpaqueData.get(t2("t2")).mustEqual(Some(Map('y -> 7)))
      to[Map[Symbol,Map[Symbol,Any]]](t2).mustEqual(Some(
        Map('t1 -> Map('s -> "foo", 'x -> 3), 
          't2 -> Map('s -> "bar", 'y -> 7))))

      // Reset
      implicit val conversionSettings = ConversionSettings(
        false,
        OpaqueDataSettings(false)
      )
      true.mustEqual(true)  // Keep specs happy
    }
  }
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
