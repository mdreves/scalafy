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
package test.scalafy.util.xml

import org.specs2.mutable.Specification

import scalafy.types.meta._
import scalafy.types.reifiable._
import scalafy.util._
import scalafy.util.casing._
import scalafy.util.xml._

/** Test specification for xml package */
object XmlSpec extends Specification {

  "The fromXml function" should {

    "support extracting XML passed as Strings" in {
      fromXml[String](wrapXml("foo")).mustEqual(Some("foo"))
      fromXml[String](wrapXml("foo bar")).mustEqual(Some("foo bar"))
      fromXml[String](wrapXml("foo \\bar\\")).mustEqual(Some("foo \\bar\\"))
      val t = "<result>foo \nbar</result>"
      fromXml[String](t.toString).mustEqual(Some("foo \nbar"))
      fromXml[String](wrapXml("foo \u0023bar")).mustEqual(Some("foo \u0023bar"))
      fromXml[String](wrapXml("foo &gt; bar")).mustEqual(Some("foo > bar"))
      fromXml[String](wrapXml("1234")).mustEqual(Some("1234"))
      // Failure cases
      fromXml[Int](wrapXml("foo")).mustEqual(None)
      fromXml[Long](wrapXml("foo")).mustEqual(None)
      fromXml[Double](wrapXml("foo")).mustEqual(None)
      fromXml[Boolean](wrapXml("foo")).mustEqual(None)
      fromXml[List[String]](wrapXml("foo")).mustEqual(None)
    }
 
    "support extracting XML as Shorts" in {
      fromXml[Short](wrapXml("1234")).mustEqual(Some(1234))
      fromXml[Short](wrapXml("1234")).mustEqual(Some(1234))
      fromXml[Short](wrapXml("-1234")).mustEqual(Some(-1234))
      // Failure cases
      fromXml[Short](wrapXml("2147483647")).mustEqual(None)
      fromXml[Short](wrapXml("-2147483648")).mustEqual(None)
    }
   
    "support extracting XML as Ints" in {
      fromXml[Int](wrapXml("1234")).mustEqual(Some(1234))
      fromXml[Int](wrapXml("1234")).mustEqual(Some(1234))
      fromXml[Int](wrapXml("-1234")).mustEqual(Some(-1234))
      fromXml[Int](wrapXml("2147483647")).mustEqual(Some(2147483647))
      fromXml[Int](wrapXml("-2147483648")).mustEqual(Some(-2147483648))
      // Failure cases
      fromXml[Int](wrapXml("9223372036854775807")).mustEqual(None)
      fromXml[Int](wrapXml("-9223372036854775808")).mustEqual(None)
      fromXml[Boolean](wrapXml("1234")).mustEqual(None)
      fromXml[List[Int]](wrapXml("1234")).mustEqual(None)
   }
    
    "support extracting XML as Longs" in {
      fromXml[Long](wrapXml("1234")).mustEqual(Some(1234L))
      fromXml[Long](wrapXml("9223372036854775807"))
        .mustEqual(Some(9223372036854775807L))
      fromXml[Long](wrapXml("-9223372036854775808"))
        .mustEqual(Some(-9223372036854775808L))
    }
 
    "support extracting XML as Floats" in {
      fromXml[Float](wrapXml("1.")).mustEqual(Some(1.f))
      fromXml[Float](wrapXml("-1.")).mustEqual(Some(-1.f))
      fromXml[Float](wrapXml("1.0")).mustEqual(Some(1.0f))
      fromXml[Float](wrapXml("-1.0")).mustEqual(Some(-1.0f))
      fromXml[Float](wrapXml("1.79769313486E10"))
        .mustEqual(Some(1.79769313486E10f))
      fromXml[Float](wrapXml("-1.7976931348E10"))
        .mustEqual(Some(-1.7976931348E10f))
      fromXml[Float](wrapXml("1.0e2")).mustEqual(Some(1.0e2f))
      fromXml[Float](wrapXml("-1.0e2")).mustEqual(Some(-1.0e2f))
      fromXml[Float](wrapXml("1.e2")).mustEqual(Some(1.e2f))
      fromXml[Float](wrapXml("-1.e2")).mustEqual(Some(-1.e2f))
      fromXml[Float](wrapXml("1.e+2")).mustEqual(Some(1.e+2f))
      fromXml[Float](wrapXml("-1.e+2")).mustEqual(Some(-1.e+2f))
      fromXml[Float](wrapXml("1.e-2")).mustEqual(Some(1.e-2f))
      fromXml[Float](wrapXml("-1.e-2")).mustEqual(Some(-1.e-2f))
      fromXml[Float](wrapXml("1.0E2")).mustEqual(Some(1.0E2f))
      fromXml[Float](wrapXml("-1.0E2")).mustEqual(Some(-1.0E2f))
      fromXml[Float](wrapXml("1.E2")).mustEqual(Some(1.E2f))
      fromXml[Float](wrapXml("-1.E2")).mustEqual(Some(-1.E2f))
      fromXml[Float](wrapXml("1.E+2")).mustEqual(Some(1.E+2f))
      fromXml[Float](wrapXml("-1.E+2")).mustEqual(Some(-1.E+2f))
      fromXml[Float](wrapXml("1.E-2")).mustEqual(Some(1.E-2f))
      fromXml[Float](wrapXml("-1.E-2")).mustEqual(Some(-1.E-2f))
      fromXml[Float](wrapXml("1234")).mustEqual(Some(1234.0f))
    }
  
    "support extracting XML as Doubles" in {
      fromXml[Double](wrapXml("1.")).mustEqual(Some(1.))
      fromXml[Double](wrapXml("-1.")).mustEqual(Some(-1.))
      fromXml[Double](wrapXml("1.0")).mustEqual(Some(1.0))
      fromXml[Double](wrapXml("-1.0")).mustEqual(Some(-1.0))
      fromXml[Double](wrapXml("1.7976931348623157E308"))
        .mustEqual(Some(1.7976931348623157E308))
      fromXml[Double](wrapXml("-1.7976931348623157E308"))
        .mustEqual(Some(-1.7976931348623157E308))
      fromXml[Double](wrapXml("1.0e2")).mustEqual(Some(1.0e2))
      fromXml[Double](wrapXml("-1.0e2")).mustEqual(Some(-1.0e2))
      fromXml[Double](wrapXml("1.e2")).mustEqual(Some(1.e2))
      fromXml[Double](wrapXml("-1.e2")).mustEqual(Some(-1.e2))
      fromXml[Double](wrapXml("1.e+2")).mustEqual(Some(1.e+2))
      fromXml[Double](wrapXml("-1.e+2")).mustEqual(Some(-1.e+2))
      fromXml[Double](wrapXml("1.e-2")).mustEqual(Some(1.e-2))
      fromXml[Double](wrapXml("-1.e-2")).mustEqual(Some(-1.e-2))
      fromXml[Double](wrapXml("1.0E2")).mustEqual(Some(1.0E2))
      fromXml[Double](wrapXml("-1.0E2")).mustEqual(Some(-1.0E2))
      fromXml[Double](wrapXml("1.E2")).mustEqual(Some(1.E2))
      fromXml[Double](wrapXml("-1.E2")).mustEqual(Some(-1.E2))
      fromXml[Double](wrapXml("1.E+2")).mustEqual(Some(1.E+2))
      fromXml[Double](wrapXml("-1.E+2")).mustEqual(Some(-1.E+2))
      fromXml[Double](wrapXml("1.E-2")).mustEqual(Some(1.E-2))
      fromXml[Double](wrapXml("-1.E-2")).mustEqual(Some(-1.E-2))
      fromXml[Double](wrapXml("1234")).mustEqual(Some(1234.0))
    }
    
    "support extracting XML as Booleans" in {
      fromXml[Boolean](wrapXml("true")).mustEqual(Some(true))
      fromXml[Boolean](wrapXml("false")).mustEqual(Some(false))
    }
 
    "support extracting XML as Chars" in {
      fromXml[Char](wrapXml("a")).mustEqual(Some('a'))
      fromXml[Char](wrapXml("b")).mustEqual(Some('b'))
      // Failure cases
      fromXml[Char](wrapXml("ab")).mustEqual(None)
    }
   
    "support extracting XML as Bytes" in {
      fromXml[Byte](wrapXml("123")).mustEqual(Some(123))
      fromXml[Byte](wrapXml("1234")).mustEqual(None)
    }

    "support extracting XML as BigInt/BigDecimal" in {
      fromXml[BigInt](wrapXml("123331234123411233"))
        .mustEqual(Some(BigInt("123331234123411233")))
      fromXml[BigDecimal](wrapXml("123331234123411233"))
        .mustEqual(Some(BigDecimal("123331234123411233")))
      fromXml[Map[String,BigInt]](wrapXml(
        "<foo>123331234123411233</foo>"))
        .mustEqual(Some(Map("foo" -> BigInt("123331234123411233"))))
    }

    "support extracting XML as Dates and Timezones" in {
      val formatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
      fromXml[java.util.Date](wrapXml("2012-01-01T20:01:01Z")).mustEqual(
        Some(formatter.parse("2012-01-01T20:01:01Z")))
      fromXml[List[java.util.Date]](wrapXml(
          "<item>2012-01-01T20:01:01Z</item><item>2012-02-01T23:11:01Z</item>"))
        .mustEqual(
          Some(List(formatter.parse("2012-01-01T20:01:01Z"),
            formatter.parse("2012-02-01T23:11:01Z"))))
      fromXml[java.util.TimeZone](wrapXml("PST")).mustEqual(
        Some(java.util.TimeZone.getTimeZone("PST")))
    }

    "support extracting XML as null" in {
      fromXml[Any](<result xsi:nil="true"/>.toString).mustEqual(Some(null))
      fromXml[String](<result xsi:nil="true"/>.toString).mustEqual(Some(null))
      fromXml[List[String]](
        <result xsi:nil="true"/>.toString).mustEqual(Some(null))
      fromXml[Map[String,String]](
        <result xsi:nil="true"/>.toString).mustEqual(Some(null))
      // Failure cases
      fromXml[Any](<result xsi:nil="false"/>.toString).mustEqual(None)
      fromXml[Int](<result xsi:nil="true"/>.toString).mustEqual(None)
      fromXml[Long](<result xsi:nil="true"/>.toString).mustEqual(None)
      fromXml[Double](<result xsi:nil="true"/>.toString).mustEqual(None)
      fromXml[Boolean](<result xsi:nil="true"/>.toString).mustEqual(None) 
    }

    "support extracting XML with Option types" in {
      fromXml[Option[String]](wrapXml("")).mustEqual(Some(None))
      fromXml[Option[String]](wrapXml("foo")).mustEqual(Some(Some("foo")))
      fromXml[Option[Int]](wrapXml("")).mustEqual(Some(None))
      fromXml[Option[Int]](wrapXml("123")).mustEqual(Some(Some(123)))
      fromXml[Option[List[String]]](wrapXml("")).mustEqual(Some(None))
      fromXml[Option[List[Int]]](wrapXml(mkList(List(1,2)))).mustEqual(
        Some(Some(List(1,2))))
      fromXml[List[Option[Int]]](
          wrapXml(mkList(List(1,2))+"<item xsi:nil=\"true\"/>"))
        .mustEqual(Some(List(Some(1),Some(2),None)))
      fromXml[Map[String, Option[String]]](
          wrapXml(mkMap(Map("foo" -> 2, "bar" -> null))))
        .mustEqual(
          Some(Map("foo" -> Some("2"), "bar" -> None)))
      // Requires type hints
      fromXml[Test12](wrapXml(Test12("foo", None).toXml), TestData.typeHints)
        .mustEqual(Some(Test12("foo", None)))
      fromXml[Test12](wrapXml(Test12("foo", Some(1)).toXml), TestData.typeHints)
        .mustEqual(Some(Test12("foo", Some(1))))
    }

    "support extracting XML as List of primitives" in {
      fromXml[List[Int]](wrapXml(mkList(List(1, 2, 3))))
        .mustEqual(Some(List(1,2,3)))
      fromXml[List[Short]](wrapXml(mkList(List(1, 2, 3))))
        .mustEqual(Some(List[Short](1,2,3)))
      fromXml[List[Long]](wrapXml(mkList(List(2147483648L, 2147483649L))))
        .mustEqual(Some(List(2147483648L, 2147483649L)))
      fromXml[List[Double]](wrapXml(mkList(List(1.0, 2.0, 3.0))))
        .mustEqual(Some(List(1.0, 2.0, 3.0)))
      fromXml[List[Float]](wrapXml(mkList(List(1.0f, 2.0f, 3.0f))))
        .mustEqual(Some(List(1.0f, 2.0f, 3.0f)))
      fromXml[List[Boolean]](wrapXml(mkList(List(true, false, true))))
        .mustEqual(Some(List(true, false, true)))
      fromXml[List[String]](wrapXml(mkList(List("foo", "bar"))))
        .mustEqual(Some(List("foo", "bar")))
      fromXml[List[Char]](wrapXml(mkList(List("a", "b"))))
        .mustEqual(Some(List('a','b')))
      // Failure cases
      fromXml[List[Int]](wrapXml(mkList(List("foo", "bar"))))
        .mustEqual(None)
    }
 
    "support extracting XML as Tuples" in {
      fromXml[Tuple2[Symbol,Int]](wrapXml(mkList(List("x",1))))
        .mustEqual(Some('x -> 1))
      fromXml[Tuple3[String,Int,Boolean]](wrapXml(mkList(List("x",1, true))))
        .mustEqual(
        Some(Tuple3("x", 1, true)))
      fromXml[List[Tuple2[Symbol,Int]]](
          wrapXml(mkList(List(mkList(List("x",1)),mkList(List("y",2))))))
           .mustEqual(Some(List('x -> 1, 'y -> 2)))
      fromXml[Map[Symbol,Tuple2[Symbol,Int]]](
          wrapXml(mkMap(Map('x -> List("y", 2)))))
        .mustEqual(Some(Map('x -> ('y -> 2))))
    }
  
    "support extracting XML as Map of primitives" in {
      fromXml[Map[String,Int]](wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
        .mustEqual(Some(Map("name1" -> 1, "name2" -> 2)))
      fromXml[Map[String,Short]](
          wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
        .mustEqual(Some(Map[String,Short]("name1" -> 1, "name2" -> 2)))
      fromXml[Map[String,Long]](
          wrapXml(mkMap(Map("name1" -> 2147483684L, "name2" -> 2147483649L))))
        .mustEqual(
          Some(Map("name1" -> 2147483684L, "name2" -> 2147483649L)))
      fromXml[Map[String,Double]](
          wrapXml(mkMap(Map("name1" -> 1.0, "name2" -> 2.0))))
        .mustEqual(
          Some(Map("name1" -> 1.0, "name2" -> 2.0)))
      fromXml[Map[String,Float]](
          wrapXml(mkMap(Map("name1" -> 1.0, "name2" -> 2.0))))
        .mustEqual(
          Some(Map("name1" -> 1.0f, "name2" -> 2.0f)))
      fromXml[Map[String,Boolean]](
          wrapXml(mkMap(Map("name1" -> true, "name2" -> false))))
        .mustEqual(
          Some(Map("name1" -> true, "name2" -> false)))
      fromXml[Map[String,String]](
          wrapXml(mkMap(Map("name1" -> "foo", "name2" -> "bar"))))
        .mustEqual(
          Some(Map("name1" -> "foo", "name2" -> "bar")))
      fromXml[Map[String,Char]](
          wrapXml(mkMap(Map("name1" -> "a", "name2" -> "b"))))
        .mustEqual(
          Some(Map("name1" -> 'a', "name2" -> 'b')))
      // Failure cases
      fromXml[Map[String,Int]](
          wrapXml(mkMap(Map("name1" -> "foo", "name2" -> "bar"))))
        .mustEqual(None)
    }

    "support extracting XML as Lists of Lists" in {
      fromXml[List[Any]](
          wrapXml(mkList((List(List("foo", "bar"), List(1,2))))))
        .mustEqual(
          Some(List(List("foo", "bar"), List(1,2))))
      fromXml[List[Any]](
          wrapXml(mkList(
            List(List("foo", "bar"), 
            List(1,List(1.0,2.0),2), null))))
        .mustEqual(Some(
          List(List("foo", "bar"), 
          List(1,List(1.0,2.0),2), null)))
      // Failure cases
      fromXml[List[Int]](
          wrapXml(mkList((List(List("foo", "bar"), List(1,2))))))
        .mustEqual(None)
    }
    
    "support extracting XML as Lists of Maps" in {
      fromXml[List[Map[String,String]]](
          wrapXml(mkList(List(Map("name1" -> "foo", "name2" -> "bar"),
            Map("name1" -> "foo2", "name2" -> "bar2")))))
        .mustEqual(Some(
          List(Map("name1" -> "foo", "name2" -> "bar"),
            Map("name1" -> "foo2", "name2" -> "bar2"))))
      // Failure cases
      fromXml[List[Map[String,Int]]](
          wrapXml(mkList(List(Map("name1" -> "foo", "name2" -> "bar"),
            Map("name1" -> "foo2", "name2" -> "bar2")))))
        .mustEqual(None)
    }

    "support extracting XML as Map of Lists" in {
      fromXml[Map[String,List[String]]](
          wrapXml(mkMap(Map("name1" -> List("foo","bar"), 
            "name2" -> List("bar2")))))
        .mustEqual(Some(
          Map("name1" -> List("foo","bar"), 
            "name2" -> List("bar2"))))
      // Failure cases
      fromXml[Map[String,List[Int]]](
          wrapXml(mkMap(Map("name1" -> List("foo","bar"), 
            "name2" -> List("bar2")))))
        .mustEqual(None)
    }

    "support extracting XML as Map of Maps" in {
      fromXml[Map[String, Map[String,String]]](
          wrapXml(mkMap(Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> null)))))
        .mustEqual(Some(
          Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> null))))
      // Failure cases
      fromXml[Map[String, Map[String,Int]]](
          wrapXml(mkMap(Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> null)))))
        .mustEqual(None)
    }

    "support extracting XML using Any type" in {
      fromXml[List[Any]](wrapXml(mkList(List(1,2,3))))
        .mustEqual(Some(List(1,2,3)))
      fromXml[Map[String,Map[String,Any]]](
          wrapXml(mkMap(Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> null)))))
        .mustEqual(Some(
          Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> null))))
    }

    "support extracting XML using Maps of non-Symbol primitive keys" in {
      fromXml[Map[Symbol,String]](
          wrapXml(mkMap(Map("name1" -> "foo", "name2" -> "bar"))))
        .mustEqual(
          Some(Map('name1 -> "foo", 'name2 -> "bar")))
      fromXml[Map[String,String]](
          wrapXml(mkMap(Map("name1" -> "foo", "name2" -> "bar"))))
        .mustEqual(
          Some(Map("name1" -> "foo", "name2" -> "bar")))
      fromXml[Map[Int,String]](
          wrapXml(mkMap(Map(1 -> "foo", 2 -> "bar"))))
        .mustEqual(Some(Map(1 -> "foo", 2 -> "bar")))
      fromXml[Map[Short,String]](
          wrapXml(mkMap(Map(1 -> "foo", 2 -> "bar"))))
        .mustEqual(Some(Map(1.toShort -> "foo", 2.toShort -> "bar")))
      fromXml[Map[Long,String]](
          wrapXml(mkMap(Map(1 -> "foo", 2 -> "bar"))))
        .mustEqual(Some(Map(1L -> "foo", 2L -> "bar")))
      fromXml[Map[Byte,String]](
          wrapXml(mkMap(Map(1 -> "foo", 2 -> "bar"))))
        .mustEqual(Some(Map(1.toByte -> "foo", 2.toByte -> "bar")))
      fromXml[Map[Float,String]](
          wrapXml(mkMap(Map(1.0f -> "foo", 2.0f -> "bar"))))
        .mustEqual(Some(Map(1.0f -> "foo", 2.0f -> "bar")))
      fromXml[Map[Double,String]](
          wrapXml(mkMap(Map(1.0 -> "foo", 2.0 -> "bar"))))
        .mustEqual(Some(Map(1.0 -> "foo", 2.0 -> "bar")))
      fromXml[Map[Boolean,String]](
          wrapXml(mkMap(Map("true" -> "foo", "false" -> "bar"))))
        .mustEqual(Some(Map(true -> "foo", false -> "bar")))
      fromXml[Map[Char,String]](
          wrapXml(mkMap(Map("a" -> "foo", "b" -> "bar"))))
        .mustEqual(Some(Map('a' -> "foo", 'b' -> "bar")))
      fromXml[Map[Int,Int]](
          wrapXml(mkMap(Map(1 -> 3, 2 -> "4"))))
        .mustEqual(Some(Map(1 -> 3, 2 -> 4)))
    }

    "support extracting XML using Uniform types" in {
      import scalafy.collection.uniform._

      // Primitives
      fromXml[UniformString](wrapXml("foo"))
        .mustEqual(Some(UniformString("foo")))
      fromXml[UniformData[String]](wrapXml("foo"))
        .mustEqual(Some(UniformString("foo")))
      fromXml[UniformPrimitive[String]](wrapXml("foo"))
        .mustEqual(Some(UniformString("foo")))
      fromXml[UniformSymbol](wrapXml("foo"))
        .mustEqual(Some(UniformSymbol('foo)))
      fromXml[UniformInt](wrapXml("1")).mustEqual(Some(UniformInt(1)))
      fromXml[UniformByte](wrapXml("1")).mustEqual(Some(UniformByte(1)))
      fromXml[UniformShort](wrapXml("1"))
        .mustEqual(Some(UniformShort(1.toShort)))
      fromXml[UniformLong](wrapXml("1")).mustEqual(Some(UniformLong(1L)))
      fromXml[UniformFloat](wrapXml("1")).mustEqual(Some(UniformFloat(1.0f)))
      fromXml[UniformDouble](wrapXml("1")).mustEqual(Some(UniformDouble(1.0)))
      fromXml[UniformBoolean](wrapXml("false"))
        .mustEqual(Some(UniformBoolean(false)))
      fromXml[UniformChar](wrapXml("f")).mustEqual(Some(UniformChar('f')))

      // Primitives with lists
      fromXml[UniformList[Int]](wrapXml(mkList(List(1, 2, 3))))
        .mustEqual(Some(UniformList(1,2,3)))
      fromXml[UniformList[Short]](wrapXml(mkList(List(1, 2, 3))))
        .mustEqual(Some(
          UniformList(1.toShort,2.toShort,3.toShort).filter[Short]))
      fromXml[UniformList[Long]](
          wrapXml(mkList(List(2147483648L, 2147483649L))))
        .mustEqual(Some(UniformList(2147483648L, 2147483649L)))
      fromXml[UniformList[Double]](wrapXml(mkList(List(1.0, 2.0, 3.0))))
        .mustEqual(Some(UniformList(1.0, 2.0, 3.0)))
      fromXml[UniformList[Float]](wrapXml(mkList(List(1.0f, 2.0f, 3.0f))))
        .mustEqual(Some(UniformList(1.0f, 2.0f, 3.0f)))
      fromXml[UniformList[Boolean]](wrapXml(mkList(List(true,false,true))))
        .mustEqual(Some(UniformList(true, false, true)))
      fromXml[UniformList[String]](wrapXml(mkList(List("foo","bar"))))
        .mustEqual(Some(UniformList("foo", "bar")))
      fromXml[UniformList[Char]](wrapXml(mkList(List('a','b'))))
        .mustEqual(Some(UniformList('a','b')))
      // Failure cases
      fromXml[UniformList[Int]](wrapXml(mkList(List("foo","bar"))))
        .mustEqual(None)

      // Primitives with maps
      fromXml[UniformMap[Int]](
          wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
        .mustEqual(Some(UniformMap('name1 -> 1, 'name2 -> 2)))
      fromXml[UniformMap[Short]](
          wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
        .mustEqual(Some(
          UniformMap('name1 -> 1.toShort, 'name2 -> 2.toShort).filter[Short]))
      fromXml[UniformMap[Long]](
          wrapXml(mkMap(Map("name1" -> 2147483684L, "name2" -> 2147483649L))))
        .mustEqual(
          Some(UniformMap('name1 -> 2147483684L, 'name2 -> 2147483649L)))
      fromXml[UniformMap[Double]](
          wrapXml(mkMap(Map("name1" -> 1.0, "name2" -> 2.0))))
        .mustEqual(
          Some(UniformMap('name1 -> 1.0, 'name2 -> 2.0)))
      fromXml[UniformMap[Float]](
          wrapXml(mkMap(Map("name1" -> 1.0, "name2" -> 2.0))))
        .mustEqual(
          Some(UniformMap('name1 -> 1.0f, 'name2 -> 2.0f)))
      fromXml[UniformMap[Boolean]](
          wrapXml(mkMap(Map("name1" -> true, "name2" -> false))))
        .mustEqual(
          Some(UniformMap('name1 -> true, 'name2 -> false)))
      fromXml[UniformMap[String]](
          wrapXml(mkMap(Map("name1" -> "foo", "name2" -> "bar"))))
        .mustEqual(
          Some(UniformMap('name1 -> "foo", 'name2 -> "bar")))
      fromXml[UniformMap[Char]](
          wrapXml(mkMap(Map("name1" -> "a", "name2" -> "b"))))
        .mustEqual(
          Some(UniformMap('name1 -> 'a', 'name2 -> 'b')))
      // Failure cases
      fromXml[UniformMap[Int]](
          wrapXml(mkMap(Map("name1" -> "foo", "name2" -> "bar"))))
        .mustEqual(None)
      
      // Lists in lists
      fromXml[UniformList[Any]](
          wrapXml(mkList((List(List("foo", "bar"), List(1,2))))))
        .mustEqual(
          Some(UniformList(UniformList("foo", "bar"), UniformList(1,2))))
      fromXml[UniformList[Any]](
          wrapXml(mkList(
            List(List("foo", "bar"), 
            List(1,List(1.0,2.0),2)))))
        .mustEqual(Some(
          UniformList(UniformList("foo", "bar"), 
          UniformList(1,UniformList(1.0,2.0),2))))
      // Failure cases
      fromXml[UniformList[Int]](
          wrapXml(mkList((List(List("foo", "bar"), List(1,2))))))
        .mustEqual(None)

      // Maps in maps
      fromXml[UniformMap[UniformMap[String]]](
          wrapXml(mkMap(Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> "foo")))))
        .mustEqual(Some(
          UniformMap('name1 -> UniformMap('name3 -> "bar"), 
            'name2 -> UniformMap('name4 -> "foo"))))
      // Failure cases
      fromXml[UniformMap[UniformMap[Int]]](
          wrapXml(mkMap(Map("name1" -> Map("name3" -> "bar"), 
            "name2" -> Map("name4" -> "foo")))))
        .mustEqual(None)

      // Lists of Maps
      fromXml[UniformList[UniformMap[String]]](
          wrapXml(mkList(List(Map("name1" -> "foo", "name2" -> "bar"),
            Map("name1" -> "foo2", "name2" -> "bar2")))))
        .mustEqual(Some(
          UniformList(UniformMap('name1 -> "foo", 'name2 -> "bar"),
            UniformMap('name1 -> "foo2", 'name2 -> "bar2"))))
      // Failure cases
      fromXml[UniformList[UniformMap[Int]]](
          wrapXml(mkList(List(Map("name1" -> "foo", "name2" -> "bar"),
            Map("name1" -> "foo2", "name2" -> "bar2")))))
        .mustEqual(None)

      // Map of Lists
      fromXml[UniformMap[UniformList[String]]](
          wrapXml(mkMap(Map("name1" -> List("foo","bar"), 
            "name2" -> List("bar2")))))
        .mustEqual(Some(
          UniformMap('name1 -> UniformList("foo","bar"), 
            'name2 -> UniformList("bar2"))))
      // Failure cases
      fromXml[UniformMap[UniformList[Int]]](
          wrapXml(mkMap(Map("name1" -> List("foo","bar"), 
            "name2" -> List("bar2")))))
        .mustEqual(None)
    }

    "support extracting XML using Any type and Reifiable" in {
      implicit val testSettings = XmlSettings(
        IgnoreCasing,
        PrettyPrintSettings(false, 2),
        "result",
        "item",
        new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"),        
        TypeHintSettings(),
        null, 
        OpaqueDataSettings(false),
        ReifiableSettings(true)
      )

      (fromXml[Any](wrapXml(mkList(List(1,2,3)))) match {
        case Some(xs) if (xs.isType[List[Int]]) => "match"
        case _ => "no match"
      }).mustEqual("match")

      (fromXml[Any](wrapXml(mkMap(Map("name1" -> 2)))) match {
        case Some(xm) if (xm.isType[Map[Symbol, Int]]) => "match"
        case _ => "no match"
      }).mustEqual("match")

      (fromXml[List[Tuple2[Any,Any]]](
          wrapXml(mkList(List(List("x",1),List("y",2))))) match {
        case Some(xm) if (xm.isType[List[Tuple2[String, Int]]]) => "match"
        case _ => "no match"
      }).mustEqual("match")

      import scalafy.collection.uniform._
      (fromXml[UniformList[Any]](
          wrapXml(mkList(List(List(1,2),List(3,4))))) match {
        case Some(xm) if (xm.isType[UniformList[UniformList[Int]]]) => "match"
        case _ => "no match"
      }).mustEqual("match")


      // Failure cases 
      (fromXml[Any](wrapXml(mkMap(Map("name1" -> 2)))) match {
        case Some(xm) if (xm.isType[Map[Symbol, String]]) => "match"
        case _ => "no match"
      }).mustEqual("no match")

      // Rest
      implicit val xmlSettings = XmlSettings(
        IgnoreCasing,
        PrettyPrintSettings(false, 2),
        "result",
        "item",
        new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"), 
        TypeHintSettings(),
        null,
        OpaqueDataSettings(false),
        ReifiableSettings(false)
      )
      true.mustEqual(true)  // Keep specs happy
    }

    "support extracting XML and storing opaque data" in {
      implicit val testSettings = XmlSettings(
        IgnoreCasing,
        PrettyPrintSettings(false, 2),
        "result",
        "item",
        new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"), 
        TypeHintSettings(),
        null,
        OpaqueDataSettings(true),
        ReifiableSettings(false)
      )

      val t1 = fromXml[Test1](wrapXml(mkMap(Map("s" -> "foo", "i" -> 1)))).get
      OpaqueData.get(t1).mustEqual(Some(Map('i -> 1)))
      toXml(t1).mustEqual(wrapXml(mkMap(Map("s" -> "foo", "i" -> 1))))

      val t2 = fromXml[Map[String,Test1]](
        wrapXml(mkMap(Map("t1" -> Map("s" -> "foo", "x" -> 3),
          "t2" -> Map("s" -> "bar", "y" -> 7))))).get
      OpaqueData.get(t2("t1")).mustEqual(Some(Map('x -> 3)))
      OpaqueData.get(t2("t2")).mustEqual(Some(Map('y -> 7)))
      toXml(t2).mustEqual(
        wrapXml(mkMap(Map("t1" -> Map("s" -> "foo", "x" -> 3),
          "t2" -> Map("s" -> "bar", "y" -> 7)))))

      // Reset
      implicit val xmlSettings = XmlSettings(
        IgnoreCasing,
        PrettyPrintSettings(false, 2),
        "result",
        "item",
        new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"), 
        TypeHintSettings(),
        null,
        OpaqueDataSettings(false),
        ReifiableSettings(false)
      )
      true.mustEqual(true)  // Keep specs happy
    }

    "support extracting XML using reflection" in {
      fromXml[Test1](wrapXml(mkMap(Map("s" -> "foo"))))
        .mustEqual(Some(Test1("foo")))
      fromXml[Test2](
          wrapXml(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3).toXml))
        .mustEqual(
          Some(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))
    }

    "support extracting XML using List of objects from reflection" in {
      fromXml[List[Test1]](wrapXml(mkList(List(Test1("foo").toXml,
        Test1("bar").toXml))))
        .mustEqual(Some(List(Test1("foo"), Test1("bar"))))
    }

    "support extracting XML using Maps of objects from reflection" in {
      fromXml[Map[Int,Test1]](
        wrapXml(mkMap(Map(10 -> Test1("foo").toXml, 15 -> Test1("bar").toXml))))
        .mustEqual(Some(Map(10 -> Test1("foo"), 15 -> Test1("bar"))))
    }

    "support extracting XML using reflection with embedded List types" in {
      fromXml[Test3](wrapXml(Test3(List("foo", "bar")).toXml), 
          TestData.typeHints).mustEqual(
        Some(Test3(List("foo", "bar"))))
      fromXml[Test4](wrapXml(Test4(List("foo", "bar"), List(1, 2)).toXml),
          TestData.typeHints)
        .mustEqual(Some(Test4(List("foo", "bar"), List(1, 2))))
      fromXml[Test5](wrapXml(Test5(List(Test1("foo"),Test1("bar"))).toXml),
          TestData.typeHints)
        .mustEqual(Some(Test5(List(Test1("foo"),Test1("bar")))))
    }

    "support extracting XML using reflection with embedded Map types" in {
      fromXml[Test6](wrapXml(Test6(Map("foo" -> "bar")).toXml), 
          TestData.typeHints)
        .mustEqual(Some(Test6(Map("foo" -> "bar"))))
      fromXml[Test7](wrapXml(Test7(Map("foo" -> "bar"), Map("bat" -> 2)).toXml),
          TestData.typeHints)
        .mustEqual(Some(Test7(Map("foo" -> "bar"), Map("bat" -> 2))))
    }

    "support extracting XML using reflection with embedded objects" in {
      fromXml[Test8](wrapXml(Test8(
            Test1("foo"), 
            Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)).toXml))
        .mustEqual(Some(Test8(
            Test1("foo"), 
            Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3))))
    }

    "support extracting XML using reflection with non-case class types" in {
      fromXml[Test9](wrapXml(new Test9("foo", 2).toXml))
        .mustEqual(Some(new Test9("foo", 2)))
    }

    "support extracting XML using reflection with multiple ctors" in {
      fromXml[Test10](wrapXml(new Test10("foo", 1).toXml))
        .mustEqual(Some(new Test10("foo", 1)))
      fromXml[Test10](wrapXml(new Test10("foo", 2).toXml))
        .mustEqual(Some(new Test10("foo", 2)))
      // This tests setting a field after construction
      val test11 = new Test11("foo")
      test11.b = true
      fromXml[Test11](wrapXml(test11.toXml)).mustEqual(Some(test11))
    }

    "support extracting JSON as Enumerations/singletons" in {
      /* Doesn't work in Specs, works outside
      case object Foo
      fromXml[Foo.type](wrapXml("Foo")).mustEqual(Some(Foo)) */

      fromXml[List[WeekDay.Value]](wrapXml(mkList(List("Mon","Wed"))),
          List(WeekDay))
        .mustEqual(Some(List(WeekDay.Mon, WeekDay.Wed)))

      val t = Test13(WeekDay.Mon)
      fromXml[Test13](wrapXml(mkMap(Map("d" -> "Mon"))), TestData.typeHints)
        .mustEqual(Some(t))
    }

    "support lazy conversion to XML using iterable/iterator" in {
      val iterable = fromXml[Iterable[Int]](wrapXml(mkList(List(1, 2, 3)))).get
      val iterator1 = iterable.iterator
      var data1 = List[Int]()
      while (iterator1.hasNext) data1 :+= iterator1.next()
      data1.mustEqual(List(1,2,3))
      
      val iterator2 = fromXml[Iterator[Int]](wrapXml(mkList(List(1, 2, 3)))).get
      var data2 = List[Int]()
      while (iterator2.hasNext) data2 :+= iterator2.next()
      data2.mustEqual(List(1,2,3))

      val iterator3 = fromXml[Iterator[Test1]](
         wrapXml(mkList(List(Test1("foo").toXml, Test1("bar").toXml)))).get
      var data3 = List[Test1]()
      while (iterator3.hasNext) data3 :+= iterator3.next()
      data3.mustEqual(List(Test1("foo"),Test1("bar")))
    }

    "support lazy conversion to XML using Stream" in {
      fromXml[collection.immutable.Stream[Int]](
        wrapXml(mkList(List(1, 2, 3))))
        .mustEqual(Some(collection.immutable.Stream(1,2,3)))
    }

    "support conversion to XML using other sequence types" in {
      val xml = wrapXml(mkList(List(1,2,3)))

      fromXml[::[Int]](xml).mustEqual(Some(1 :: 2 :: 3 :: Nil))
      fromXml[Vector[Int]](xml).mustEqual(Some(Vector(1,2,3)))
      fromXml[Seq[Int]](xml).mustEqual(Some(Seq(1,2,3)))
      fromXml[IndexedSeq[Int]](xml).mustEqual(Some(IndexedSeq(1,2,3)))
      fromXml[collection.immutable.LinearSeq[Int]](xml)
        .mustEqual(Some(collection.immutable.LinearSeq(1,2,3)))
      fromXml[collection.immutable.HashSet[Int]](xml)
        .mustEqual(Some(collection.immutable.HashSet(1,2,3)))
      fromXml[collection.immutable.SortedSet[Int]](xml)
        .mustEqual(Some(collection.immutable.SortedSet(1,2,3)))
      fromXml[collection.immutable.TreeSet[Int]](xml)
        .mustEqual(Some(collection.immutable.TreeSet(1,2,3)))
      fromXml[collection.immutable.ListSet[Int]](xml)
        .mustEqual(Some(collection.immutable.ListSet(1,2,3)))
      fromXml[Set[Int]](xml).mustEqual(Some(Set(1,2,3)))
      fromXml[collection.immutable.Stack[Int]](xml)
        .mustEqual(Some(collection.immutable.Stack(1,2,3)))
      fromXml[collection.immutable.Queue[Int]](xml)
        .mustEqual(Some(collection.immutable.Queue(1,2,3)))
      fromXml[collection.mutable.ListBuffer[Int]](xml)
        .mustEqual(Some(collection.mutable.ListBuffer(1,2,3)))
      // Priority queue's don't compare equal....
      fromXml[collection.mutable.PriorityQueue[Int]](xml).get.toList
        .mustEqual(collection.mutable.PriorityQueue(1,2,3).toList)
      fromXml[collection.mutable.Queue[Int]](xml)
        .mustEqual(Some(collection.mutable.Queue(1,2,3)))
      fromXml[collection.mutable.HashSet[Int]](xml)
        .mustEqual(Some(collection.mutable.HashSet(1,2,3)))
      fromXml[collection.mutable.LinkedHashSet[Int]](xml)
        .mustEqual(Some(collection.mutable.Set(1,2,3)))
      fromXml[collection.mutable.Set[Int]](xml)
        .mustEqual(Some(collection.mutable.Set(1,2,3)))
      fromXml[collection.mutable.ArrayBuffer[Int]](xml)
        .mustEqual(Some(collection.mutable.ArrayBuffer(1,2,3)))
      fromXml[collection.mutable.ResizableArray[Int]](xml)
        .mustEqual(Some(collection.mutable.ResizableArray(1,2,3)))
      // NOTE: Bug in scala, third item is null
      fromXml[collection.mutable.ArrayStack[Int]](wrapXml(mkList(List(1,2))))
        .mustEqual(Some(collection.mutable.ArrayStack(1,2)))
      fromXml[collection.mutable.Stack[Int]](xml)
        .mustEqual(Some(collection.mutable.Stack(1,2,3)))
      fromXml[collection.mutable.LinkedList[Int]](xml)
        .mustEqual(Some(collection.mutable.LinkedList(1,2,3)))
      fromXml[collection.mutable.DoubleLinkedList[Int]](xml)
        .mustEqual(Some(collection.mutable.DoubleLinkedList(1,2,3)))
      fromXml[collection.mutable.MutableList[Int]](xml)
        .mustEqual(Some(collection.mutable.MutableList(1,2,3)))
      fromXml[collection.mutable.ArraySeq[Int]](xml)
        .mustEqual(Some(collection.mutable.ArraySeq(1,2,3)))
      fromXml[collection.mutable.IndexedSeq[Int]](xml)
        .mustEqual(Some(collection.mutable.IndexedSeq(1,2,3)))
      fromXml[collection.mutable.LinearSeq[Int]](xml)
        .mustEqual(Some(collection.mutable.LinearSeq(1,2,3)))
      fromXml[collection.mutable.Seq[Int]](xml)
        .mustEqual(Some(collection.mutable.Seq(1,2,3)))
      fromXml[collection.mutable.UnrolledBuffer[Int]](xml)
        .mustEqual(Some(collection.mutable.UnrolledBuffer(1,2,3)))
      fromXml[collection.mutable.Buffer[Int]](xml)
        .mustEqual(Some(collection.mutable.Buffer(1,2,3)))
    }

    "support conversion to XML using other Map types" in {
      val xml = wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2)))

      fromXml[collection.immutable.HashMap[String,Int]](xml)
        .mustEqual(Some(collection.immutable.HashMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.immutable.TreeMap[String,Int]](xml)
        .mustEqual(Some(collection.immutable.TreeMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.immutable.SortedMap[String,Int]](xml)
        .mustEqual(Some(collection.immutable.SortedMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.immutable.ListMap[String,Int]](xml)
        .mustEqual(Some(collection.immutable.ListMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.mutable.Map[String,Int]](xml)
        .mustEqual(Some(collection.mutable.Map(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.mutable.HashMap[String,Int]](xml)
        .mustEqual(Some(collection.mutable.HashMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.mutable.WeakHashMap[String,Int]](xml)
        .mustEqual(Some(collection.mutable.WeakHashMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.mutable.LinkedHashMap[String,Int]](xml)
        .mustEqual(Some(collection.mutable.LinkedHashMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.mutable.OpenHashMap[String,Int]](xml)
        .mustEqual(Some(collection.mutable.OpenHashMap(
          "name1" -> 1, "name2" -> 2)))
      fromXml[collection.mutable.ListMap[String,Int]](xml)
        .mustEqual(Some(collection.mutable.ListMap(
          "name1" -> 1, "name2" -> 2)))
    }

    "support conversion of field casing while extracting XML" in {
      val xml = wrapXml(mkMap(Map("NameOne" -> Map("NamePartTwo" -> 2),
        "namePart3" -> null)))

      // Default is IgnoreCasing
      fromXml[Any](
          wrapXml(mkMap(Map("NameOne" -> Map("NamePartTwo" -> 2),
            "NamePart3" -> null))))
        .mustEqual(Some(Map(
          'NameOne -> Map('NamePartTwo -> 2), 'NamePart3 -> null)))
      fromXml[Any](
          wrapXml(mkMap(Map("NameOne" -> Map("NamePartTwo" -> 2),
            "NamePart3" -> null))),
        LowerCamelCase).mustEqual(Some(Map(
          'nameOne -> Map('namePartTwo -> 2),
          'namePart3 -> null)))
      fromXml[Any](xml,
        UpperCamelCase).mustEqual(Some(Map(
          'NameOne -> Map('NamePartTwo -> 2), 
          'NamePart3 -> null)))
      fromXml[Any](xml,
        LowerSnakeCase).mustEqual(Some(Map(
          'name_one -> Map('name_part_two -> 2), 
          'name_part_3 -> null)))
      fromXml[Any](xml,
        UpperSnakeCase).mustEqual(Some(Map(
          'NAME_ONE -> Map('NAME_PART_TWO -> 2), 
          'NAME_PART_3 -> null)))
      fromXml[Any](xml,
        LowerCase).mustEqual(Some(Map(
          'nameone -> Map('nameparttwo -> 2), 
          'namepart3 -> null)))
      fromXml[Any](xml,
        UpperCase).mustEqual(Some(Map(
          'NAMEONE -> Map('NAMEPARTTWO -> 2), 
          'NAMEPART3 -> null)))
      fromXml[Any](xml,
        LowerDashCase).mustEqual(Some(Map(
          Symbol("name-one") -> Map(Symbol("name-part-two") -> 2), 
          Symbol("name-part-3") -> null)))
      fromXml[Any](xml,
        UpperDashCase).mustEqual(Some(Map(
          Symbol("Name-One") -> Map(Symbol("Name-Part-Two") -> 2), 
          Symbol("Name-Part-3") -> null)))
      fromXml[Any](xml,
        IgnoreCasing).mustEqual(Some(Map(
          'NameOne -> Map('NamePartTwo -> 2), 
          'namePart3 -> null))) 
    }
  }

  "The toXml function" should {
    "support conversion from primitives" in {
      toXml("foo bar").mustEqual(wrapXml("foo bar"))
      toXml(3).mustEqual(wrapXml("3"))
      toXml(3: Short).mustEqual(wrapXml("3"))
      toXml(3L).mustEqual(wrapXml("3"))
      toXml(1.0f).mustEqual(wrapXml("1.0"))
      toXml(1.0).mustEqual(wrapXml("1.0"))
      toXml(true).mustEqual(wrapXml("true"))
      toXml(false).mustEqual(wrapXml("false"))
      toXml('a').mustEqual(wrapXml("a"))
      toXml(3: Byte).mustEqual(wrapXml("3"))
      toXml(BigInt("12341235123312")).mustEqual(wrapXml("12341235123312"))
      toXml(BigDecimal("12341235123312")).mustEqual(wrapXml("12341235123312"))
      val formatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
      toXml(formatter.parse("2012-01-01T20:01:01Z")).mustEqual(
        wrapXml("2012-01-01T20:01:01Z"))
      toXml(java.util.TimeZone.getTimeZone("PST")).mustEqual(wrapXml("PST"))
    }

    "support proper encoding of string data" in {
      toXml("foo\"bar").mustEqual(wrapXml("foo&quot;bar"))
      toXml("foo\nbar").mustEqual(wrapXml("foo\nbar"))
    }

    "support conversion from Lists" in {
      toXml(List("foo", "bar")).mustEqual(wrapXml(mkList(List("foo","bar"))))
      toXml(List(1,2,3)).mustEqual(wrapXml(mkList(List(1,2,3))))
    }

    "support conversion from Tuples" in {
      toXml('x -> 1).mustEqual(wrapXml(mkList(List("x",1))))
      toXml(Tuple3("x",1,true)).mustEqual(wrapXml(mkList(List("x",1,true))))
      toXml(List('x -> 1, 'y -> 2)).mustEqual(
        wrapXml(mkList(List(mkList(List("x",1)),mkList(List("y",2))))))
      toXml(Map('x -> ('y -> 2))).mustEqual(wrapXml(mkMap(
        Map("x" -> mkList(List("y", 2))))))
    }

    "support conversion from Maps" in {
      toXml(Map("foo" -> "bar")).mustEqual(wrapXml(mkMap(Map("foo" -> "bar"))))
      toXml(Map("foo" -> 3)).mustEqual(wrapXml(mkMap(Map("foo" -> 3))))
      toXml(Map("foo" -> List(1,2,3))).mustEqual(wrapXml(mkMap(
        Map("foo" -> List(1,2,3)))))
      toXml(Map("foo" -> Map("bar" -> 3))).mustEqual(
        wrapXml(mkMap(Map("foo" -> Map("bar" -> 3)))))
    }

    "support conversion using Option types" in {
      toXml(None).mustEqual("<result xsi:nil=\"true\"/>")
      toXml(Some("foo")).mustEqual(wrapXml("foo"))
      toXml(Some(123)).mustEqual(wrapXml("123"))
      toXml(Some(List(1,2))).mustEqual(wrapXml(mkList(List(1,2))))
      toXml(List(Some(1),Some(2),None)).mustEqual(
        wrapXml(mkList(List(1,2))+"<item xsi:nil=\"true\"/>"))
      toXml(Map("foo" -> Some(2), "bar" -> None)).mustEqual(
        wrapXml(mkMap(Map("foo" -> 2, "bar" -> null))))
      toXml(Test12("foo", None)).mustEqual(wrapXml(Test12("foo", None).toXml))
      toXml(Test12("foo", Some(1))).mustEqual(
        wrapXml(Test12("foo", Some(1)).toXml))
    }

    "support conversion using reflection" in {
      toXml(Test1("foo")).mustEqual(wrapXml(Test1("foo").toXml))
      toXml(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)).mustEqual(
        wrapXml(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3).toXml))
    }

    "support conversion using List of objects from reflection" in {
      toXml(List(Test1("foo"), Test1("bar"))).mustEqual(
        wrapXml(mkList(List(Test1("foo").toXml, Test1("bar").toXml))))
    }

    "support conversion using Maps of objects from reflection" in {
      toXml(Map(10 -> Test1("foo"), 15 -> Test1("bar"))).mustEqual(
        wrapXml(mkMap(Map(10 -> Test1("foo").toXml, 15 -> Test1("bar").toXml))))
    }

    "support conversion of Enumerations/singletons" in {
      case object Foo
      toXml(Foo).mustEqual(wrapXml("Foo"))
      toXml(List(WeekDay.Mon, WeekDay.Wed)).mustEqual(
        wrapXml(mkList(List("Mon","Wed"))))
      toXml(Test13(WeekDay.Mon)).mustEqual(wrapXml(mkMap(Map("d" -> "Mon"))))
    }

    "support conversion using reflection with embedded List types" in {
      // Only Lists of type String, Int (Long), Double, and Boolean possible
      toXml(Test3(List("foo", "bar"))).mustEqual(
        wrapXml(Test3(List("foo", "bar")).toXml))
      toXml(Test4(List("foo", "bar"), List(1, 2))).mustEqual(
        wrapXml(Test4(List("foo", "bar"), List(1, 2)).toXml))
      // This only works in 'to' direction, not reverse
      toXml(Test5(List(Test1("foo"),Test1("bar")))).mustEqual(
        wrapXml(Test5(List(Test1("foo"),Test1("bar"))).toXml))
    }

    "support conversion using reflection with embedded Map types" in {
      // Only Maps of String to String, Int (Long), Double, and Boolean possible
      toXml(Test6(Map("foo" -> "bar"))).mustEqual(
        wrapXml(Test6(Map("foo" -> "bar")).toXml))
      toXml(Test7(Map("foo" -> "bar"), Map("bat" -> 2))).mustEqual(
        wrapXml(Test7(Map("foo" -> "bar"), Map("bat" -> 2)).toXml))
    }

    "support conversion using reflection with embedded objects" in {
      val t = Test8( Test1("foo"), 
          Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3))
      toXml(t).mustEqual(wrapXml(t.toXml))
    }

    "support conversion with null values in reflection" in {
      toXml(List(Test1("foo"), null : Test2)).mustEqual(
        wrapXml(mkList(List(Test1("foo").toXml, null))))
    }

    "support conversion using reflection with non-case class types" in {
      toXml(new Test9("foo", 2)).mustEqual(wrapXml(new Test9("foo", 2).toXml))
    }

    "support conversion using reflection with readonly params" in {
      toXml(new Test10("foo", 2)).mustEqual(wrapXml(new Test10("foo", 2).toXml))
      // This tests setting a field after construction
      val test11 = new Test11("foo")
      test11.b = true
      toXml(test11).mustEqual(wrapXml(test11.toXml))
    }

    "support conversion using Streams" in {
      toXml(collection.immutable.Stream(1,2,3)).mustEqual(
        wrapXml(mkList(List(1,2,3))))
    }

    "support conversion using any iterable/iterator type" in {
      val iterable: Iterable[_] = List(1,2,3)
      toXml(iterable).mustEqual(wrapXml(mkList(List(1,2,3))))
      val iterator: Iterator[_] = List(1,2,3).iterator
      toXml(iterator).mustEqual(wrapXml(mkList(List(1,2,3))))
    }

    "support conversion using other seq types" in {
      // May need to revisit these, order is not always guarnateed
      toXml(Vector(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(Seq(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(IndexedSeq(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.immutable.LinearSeq(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.immutable.HashSet(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.immutable.SortedSet(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.immutable.TreeSet(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.immutable.ListSet(1,2,3))
        .mustEqual(wrapXml(mkList(List(3,2,1))))
      toXml(Set(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.immutable.Stack(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.immutable.Queue(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.ListBuffer(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.PriorityQueue(1,2,3))
        .mustEqual(wrapXml(mkList(List(3,1,2))))
      toXml(collection.mutable.Queue(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.HashSet(1,2,3))
        .mustEqual(wrapXml(mkList(List(2,1,3))))
      toXml(collection.mutable.LinkedHashSet(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.Set(1,2,3))
        .mustEqual(wrapXml(mkList(List(2,1,3))))
      toXml(collection.mutable.ArrayBuffer(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.ResizableArray(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.ArrayStack(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.Stack(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.LinkedList(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.DoubleLinkedList(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.MutableList(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.ArraySeq(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.IndexedSeq(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.LinearSeq(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.Seq(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.UnrolledBuffer(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
      toXml(collection.mutable.Buffer(1,2,3))
        .mustEqual(wrapXml(mkList(List(1,2,3))))
    }

    "support conversion using other map types" in {
      toXml(collection.immutable.HashMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name2" -> 2, "name1" -> 1))))
      toXml(collection.immutable.TreeMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
      toXml(collection.immutable.SortedMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
      toXml(collection.immutable.ListMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
      toXml(collection.mutable.Map("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name2" -> 2, "name1" -> 1))))
      toXml(collection.mutable.HashMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name2" -> 2, "name1" -> 1))))
      toXml(collection.mutable.WeakHashMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name2" -> 2, "name1" -> 1))))
      toXml(collection.mutable.LinkedHashMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
      toXml(collection.mutable.OpenHashMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name1" -> 1, "name2" -> 2))))
      toXml(collection.mutable.ListMap("name1" -> 1, "name2" -> 2))
        .mustEqual(wrapXml(mkMap(Map("name2" -> 2, "name1" -> 1))))
    }

    "support field casing conversion" in {
      toXml(Map("SomeName" -> 1), LowerCamelCase).mustEqual(
        wrapXml(mkMap(Map("someName" -> 1))))
      toXml(Map("SomeName" -> 1), LowerSnakeCase).mustEqual(
        wrapXml(mkMap(Map("some_name" -> 1))))
    }

    "support pretty printing" in {
      toXml(Map("SomeName" -> 1), true).mustEqual(
        "<result>\n  <SomeName>1</SomeName>\n</result>")
      toXml(Map("foo" -> Map("bar" -> 3)), true).mustEqual(
        "<result>\n  <foo>\n    <bar>3</bar>\n  </foo>\n</result>")
      toXml(Map("foo" -> List(1,2,3)), true).mustEqual(
        "<result>\n  <foo><item>1</item> <item>2</item> <item>3</item></foo>\n</result>")
      toXml(Map("foo" -> Map("bar" -> List(1,2,3))), true).mustEqual(
        "<result>\n  <foo>\n    <bar><item>1</item> <item>2</item> <item>3</item></bar>\n  </foo>\n</result>")
      toXml(List(Map("foo" -> 1), Map("bar" -> 2)), true).mustEqual(
        "<result><item>\n  <foo>1</foo>\n</item> <item>\n  <bar>2</bar>\n</item></result>")
    }
  } 

  "The xmlIterator function" should {
    "support iteration for any type" in {
      val iterator = xmlIterator[Int](
        wrapXml(mkList(List(1,2,3)))).map(_ + 1)
      var data = List[Int]()
      while (iterator.hasNext) data :+= iterator.next()
      data.mustEqual(List(2,3,4))
    }
  }

  "The Xml object" should {
    "support extraction operations" in {
      ("<result>foo</result>" match {
        case Xml(s) => s 
        case _ => "no match"
      }).mustEqual("foo")
      ("<result><foo>1</foo></result>" match {
        case Xml(xm) => xm 
        case _ => "no match"
      }).mustEqual(Map('foo -> 1))
    }
  }

  "The xml package" should {
    "support pimping any type to support using withXml" in {
      "foo".withXml(wrapXml("bar")) { _ + _ }.mustEqual("foobar")
      1.withXml(wrapXml("2")) { _ + _ }.mustEqual(3)
      1L.withXml(wrapXml("9223372036854775806")) { _ + _ }
        .mustEqual(9223372036854775807L)
      1.0.withXml(wrapXml("2.0")) { _ + _ }.mustEqual(3.0)
      true.withXml(wrapXml("false")) { _ || _ }.mustEqual(true)
      true.withXml(wrapXml("false")) { _ && _ }.mustEqual(false)
      List(1,2).withXml(wrapXml(mkList(List(3,4)))) { _ ++ _ }
        .mustEqual(List(1,2,3,4))
      Map("foo" -> "bar").withXml(wrapXml(mkMap(Map("foo2" -> "bar2")))) {
          _ ++ _ }.mustEqual(Map("foo" -> "bar", "foo2" -> "bar2"))
    }
  } 

  def wrapXml(xml: String) = "<result>" + xml + "</result>"

  def mkList[A](xs: Seq[A]): String = {
    (for (x <- xs) yield {
      if (x == null) "<item xsi:nil=\"true\"/>"
      else {
        val value = 
          if (x.isInstanceOf[Seq[_]]) mkList(x.asInstanceOf[Seq[Any]])
          else if (x.isInstanceOf[Map[_,_]]) mkMap(x.asInstanceOf[Map[Any,Any]])
          else x.toString
        "<item>" + value + "</item>" 
      }
    }).mkString
  }

  def mkMap[A,B](xm: Map[A,B]): String = {
    val buf = new StringBuilder
    for ((k,v) <- xm) {
      val openTag = 
        if (k.isInstanceOf[Symbol]) k.asInstanceOf[Symbol].name
        else if (k.isInstanceOf[String] || k.isInstanceOf[Char] || 
          k.isInstanceOf[Boolean]) k.toString
        else "__escaped__ __value__=\"" + k.toString + "\""

      val closeTag = 
        if (k.isInstanceOf[Symbol]) k.asInstanceOf[Symbol].name
        else if (k.isInstanceOf[String] || k.isInstanceOf[Char] || 
          k.isInstanceOf[Boolean]) k.toString
        else "__escaped__"


      if (v == null) buf.append("<" + openTag + " xsi:nil=\"true\"/>")
      else {
        val value = 
          if (v.isInstanceOf[Seq[_]]) mkList(v.asInstanceOf[Seq[Any]])
          else if (v.isInstanceOf[Map[_,_]]) mkMap(v.asInstanceOf[Map[Any,Any]])
          else v.toString
        buf.append("<" + openTag + ">" + value + "</" + closeTag + ">")
      }
    }
    buf.toString
  }

  // Test Classes
  case class Test1(s: String) {
    def toXml(): String = {
      <s>{s}</s>.toString
    }
  }
  case class Test2(str: String, num: Int, sht: Short, lng: Long, flt: Float, dbl: Double, ch: Char, bool: Boolean, byt: Byte) {
    def toXml(): String = {
      <str>{str}</str>
      <num>{num}</num>
      <sht>{sht}</sht>
      <lng>{lng}</lng>
      <flt>{flt}</flt>
      <dbl>{dbl}</dbl>
      <ch>{ch}</ch>
      <bool>{bool}</bool>
      <byt>{byt}</byt>.mkString
    }
  }

  case class Test3(xs: List[String]) {
    def toXml(): String = "<xs>" + mkList(xs) + "</xs>"
  }
  case class Test4(xs1: List[String], xs2: List[Int]) {
    def toXml(): String = 
      "<xs1>" + mkList(xs1) + "</xs1><xs2>" + mkList(xs2) + "</xs2>"
  }
  case class Test5(xs: List[Test1]) {
    def toXml(): String = {
      val buf = new StringBuilder
      buf.append("<xs>")
      for (x <- xs) buf.append("<item>" + x.toXml() + "</item>")
      buf.append("</xs>")
      buf.toString
    }
  }
  case class Test6(xm: Map[String, String]) {
    def toXml(): String = "<xm>" + mkMap(xm) + "</xm>"
  }
  case class Test7(xm1: Map[String, String], xm2: Map[String, Int]) {
    def toXml(): String = 
      "<xm1>" + mkMap(xm1) + "</xm1><xm2>" + mkMap(xm2) + "</xm2>"
  }

  case class Test8(t: Test1, t2: Test2) {
    def toXml(): String =
      "<t>" + t.toXml() + "</t><t2>" + t2.toXml() + "</t2>"
  }
  class Test9(val s: String, var i: Int) {
    override def equals(that: Any): Boolean = {
      if (that == null || !that.isInstanceOf[Test9]) return false
      val o = that.asInstanceOf[Test9]
      o.s == s && o.i == i
    }

    def toXml(): String = {
      <s>{s}</s>
      <i>{i}</i>.mkString
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

    def toXml(): String = {
      <s>{s}</s>
      <i>{i}</i>.mkString
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

    def toXml(): String = { 
      <s>{s}</s>
      <l>{l}</l>
      <b>{b}</b>.mkString
    }
  }
  case class Test12(val s: String, val i: Option[Int]) {
    def toXml(): String = {
      if (i == None) {
        "<s>" + s.toString + "</s>" +
        "<i xsi:nil=\"true\"/>"
      } else {
        <s>{s}</s>
        <i>{i.get}</i>.mkString
      }
    }
  }

  trait Singleton
  case object CaseObject extends Singleton
  object WeekDay extends Enumeration {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }
  case class Test13(d: WeekDay.Value)

  // Type hints for test classes
  object TestData {
    val typeHints = TypeHintSettings(
      List[Enumeration](WeekDay),
      Map[Class[_], Map[Symbol, Manifest[_]]](
        classOf[Test3] -> Map('xs -> manifest[List[String]]),
        classOf[Test4] -> Map('xs1 -> manifest[List[String]], 
            'xs2 -> manifest[List[Int]]),
        classOf[Test5] -> Map('xs -> manifest[List[Test1]]),
        classOf[Test6] -> Map('xm -> manifest[Map[String,String]]),
        classOf[Test7] -> Map('xm1 -> manifest[Map[String,String]], 
            'xm2 -> manifest[Map[String,Int]]),
        classOf[Test12] -> Map('i -> manifest[Option[Int]]) ))
  }
}
