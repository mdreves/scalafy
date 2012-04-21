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
package test.scalafy.collection.uniform

import java.util.Date
import java.util.TimeZone

import org.specs2.mutable.Specification

import scalafy.collection.uniform._
import scalafy.types.meta._
import scalafy.util.converters.ConversionSettings

/** Test specification for uniform types package */
object UniformTypesSpec extends Specification {

  "The fromUniformPrimitive function" should {
    "support conversion from UniformPrimitive types" in {
      fromUniformPrimitive(UniformString("foo")).mustEqual(Some("foo"))
      fromUniformPrimitive(UniformSymbol('foo)).mustEqual(Some('foo))
      fromUniformPrimitive(UniformInt(1)).mustEqual(Some(1))
      fromUniformPrimitive(UniformShort(1)).mustEqual(Some(1))
      fromUniformPrimitive(UniformLong(1L)).mustEqual(Some(1L))
      fromUniformPrimitive(UniformFloat(1.0f)).mustEqual(Some(1.0f))
      fromUniformPrimitive(UniformDouble(1.0d)).mustEqual(Some(1.0d))
      fromUniformPrimitive(UniformBoolean(true)).mustEqual(Some(true))
      fromUniformPrimitive(UniformChar('a')).mustEqual(Some('a'))
      fromUniformPrimitive(UniformByte(0)).mustEqual(Some(0))
      fromUniformPrimitive(UniformBigInt(BigInt("1234")))
        .mustEqual(Some(BigInt("1234")))
      fromUniformPrimitive(UniformBigDecimal(BigDecimal("1234")))
        .mustEqual(Some(BigDecimal("1234")))
      val d = new Date
      fromUniformPrimitive(UniformDate(d)).mustEqual(Some(d))
      fromUniformPrimitive(UniformTimeZone(TimeZone.getTimeZone("PST")))
        .mustEqual(Some(TimeZone.getTimeZone("PST")))
      
    }
  }

  "The fromUniformList function" should {
    "support conversion from UniformList to List" in {
      fromUniformList[List[String]](UniformList("foo", 2))
        .mustEqual(Some(List("foo", "2")))

      // Different types
      fromUniformList[Vector[Int]](UniformList("1", 2))
        .mustEqual(Some(Vector(1,2)))

      fromUniformList[List[List[Int]]](
          UniformList(UniformList(1, 2), UniformList(3,4)))
        .mustEqual(Some(List(List(1, 2), List(3, 4))))
    }

    "support conversion from UniformList to Tuple" in {
      fromUniformList[Tuple2[String,Int]](UniformList("foo", 2))
        .mustEqual(Some("foo" -> 2))

      fromUniformList[List[Tuple2[Int,Int]]](
          UniformList(UniformList(1, 2), UniformList(3,4)))
        .mustEqual(Some(List(1 -> 2, 3 -> 4)))
    }
  }

  "The fromUniformMap function" should {
    "support conversion from UniformMap to Map" in {
      fromUniformMap[Map[Symbol,Any]](UniformMap('test -> "foo", 'test2 -> 2))
        .mustEqual(Some(Map('test -> "foo", 'test2 -> 2)))
      fromUniformMap[Map[String,String]](
          UniformMap('test -> "foo", 'test2 -> 2))
        .mustEqual(Some(Map("test" -> "foo", "test2" -> "2")))
      fromUniformMap[Map[String,Boolean]](
          UniformMap('test -> "true", 'test2 -> "false"))
        .mustEqual(Some(Map("test" -> true, "test2" -> false)))
    }

    "support conversion from UniformMap to objects" in {
      fromUniformMap[Test1](UniformMap('s -> "foo"))
         .mustEqual(Some(Test1("foo")))

      fromUniformMap[Test2](UniformMap('str -> "bar", 'num -> 1, 'sht -> 2,
          'lng -> 3, 'flt -> 1.0, 'dbl -> 2.0, 'ch -> 'a', 'bool -> true,
          'byt -> 3))
        .mustEqual(Some(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))

      fromUniformMap[Test3](
          UniformMap('xs -> UniformList("foo", "bar", "baz")))
        .mustEqual(Some(Test3(List("foo", "bar", "baz"))))

      fromUniformMap[Test4](UniformMap('xs1 -> UniformList("foo", "bar"), 
          'xs2 -> UniformList(1,2,3)))
        .mustEqual(Some(Test4(List("foo", "bar"), List(1,2,3))))

      // Won't work due to type erasure 
      //val t5 = toUniformMap(List(Test1("test"), Test1("test2")))
      //fromUniformMap[Test5](UniformMap('xs -> t5))
      //  .mustEqual(Some(Test5(List(Test1("test"), Test1("test2")))))
      
      fromUniformMap[Test6](
          UniformMap('xm -> UniformMap('a -> "b", 'c -> "d")))
        .mustEqual(Some(Test6(Map('a -> "b", 'c -> "d"))))
      
      fromUniformMap[Test7](UniformMap('xm1 -> UniformMap('a -> "b"), 
          'xm2 -> UniformMap('c -> 2)))
        .mustEqual(Some(Test7(UniformMap('a -> "b"), UniformMap('c -> 2))))
  
      val t81 = toUniformMap(Test1("foo")).get
      val t82 = toUniformMap(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)).get
      fromUniformMap[Test8](UniformMap('t -> t81, 't2 -> t82))
        .mustEqual(Some(Test8(Test1("foo"), 
            Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3))))
      
      fromUniformMap[Test9](UniformMap('s -> "foo", 'i -> 3)).mustEqual(
        Some(new Test9("foo", 3)))

      fromUniformMap[Test10](UniformMap('s -> "bar"))
        .mustEqual(Some(new Test10("bar", 1)))
      fromUniformMap[Test10](UniformMap('i -> 3))
        .mustEqual(Some(new Test10("foo", 3)))
     
      val t11 = new Test11("foo")
      fromUniformMap[Test11](UniformMap('s -> "foo", 'l -> 4L, 'b -> false))
        .mustEqual(Some(t11))
      t11.b = true
      fromUniformMap[Test11](UniformMap('s -> "foo", 'l -> 4L, 'b -> true))
        .mustEqual(Some(t11))

      // From within other types (list, tuple, ...)
      fromUniformList[List[Test1]](UniformList(UniformMap('s -> "foo")))
        .mustEqual(Some(List(Test1("foo"))))
      fromUniformList[Tuple2[Test1, Test1]](
          UniformList(UniformMap('s -> "foo"), UniformMap('s -> "bar")))
        .mustEqual(Some(Test1("foo") -> Test1("bar")))

      val t1 = toUniformMap(Test1("foo")).get
      val t2 = toUniformMap(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)).get
      
      // from objects
      fromUniformMap[Map[Symbol, Any]](t1).mustEqual(Some(Map('s -> "foo")))
      fromUniformMap[Map[Symbol, String]](t1).mustEqual(Some(Map('s -> "foo")))
      fromUniformMap[Map[Symbol, Any]](t2)
        .mustEqual(Some(Map('str -> "bar", 'num -> 1, 'sht -> 2, 
          'lng -> 3, 'flt -> 1.0, 'dbl -> 2.0, 'ch -> 'a', 'bool -> true,
          'byt -> 3)))

      // Support filtering for fields that match type
      fromUniformMap[Map[Symbol, String]](t2.filter[String])
        .mustEqual(Some(Map('str -> "bar")))
      fromUniformMap[Map[String, Int]](t2.filter[Int])
        .mustEqual(Some(Map("num" -> 1)))

      // Maps/Lists mixed with objects 
      fromUniformList[List[Test10]](UniformList(UniformMap('i -> "1")))
        .mustEqual(Some(List(new Test10("foo",1))))
      fromUniformMap[Map[Symbol,Test10]](
          UniformMap('t -> UniformMap('i -> "1")))
        .mustEqual(Some(Map('t -> new Test10("foo",1))))
      fromUniformList[Tuple2[Test1,Test1]](
          UniformList(UniformMap('s -> "foo"), UniformMap('s -> "bar")))
        .mustEqual(Some(Test1("foo") -> Test1("bar")))

      // Between objects
      val t9 = toUniformMap(new Test9("foo", 3)).get
      val t10 = toUniformMap(new Test10("foo")).get
      fromUniformMap[Test10](t9).mustEqual(Some(new Test10("foo", 3)))
      fromUniformMap[Test9](t10).mustEqual(Some(new Test9("foo", 1)))
    }
  }

  "The toUniformPrimitive function" should {
    "support conversion to UniformPrimitive types" in {
      toUniformPrimitive("foo").mustEqual(UniformString("foo"))
      toUniformPrimitive('foo).mustEqual(UniformSymbol('foo))
      toUniformPrimitive(1).mustEqual(UniformInt(1))
      toUniformPrimitive(1.toShort).mustEqual(UniformShort(1))
      toUniformPrimitive(1L).mustEqual(UniformLong(1L))
      toUniformPrimitive(1.0f).mustEqual(UniformFloat(1.0f))
      toUniformPrimitive(1.0).mustEqual(UniformDouble(1.0d))
      toUniformPrimitive(true).mustEqual(UniformBoolean(true))
      toUniformPrimitive('a').mustEqual(UniformChar('a'))
      toUniformPrimitive(0.toByte).mustEqual(UniformByte(0))
      toUniformPrimitive(BigInt("1234"))
        .mustEqual(UniformBigInt(BigInt("1234")))
      toUniformPrimitive(BigDecimal("1234"))
        .mustEqual(UniformBigDecimal(BigDecimal("1234")))
      val d = new Date
      toUniformPrimitive(d).mustEqual(UniformDate(d))
      toUniformPrimitive(TimeZone.getTimeZone("PST"))
        .mustEqual(UniformTimeZone(TimeZone.getTimeZone("PST")))
    }
  }

  "The toUniformList function" should {
    "support conversion to UniformList from List types" in {
      toUniformList(List("foo")).mustEqual(Some(UniformList("foo")))
      toUniformList(List(Map('t -> "foo"))).mustEqual(
        Some(UniformList(UniformMap('t -> "foo"))))
    }

    "support conversion to UniformList from Tuple types" in {
      toUniformList("foo" -> 2).mustEqual(Some(UniformList("foo", 2)))
      toUniformList(List('t -> "foo", "test" -> true)).mustEqual(
        Some(UniformList(UniformList('t, "foo"), UniformList("test", true))))
    }
  }

  "The toUniformMap function" should {
    "support conversion to UniformMap from Map types" in {
      toUniformMap(Map('test -> "foo")).mustEqual(
        Some(UniformMap('test -> "foo")))
      toUniformMap(Map("test" -> "foo")).mustEqual(
        Some(UniformMap('test -> "foo")))
      toUniformMap(Map("test" -> List(true, false)))
        .mustEqual(Some(UniformMap('test -> UniformList(true, false))))
      toUniformMap(Map("test" -> List(1, 1.0)))
        .mustEqual(Some(UniformMap('test -> UniformList(1, 1.0))))
    }

    "support conversion to UniformMap from object types" in {
      toUniformMap(Test1("foo")).mustEqual(Some(UniformMap('s -> "foo")))

      toUniformMap(Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3))
        .mustEqual(Some(UniformMap('str -> "bar", 'num -> 1, 'sht -> 2,
          'lng -> 3, 'flt -> 1.0, 'dbl -> 2.0, 'ch -> 'a', 'bool -> true,
          'byt -> 3)))

      toUniformMap(Test3(List("foo", "bar", "baz")))
         .mustEqual(Some(UniformMap('xs -> UniformList("foo", "bar", "baz"))))

      toUniformMap(Test4(List("foo", "bar"), List(1,2,3)))
        .mustEqual(Some(UniformMap('xs1 -> UniformList("foo", "bar"), 
          'xs2 -> UniformList(1,2,3))))

      // NOTE: Can't convert back due to type erasure ...
      toUniformList(List(Test1("test"), Test1("test2"))).mustEqual(
        Some(UniformList(UniformMap('s -> "test"), UniformMap('s -> "test2"))))
      
      toUniformMap(Test6(Map('a -> "b", 'c -> "d"))).mustEqual(
          Some(UniformMap('xm -> UniformMap('a -> "b", 'c -> "d"))))
      
      toUniformMap(Test7(UniformMap('a -> "b"), UniformMap('c -> 2))).mustEqual(
        Some(UniformMap('xm1 -> UniformMap('a -> "b"), 
          'xm2 -> UniformMap('c -> 2))))
  
      toUniformMap(Test8(Test1("foo"), 
          Test2("bar", 1, 2, 3, 1.0f, 2.0, 'a', true, 3)))
        .mustEqual(
          Some((UniformMap(
            't -> UniformMap('s -> "foo"), 
            't2 -> UniformMap('str -> "bar", 'num -> 1, 'sht -> 2,
              'lng -> 3, 'flt -> 1.0, 'dbl -> 2.0, 'ch -> 'a', 'bool -> true,
              'byt -> 3)))))
      
      toUniformMap(new Test9("foo", 3)).mustEqual(
        Some(UniformMap('s -> "foo", 'i -> 3)))

      toUniformMap(new Test10("bar")).mustEqual(
        Some((UniformMap('s -> "bar", 'i -> 1))))
      toUniformMap(new Test10(3)).mustEqual(
        Some(UniformMap('s -> "foo", 'i -> 3)))

      val t11 = new Test11("foo")
      toUniformMap(t11).mustEqual(
        Some(UniformMap('s -> "foo", 'l -> 3L, 'b -> false)))
      t11.b = true
      toUniformMap(t11).mustEqual(
        Some(UniformMap('s -> "foo", 'l -> 3L, 'b -> true)))

      // From within other types (list, tuple, ...)
      toUniformList(List(Test1("foo"))).mustEqual(
        Some(UniformList(UniformMap('s -> "foo"))))
      toUniformList(Test1("foo") -> Test1("bar")).mustEqual(
        Some(UniformList(UniformMap('s -> "foo"), UniformMap('s -> "bar"))))

      // Maps/Lists mixed with objects 
      toUniformList(List(new Test10("foo",1))).mustEqual(
        Some(UniformList(UniformMap('s -> "foo", 'i -> 1))))
      toUniformMap(Map('t -> new Test10("foo",1))).mustEqual(
        Some(UniformMap('t -> UniformMap('s -> "foo", 'i -> 1))))
      toUniformList((Test1("foo") -> Test1("bar"))).mustEqual(
        Some(UniformList(UniformMap('s -> "foo"), UniformMap('s -> "bar"))))
    }

    "support conversions involving opaque data" in {
      implicit val testSettings = UniformDataSettings(
        ConversionSettings(false, OpaqueDataSettings(true))
      )

      val t1 = fromUniformMap[Test1](UniformMap('s -> "foo", 'i -> 1)).get
      OpaqueData.get(t1).mustEqual(Some(Map('i -> 1)))
      toUniformMap(t1).mustEqual(Some(UniformMap('s -> "foo", 'i -> 1)))

      val t2 = fromUniformMap[Map[String,Test1]](
        UniformMap('t1 -> UniformMap('s -> "foo", 'x -> 3),
          't2 -> UniformMap('s -> "bar", 'y -> 7))).get
      OpaqueData.get(t2("t1")).mustEqual(Some(Map('x -> 3)))
      OpaqueData.get(t2("t2")).mustEqual(Some(Map('y -> 7)))
      toUniformMap(t2).mustEqual(Some(
        UniformMap('t1 -> UniformMap('s -> "foo", 'x -> 3),
          't2 -> UniformMap('s -> "bar", 'y -> 7))))

      // Reset
      implicit val uniformDataSettings = UniformDataSettings(
        ConversionSettings(false, OpaqueDataSettings(false))
      )
      true.mustEqual(true)  // Keep specs happy
    }
  }


  "The UniformList type" should {

    "support filtering using filter" in {
      val xs = UniformList("foo", 2)
      xs.filter[String].mustEqual(UniformList("foo"))
      xs.filter[Int].mustEqual(UniformList(2))
    }

    "support getAsType method" in {
      val xs = UniformList("foo", 2)
      xs.getAsType[String](0).mustEqual(Some("foo"))
      xs.getAsType[Int](0).mustEqual(None)
      xs.getAsType[Int](1).mustEqual(Some(2))
      xs.getAsType[String](1).mustEqual(None)
      xs.getAsType[Any](0).mustEqual(Some("foo"))
    }

    "support withFilter[Type] method" in {
      val xs = UniformList("foo", 2)
      xs.withFilter[String].map(v => v).mustEqual(List("foo"))
      xs.withFilter[Int].map(_ + 1).mustEqual(List(3))
      xs.withFilter(v => v.isInstanceOf[String]).map(v => v)
        .mustEqual(List("foo"))
    }

    "support iteratorWithType[Type] method" in {
      val xs = UniformList("foo", 2)
      xs.iteratorWithType[String].next.mustEqual("foo")
      xs.iteratorWithType[Int].next.mustEqual(2)
    }

    "support operations using cons and UniformNil" in {
      ("foo" :: UniformNil).mustEqual(UniformList("foo"))
    }

    "support extraction" in {
      // NOTE: Can't use h :: t syntax, because can use implicits in pattern
      //       matching:
      // 
      //       http://scala-programming-language.1934581.n4.nabble.com/
      //         implicit-conversions-and-pattern-matching-td1986793.html 
      (UniformList("foo", 2) match {
        case UniformList(h, tail @ _*) => h.toString
        case _ => "no match"
      }).mustEqual("foo")
    }

    "support primitive types" in {
      UniformList("foo", "bar").mustEqual(UniformList("foo","bar"))
      UniformList('foo, 'bar).mustEqual(UniformList('foo,'bar))
      UniformList(1,2).mustEqual(UniformList(1,2))
      UniformList(1.toShort,2.toShort).mustEqual(
        UniformList(1.toShort,2.toShort))
      UniformList(1L,2L).mustEqual(UniformList(1L,2L))
      UniformList(1.0f,2.0f).mustEqual(UniformList(1.0f,2.0f))
      UniformList(1.0d,2.0d).mustEqual(UniformList(1.0d,2.0d))
      UniformList(true,false).mustEqual(UniformList(true,false))
      UniformList('a','b').mustEqual(UniformList('a','b'))
      UniformList(1.toByte, 2.toByte).mustEqual(UniformList(1.toByte,2.toByte))
      UniformList(BigInt("1234"), BigInt("34")).mustEqual(
        UniformList(BigInt("1234"), BigInt("34")))
      val d1 = new Date
      val d2 = new Date
      UniformList(d1, d2).mustEqual(UniformList(d1, d2))
    }

    "support embedded UniformLists and UniformMaps" in {
      UniformList(UniformList(1,2).filter[Int], 
        UniformList(3,4).filter[Int])
        .mustEqual(UniformList(UniformList(1, 2).filter[Int], 
                   UniformList(3,4).filter[Int]))


      UniformList(UniformList(1,"foo"), UniformList(3,4)).mustEqual(
        UniformList(UniformList(1, "foo"), UniformList(3,4)))

      UniformList(UniformList(UniformList(1,"foo"), UniformList(3,4)),
                  UniformList(UniformList(4,"bar")))
      .mustEqual(
        UniformList(UniformList(UniformList(1,"foo"), UniformList(3,4)),
                    UniformList(UniformList(4,"bar"))))

      UniformList(UniformList(UniformMap('test -> "foo"),
                              UniformMap('bar -> 4)),
                  UniformList(UniformMap('test3 -> "bar")))
      .mustEqual(
        UniformList(UniformList(UniformMap('test -> "foo"),
                                UniformMap('bar -> 4)),
                    UniformList(UniformMap('test3 -> "bar"))))
    }

    "support all List specific operations" in {
      val xs = UniformList("foo", 2)

      // apply
      xs(0).mustEqual("foo")
      xs(1).mustEqual(2)
      xs(3).must(throwA[IndexOutOfBoundsException])

      // isDefinedAt
      xs.isDefinedAt(0).mustEqual(true) 
      xs.isDefinedAt(3).mustEqual(false) 
 
      UniformList(1,2,3,4).::(5).mustEqual(UniformList(5,1,2,3,4)) 
      UniformList(1,2,3,4).:::(List(5, 6)).mustEqual(
        UniformList(5,6,1,2,3,4))

      UniformList(1,2,3).reverse.mustEqual(UniformList(3,2,1))
      UniformList(1,2,3).reverse_:::(List(5, 6)).mustEqual(
        UniformList(6,5,1,2,3))

      UniformList(1,2,3,4).filter[Int].segmentLength(
        x => x < 3, 1).mustEqual(1)

      // Misc
      xs.stringPrefix.mustEqual("UniformList")
      xs.length.mustEqual(2)
      xs.lengthCompare(2).mustEqual(0)
      xs.lengthCompare(3).mustEqual(-1)
      xs.toBuffer.mustEqual(
        collection.mutable.ArrayBuffer("foo", 2))

      // From List object
      UniformList.concat(List(1,2), List(3,4)).mustEqual(UniformList(1,2,3,4))

      UniformList.empty.mustEqual(UniformList())
      UniformList.fill(1,2,3)("t").mustEqual(
        UniformList(UniformList(
                      UniformList("t","t","t"),
                      UniformList("t","t","t"))))

      UniformList.iterate(1, 3)(x => x + 1).mustEqual(UniformList(1,2,3))

      UniformList.range(2, 6, 2).mustEqual(UniformList(2,4))

      UniformList.tabulate(1,2,3)((x,y,z) => x + y + z).mustEqual(
        UniformList(UniformList(UniformList(0,1,2), UniformList(1,2,3))))
    }

    "support Product trait" in {
      UniformList(1, 2, 3).productArity.mustEqual(2)
      UniformList(1, 2, 3).productElement(0).mustEqual(1)
      UniformList(1, 2, 3).productElement(1).mustEqual(UniformList(2,3))
      UniformList(1, 2, 3).productPrefix.mustEqual("UniformList")
      UniformList(1, 2, 3).productIterator.next.mustEqual(1)
    }

    "support Function1 and PartialFunction traits" in {
      List(0,1).map(UniformList("foo", "bar")).mustEqual(List("foo", "bar"))

      // Function1
      (UniformList(10, 20).compose { List(0, 1) })(1).mustEqual(20)

      // PartialFunction
      val f = UniformList(1, 2) andThen { i => i.toString}
      List(0, 1).map(f).mustEqual(List("1", "2"))
      
      UniformList(10, 20).lift(0).mustEqual(Some(10))
      
      val p = UniformList(1, 2).orElse{UniformList(1,2,3)}
      List(2).map(p).mustEqual(List(3))
    }

    "support Seq traits" in {
      val xs = UniformList("foo", 2)

      xs.+:(true).mustEqual(UniformList(true, "foo", 2))
      xs.:+(true).mustEqual(UniformList("foo", 2, true))

      // combinations
      UniformList(1,2,3,4).combinations(2).next.mustEqual(UniformList(1,2))

      // contains
      xs.contains("foo").mustEqual(true)
      xs.contains(true).mustEqual(false)
      UniformList(1,2,3,4).containsSlice(List(1,2)).mustEqual(true)
      UniformList(1,2,3,4).containsSlice(List(4,5)).mustEqual(false)
      UniformList(1,2,3,4).containsSlice(UniformList(1,2)).mustEqual(true)

      // corresponds
      UniformList(1,2,3,4).corresponds(
        List(1,2,3,4))((a,b) => a == b).mustEqual(true)
      UniformList(1,2,3,4).corresponds(
        UniformList(1,2,3,4))((a,b) => a == b).mustEqual(true)

      // diff
      UniformList(1,2,3,4).diff(UniformList(1,2)).mustEqual(UniformList(3,4))

      // distinct
      UniformList(1,1,3,3,4).distinct.mustEqual(UniformList(1,3,4))

      // endsWith/startsWith
      UniformList("foo", "bar", "bat").endsWith(UniformList("bar", "bat"))
        .mustEqual(true)
      UniformList("foo", "bar", "bat").startsWith(UniformList("foo", "bar"))
        .mustEqual(true)
      UniformList("foo", "bar", "bat").startsWith(UniformList("bar"), 1)
        .mustEqual(true)

      // indexOf
      xs.indexOf("foo").mustEqual(0)
      xs.indexOf("foo", 1).mustEqual(-1)
      xs.indexOf(2).mustEqual(1)
      UniformList("foo", 1, 2, "ba").indexOfSlice(UniformList(1,2)).mustEqual(1)
      UniformList("foo", 1, 2).indexOfSlice(UniformList(1,2), 2).mustEqual(-1)
      xs.indexWhere(x => x == 2).mustEqual(1)
      xs.indexWhere(x => x == 3).mustEqual(-1)
      UniformList(1,2,2,4).lastIndexOf(2).mustEqual(2)
      UniformList(1,2,2,4).lastIndexOf(2, 1).mustEqual(1)
      UniformList("foo", 1, 2, 1, 2).lastIndexOfSlice(UniformList(1,2))
        .mustEqual(3)
      UniformList("foo", 1, 2, 1, 2).lastIndexOfSlice(UniformList(1,2), 2)
        .mustEqual(1)

      // indicies
      xs.indices.mustEqual(Range(0,2))

      // intersect
      UniformList(1,2,3,4).intersect(UniformList(2,3))
        .mustEqual(UniformList(2,3))

      // iterator
      xs.iterator.next.mustEqual("foo")
      xs.reverseIterator.next.mustEqual(2)
 
      // padTo
      xs.padTo(4, "test").mustEqual(UniformList("foo", 2, "test", "test"))

      // patch
      UniformList(1,2,3,4,5,6).patch(2, UniformList(8,9), 3)
        .mustEqual(UniformList(1,2,8,9,6))

      // permutations
      UniformList('a','b','c').permutations.size.mustEqual(6)
      UniformList('a','b','c').permutations.next
        .mustEqual(UniformList('a','b','c'))

      // prefixLength
      UniformList(1, 2, 3, 4, 5).filter[Int].prefixLength(x => x < 3)
        .mustEqual(2) 

      // reverseMap
      UniformList(1, 2, 3, 4).filter[Int].reverseMap(x => x + 1).mustEqual(
        List(5, 4, 3, 2))

      // sortBy
      UniformList("2", "3", "1").filter[String].sortBy(
        x => x.toInt).mustEqual(UniformList("1", "2", "3"))
      UniformList(2, 3, 1).filter[Int].sortWith((a, b) => a < b).mustEqual(
        UniformList(1, 2, 3))
      UniformList(2, 3, 1).filter[Int].sorted.mustEqual(UniformList(1, 2, 3))
      
      // union
      UniformList(1, 2).union(UniformList(2,3).filter[Int])
        .mustEqual(UniformList(1,2,2,3))

      // updated
      UniformList(1, 2, 3).updated(1, 4).mustEqual(UniformList(1,4,3)) 
      
      // Misc
      xs.size.mustEqual(2)
      xs.toString.mustEqual("UniformList(foo, 2)")
    }

    "support Traversable/Iterable traits" in {
      val xs = UniformList("foo", 2)

      (xs ++ List("ba", "ca")).mustEqual(UniformList("foo", 2, "ba", "ca"))
      (xs ++ List("ba", "ca")).mustEqual(UniformList("foo", 2, "ba", "ca"))
      // Reverse (e.g. List/List ++ UniformList)
      (List("ba", "ca") ++ xs).mustEqual(List("ba", "ca", "foo", 2))
      (Map('test -> "ba", 'test2 -> "ca") ++ xs).mustEqual(
        List(('test -> "ba"), ('test2 -> "ca"), "foo", 2))

      (xs ++: List("ba", "ca")).mustEqual(List("foo", 2, "ba", "ca"))
      (xs ++: Map('test3 -> "ba", 'test4 -> "ca")).mustEqual(
        List("foo", 2, ('test3 -> "ba"), ('test4 -> "ca")))
      // Reverse (e.g. Map/List ++ UniformList)
      (List("ba", "ca") ++: xs).mustEqual(UniformList("ba", "ca", "foo", 2))

      // TODO : Need to decide if want to support UniformList of primitive pairs
      //(UniformMap('test3 -> "ba", 'test4 -> "ca") ++: xs).mustEqual(
      //  List("foo", 2, Map('test3 -> "ba", 'test4 -> "ca")))


      // Note: UniformList is like working with List[Any] so restricts
      //       usefulness of fold operations 
      
      // FoldLeft
      (("" /: xs) { case (s, v) => s + v.toString }).mustEqual("foo2")
      (xs.foldLeft("") { case (s, v) => s + v.toString }).mustEqual("foo2")
      (xs.filter[Int].foldLeft(2) { case (s, v) => s + v }).mustEqual(4)

      // FoldRight
      ((0 /: UniformList(2, 3, "foo").filter[Int]) { 
          (s, v) => s + v }).mustEqual(5)
      ((xs :\ "") { case (v, s) => s + v.toString }).mustEqual("2foo")
      (xs.foldRight("") { case (v, s) => s + v.toString }).mustEqual("2foo")
      (xs.filter[Int].foldRight(2) { case (v, s) => s + v }).mustEqual(4)

      // Parallel fold 
      ((xs /:\ "") { 
        case (v1, v2) => v1+v2.toString 
      }).mustEqual("foo2")
      (xs.fold("") { 
        case (v1, v2) => v1+v2.toString
      }).mustEqual("foo2")
      (xs.filter[Int].fold(0) { case (v1, v2) => v1+v2 }).mustEqual(2)

      // Collect
      (xs.collect { case v if (v == "foo") => v })
        .mustEqual(List("foo"))
      (xs.collectFirst { case v => v }).mustEqual(Some("foo"))

      // Copy 
      val xs1 = new Array[Any](2)
      xs.copyToArray(xs1)
      xs1.mustEqual(Array("foo", 2))
      val xs2 = new collection.mutable.ArrayBuffer[Any](2)
      xs.copyToBuffer(xs2)
      xs2.mustEqual(
        collection.mutable.ArrayBuffer("foo", 2))
      val xs3 = new Array[Int](2)
      UniformList(1, 2).filter[Int].copyToArray(xs3)
      xs3.mustEqual(Array(1, 2))

      // Count 
      (xs.count { v => v == 2 }).mustEqual(1)
      (xs.filter[Int].count { v => v > 1 }).mustEqual(1)

      // Drop
      xs.drop(1).mustEqual(UniformList(2))
      xs.dropRight(1).mustEqual(UniformList("foo"))
      xs.dropWhile(v => v == "foo").mustEqual(UniformList(2))

      // Exists
      xs.exists(v => v == "foo").mustEqual(true)
      xs.exists(v => v == "bar").mustEqual(false)

      // Filter
      xs.filter(v => v == 2).mustEqual(UniformList(2))
      xs.filter[Int].filter(v => v > 2).mustEqual(UniformList())
      
      // Find
      xs.find(v => v == "foo").mustEqual(Some("foo"))
      xs.find(v => v == "bar").mustEqual(None)
   
      // flatMap
      UniformList(1,2,3).filter[Int].flatMap(
        v => UniformList(v, 1)).mustEqual(List(1, 1, 2, 1, 3, 1))

      // forall
      UniformList("foo","bar").forall(v => v.isInstanceOf[Symbol])
        .mustEqual(false)
      UniformList("foo","bar").forall(v => v.isInstanceOf[String])
        .mustEqual(true)

      // foreach
      val strBuf = new StringBuilder()
      xs.foreach { v => strBuf.append(v) }
      strBuf.toString.mustEqual("foo2")

      // Group by
      (UniformList(1, 2, 3, 4).filter[Int].groupBy { v => v % 2 == 0 })
        .mustEqual(Map(true -> UniformList(2, 4),
                       false -> UniformList(1, 3)))

      // group
      UniformList(1, 2, 3, 4).grouped(2).next().mustEqual(UniformList(1, 2))

      // head
      xs.head.mustEqual("foo")
      xs.headOption.mustEqual(Some("foo"))

      // init
      UniformList(1, 2, 3).init.mustEqual(UniformList(1, 2))
      // Iterates over all, then all - last, ... 
      UniformList(1).inits.next.mustEqual(UniformList(1))

      // last
      xs.last.mustEqual((2))
      xs.lastOption.mustEqual(Some(2))
     
      // map 
      (xs.map { v => v + "test" }).mustEqual(List("footest", "2test"))

      // max/min
      UniformList(1,2,3,4).filter[Int].max.mustEqual(4)
      UniformList("1","2","3").filter[String].maxBy(
        x => x.toInt).mustEqual("3")
      UniformList(2,1,3,4).filter[Int].min.mustEqual(1)
      UniformList("2","1","3").filter[String].minBy(
        x => x.toInt).mustEqual("1")

      // partition
      (xs.partition { v => (v.isInstanceOf[String]) }).mustEqual(
        (UniformList("foo"), UniformList(2)))

      // product
      UniformList(1,2,3,4).filter[Int].product.mustEqual(24)
    
      // reduceLeft
      (UniformList(1, 2, 3, 4)
        .filter[Int].reduceLeft{ (v1, v2) => v1 + v2 })
        .mustEqual(10)
      (UniformList(1, 2, 3, 4)
        .filter[Int].reduceLeftOption{(v1,v2) => v1 + v2 })
        .mustEqual(Some(10))
      // reduceRight
      (UniformList(1, 2, 3, 4)
        .filter[Int].reduceRight{ (v2, v1) => v2 + v1 })
        .mustEqual(10)
      (UniformList(1, 2, 3, 4)
        .filter[Int].reduceRightOption{(v2,v1) => v2+v1 })
        .mustEqual(Some(10))
      // reduce
      (UniformList(1, 2, 3, 4)
        .filter[Int].reduce{ (v1, v2) => v1 + v2 })
        .mustEqual(10)
      (UniformList(1, 2, 3, 4)
        .filter[Int].reduceOption{ (v1, v2) => v1 + v2 })
        .mustEqual(Some(10))

      // sameElements
      xs.sameElements(List("foo", 2)).mustEqual(true)

      // scan
      (xs.scan(0) { (v, v2)  => v2 }).mustEqual(UniformList(0, "foo", 2))
      (xs.scanLeft(0 : Any) { (v, v2)  => v2 }).mustEqual(
        List(0, "foo", 2))
      (xs.scanRight(0 : Any) { (v, v2)  => v }).mustEqual(
        List("foo", 2, 0))

      // slice
      xs.slice(0,1).mustEqual(UniformList("foo"))

      // sliding
      val s = UniformList(1, 2, 3, 4).sliding(2, 2)
      s.next()
      s.next().mustEqual(UniformList(3, 4))
      UniformList(1, 2, 3, 4)
        .sliding(2).next().mustEqual(UniformList(1, 2))

      // span
      (xs.span { v => (v.isInstanceOf[String]) }).mustEqual(
        (UniformList("foo"), UniformList(2)))
     
      // sum
      UniformList(1,2,3,4).filter[Int].sum.mustEqual(10)

      // splitAt
      UniformList(1, 2, 3).splitAt(1).mustEqual(
        UniformList(1), UniformList(2, 3))

      // tail
      UniformList(1, 2, 3).tail.mustEqual(UniformList(2, 3))
      // Iterates over all, then all - last, ... 
      UniformList(1).tails.next.mustEqual(UniformList(1))

      // take
      xs.take(1).mustEqual(UniformList("foo"))
      xs.takeRight(1).mustEqual(UniformList(2))
      xs.takeWhile(v => v == "foo").mustEqual(UniformList("foo"))

      // toXxx
      xs.toArray.mustEqual(Array("foo", 2))
      xs.toIndexedSeq.mustEqual(IndexedSeq("foo", 2))
      xs.toIterable.iterator.next().mustEqual("foo")
      xs.toIterator.next().mustEqual("foo")
      xs.toList.mustEqual(List("foo", 2))
      xs.toList.mustEqual(collection.mutable.ListBuffer("foo", 2))
      xs.toSet.mustEqual(Set("foo", 2))
      xs.toStream.mustEqual(Stream("foo", 2))
      xs.toTraversable.mustEqual(List("foo", 2))

      // views/monadic filters
      xs.view.iterator.next.mustEqual("foo")
      xs.view(1,2).iterator.next.mustEqual(2)
      xs.withFilter(v => v.isInstanceOf[Int]).map(v => v)
        .mustEqual(List(2))

      // zip
      xs.zip(List(1, 2)).mustEqual(List("foo" -> 1, 2 -> 2))
      // NOTE: If decide to support UniformLists of pairs, then this can return 
      //       UniformList
      xs.zipWithIndex.mustEqual(List("foo" -> 0, 2 -> 1))

      // Misc 
      xs.addString(new StringBuilder(), "s", "-", "e").toString().mustEqual(
        "sfoo-2e")
      xs.aggregate("")( ((z, v) => z + v), ((z1, z2) => z1 + "-" + z2) )
        .mustEqual("foo2")
      xs.mkString("-").toString().mustEqual("foo-2")
      xs.size.mustEqual(2)
      xs.hasDefiniteSize.mustEqual(true)
      xs.isTraversableAgain.mustEqual(true)
      xs.nonEmpty.mustEqual(true)
    }
  }


  "The UniformMap type" should {

    "support filtering using filter" in {
      val xm = UniformMap('test -> "foo", 'test2 -> 2)
      xm.filter[String].mustEqual(UniformMap('test -> "foo"))
      xm.filter[Int].mustEqual(UniformMap('test2 -> 2))
    }

    "support getAsType method" in {
      val xm = UniformMap('test -> "foo", 'test2 -> 2)
      xm.getAsType[String]('test).mustEqual(Some("foo"))
      xm.getAsType[Int]('test).mustEqual(None)
      xm.getAsType[Int]('test2).mustEqual(Some(2))
      xm.getAsType[String]('test2).mustEqual(None)
      xm.getAsType[Any]('test).mustEqual(Some("foo"))
    } 

    "support withFilter[Type] method" in {
      val xm = UniformMap('test -> "foo", 'test2 -> 2)
      xm.withFilter[String].map(kv => kv).mustEqual(Map('test -> "foo"))
      xm.withFilter[Int].map(_._2 + 1).mustEqual(List(3))
      xm.withFilter(kv => kv._2.isInstanceOf[String]).map(kv => kv)
        .mustEqual(Map('test -> "foo"))
    }

    "support iteratorWithType[Type] method" in {
      val xm = UniformMap('test -> "foo", 'test2 -> 2)
      xm.iteratorWithType[String].next.mustEqual('test -> "foo")
      xm.iteratorWithType[Int].next.mustEqual('test2 -> 2)
    }

    "support multi-level apply/get/getAsType methods" in {
      // Mutli-level map
      val xm = UniformMap('test -> 
        UniformMap('test2 -> UniformMap('test3 -> 3)))

      xm('test :: 'test2 :: Nil).mustEqual(UniformMap('test3 -> 3))
      xm('test :: 'test4 :: Nil).must(throwA[NoSuchElementException])

      xm.get('test :: 'test2 :: Nil).mustEqual(Some(UniformMap('test3 -> 3)))
      xm.get('test :: 'test4 :: Nil).mustEqual(None)
      xm.get('test :: 'test2 :: 'test3 :: Nil).mustEqual(Some(3))

      xm.getAsType[Any]('test :: 'test2 :: Nil).mustEqual(
        Some(UniformMap('test3 -> 3)))
      xm.getAsType[Int]('test :: 'test2 :: 'test3 :: Nil).mustEqual(Some(3))
      xm.getAsType[String]('test :: 'test2 :: 'test3 :: Nil).mustEqual(None)
    }

    "support extraction" in {
      (UniformMap('test -> "foo", 'test2 -> 2) match {
        case UniformMap(h, tail @ _*) => h.toString
        case _ => "no match"
      }).mustEqual("('test,foo)")
    }

    "support primitive types" in {
      UniformMap('test -> "foo", 'test2 -> "bar").mustEqual(
        UniformMap('test -> "foo",'test2 -> "bar"))
      UniformMap('test -> 'foo, 'test2 -> 'bar).mustEqual(
        UniformMap('test -> 'foo,'test2 -> 'bar))
      UniformMap('test -> 1,'test2 -> 2).mustEqual(
        UniformMap('test -> 1,'test2 -> 2))
      UniformMap('test -> 1.toShort,'test2 -> 2.toShort).mustEqual(
        UniformMap('test -> 1.toShort,'test2 -> 2.toShort))
      UniformMap('test -> 1L,'test2 -> 2L).mustEqual(
        UniformMap('test -> 1L,'test2 -> 2L))
      UniformMap('test -> 1.0f,'test2 -> 2.0f).mustEqual(
        UniformMap('test -> 1.0f,'test2 -> 2.0f))
      UniformMap('test -> 1.0d,'test2 -> 2.0d).mustEqual(
        UniformMap('test -> 1.0d,'test2 -> 2.0d))
      UniformMap('test -> true,'test2 -> false).mustEqual(
        UniformMap('test -> true,'test2 -> false))
      UniformMap('test -> 'a','test2 -> 'b').mustEqual(
        UniformMap('test -> 'a','test2 -> 'b'))
      UniformMap('test -> 1.toByte, 'test2 -> 2.toByte).mustEqual(
        UniformMap('test -> 1.toByte,'test2 -> 2.toByte))
      UniformMap('test -> BigDecimal("1234"), 'test2 -> BigDecimal("34"))
        .mustEqual(
          UniformMap('test -> BigDecimal("1234"),'test2 -> BigDecimal("34")))
      UniformMap('test -> TimeZone.getTimeZone("PST"), 
          'test2 -> TimeZone.getTimeZone("UTC"))
        .mustEqual(
          UniformMap('test -> TimeZone.getTimeZone("PST"), 
             'test2 -> TimeZone.getTimeZone("UTC")))
    }

    "support embedded UniformLists and UniformMaps" in {
      UniformMap('test -> UniformList(1,2).filter[Int], 
                 'test2 -> UniformList(3,4).filter[Int])
        .mustEqual(
          UniformMap('test -> UniformList(1, 2).filter[Int], 
                     'test2 -> UniformList(3,4).filter[Int]))


      UniformMap('test -> UniformList(1,"foo"), 'test2 -> UniformList(3,4))
        .mustEqual(
          UniformMap('test -> UniformList(1,"foo"), 'test2 -> UniformList(3,4)))

      UniformMap('test -> UniformMap('test -> UniformMap('test -> "foo"),
                                     'test2 -> UniformMap('test -> 4)),
                 'test2 -> UniformMap('test -> UniformMap('test -> "bar")))
      .mustEqual(
        UniformMap('test -> UniformMap('test -> UniformMap('test -> "foo"),
                                       'test2 -> UniformMap('test -> 4)),
                   'test2 -> UniformMap('test -> UniformMap('test -> "bar"))))

      UniformMap('test -> UniformList(UniformMap('test -> "foo"),
                                      UniformMap('bar -> 4)),
                 'test2 -> UniformList(UniformMap('test3 -> "bar")))
      .mustEqual(
        UniformMap('test -> UniformList(UniformMap('test -> "foo"),
                                        UniformMap('bar -> 4)),
                   'test2 -> UniformList(UniformMap('test3 -> "bar"))))
    }

    "support all Map specific operations" in {
      val xm = UniformMap('test -> "foo", 'test2 -> 2)
      (xm + ('test3 -> "bar")).mustEqual(
        UniformMap('test -> "foo", 'test2 -> 2, 'test3 -> "bar"))
      // Mixed types (unique feature of UniformMap)
      (xm + ('test3 -> "bar", 'test4 -> 3)).mustEqual(
        UniformMap('test -> "foo", 'test2 -> 2, 'test3 -> "bar", 'test4 -> 3))

      (xm - 'test).mustEqual(UniformMap('test2 -> 2))
      (xm - ('test, 'test2)).mustEqual(UniformMap())

      val i = xm.iterator
      i.hasNext.mustEqual(true)
      i.next().mustEqual(('test -> "foo"))
      i.next().mustEqual(('test2 -> 2))

      val xm2 = UniformMap('test -> "foo", 'test2 -> 2, 'test3 -> "bar")
      (xm2 -- List('test2, 'test)).mustEqual(UniformMap('test3 -> "bar")) 
      // Reverse (List -- UniformMap.keySet)
      (Set('test, 'test2, 'test4) -- xm2.keySet).mustEqual(Set('test4)) 

      // apply
      xm('test).mustEqual("foo")
      xm('test2).mustEqual(2)
      xm('test5).must(throwA[NoSuchElementException])

      // updated
      xm.updated('test, "bar").mustEqual(
        UniformMap('test -> "bar", 'test2 -> 2))
      
      // Contains
      xm.contains('test).mustEqual(true)
      xm.contains('test5).mustEqual(false)

      // Empty
      xm.empty.mustEqual(UniformMap())
      xm.isEmpty.mustEqual(false)
    
      // filterKeys, filterNot
      xm.filterKeys(k => k == 'test).mustEqual(UniformMap('test -> "foo"))
      xm.filterNot(kv => kv._2.isInstanceOf[String]).mustEqual(
        UniformMap('test2 -> 2))

      // getOrElse
      xm.getOrElse('test5, 2).mustEqual(2) 

      // isDefinedAt
      xm.isDefinedAt('test2).mustEqual(true) 
      xm.isDefinedAt('foo).mustEqual(false) 

      // keys, values
      xm.keySet.mustEqual(Set('test,'test2))
      xm.keys.iterator.next().mustEqual('test)
      xm.keysIterator.next().mustEqual('test)
      xm.values.iterator.next().mustEqual("foo")
      xm.valuesIterator.next().mustEqual("foo")

      // mapValues
      (xm.mapValues { v => "test" + v.toString }).mustEqual(
        Map('test -> "testfoo", 'test2 -> "test2"))

      // withDefault
      xm.withDefault(key => "unknown")('test4).mustEqual("unknown")
      xm.withDefaultValue("unknown")('test4).mustEqual("unknown")

      // Misc
      xm.stringPrefix.mustEqual("UniformMap")
      xm.toBuffer.mustEqual(
        collection.mutable.ArrayBuffer('test -> "foo", 'test2 -> 2))
    }

    "support Function1 and PartialFunction traits" in {
      List('test,'test2).map(UniformMap('test -> "foo", 'test2 -> "bar"))
        .mustEqual(List("foo", "bar"))

      // Function1
      (UniformMap('test -> 10, 'test2 -> 20).compose { List('test,'test2) })(1)
        .mustEqual(20)

      // PartialFunction
      val f = UniformMap('test -> 1, 'test2 -> 2) andThen { i => i.toString}
      List('test,'test2).map(f).mustEqual(List("1", "2"))
      
      UniformMap('test -> 10, 'test2 -> 20).lift('test).mustEqual(Some(10))
      
      val p = UniformMap('test -> 1,'test2 -> 2).orElse{UniformMap('test3 -> 3)}
      List('test3).map(p).mustEqual(List(3))
    }

    "support Traversable/Iterable traits" in {
      val xm = UniformMap('test -> "foo", 'test2 -> 2)

      (xm ++ List(('test3 -> "ba"), ('test4 -> "ca"))).mustEqual(
        UniformMap('test -> "foo", 'test2 -> 2, 'test3 -> "ba", 'test4 -> "ca"))
      (xm ++ Map('test3 -> "ba", 'test4 -> "ca")).mustEqual(
        UniformMap('test -> "foo", 'test2 -> 2, 'test3 -> "ba", 'test4 -> "ca"))
      // Reverse (e.g. Map/List ++ UniformMap)
      (List(('test3 -> "ba"), ('test4 -> "ca")) ++ xm).mustEqual(
        List(('test3 -> "ba"),('test4 -> "ca"),('test -> "foo"),('test2 -> 2)))
      (Map('test3 -> "ba", 'test4 -> "ca") ++ xm).mustEqual(
        Map('test -> "foo", 'test2 -> 2, 'test3 -> "ba", 'test4 -> "ca"))

      (xm ++: List(('test3 -> "ba"), ('test4 -> "ca"))).mustEqual(
        List('test -> "foo", 'test2 -> 2, 'test3 -> "ba", 'test4 -> "ca"))
      (xm ++: Map('test3 -> "ba", 'test4 -> "ca")).mustEqual(
        Map('test -> "foo", 'test2 -> 2, 'test3 -> "ba", 'test4 -> "ca"))
      // Reverse (e.g. Map/List ++ UniformMap)
      (List(('test3 -> "ba"), ('test4 -> "ca")) ++: xm).mustEqual(
        UniformMap('test -> "foo",'test2 -> 2,'test3 -> "ba",'test4 -> "ca"))
      (Map('test3 -> "ba", 'test4 -> "ca") ++: xm).mustEqual(
        UniformMap('test -> "foo", 'test2 -> 2, 'test3 -> "ba", 'test4 -> "ca"))

      // Note: UniformMap is like working with Map[Symbol,Any] so restricts
      //       usefulness of fold operations 
      
      // FoldLeft
      (("" /: xm) { case (s, (k,v)) => s + v.toString }).mustEqual("foo2")
      (xm.foldLeft("") { case (s, (k,v)) => s + v.toString }).mustEqual("foo2")
      (xm.filter[Int].foldLeft(2) { case (s, (k,v)) => s + v }).mustEqual(4)

      // FoldRight
      ((0 /: UniformMap('test -> 2,'test2 -> 3,'test3 -> "foo")
        .filter[Int]) { 
          (s, kv) => s + kv._2.asInstanceOf[Int] }).mustEqual(5)
      ((xm :\ "") { case ((k,v), s) => s + v.toString }).mustEqual("2foo")
      (xm.foldRight("") { case ((k,v), s) => s + v.toString }).mustEqual("2foo")
      (xm.filter[Int].foldRight(2) { case ((k,v), s) => s + v }).mustEqual(4)

      // Parallel fold 
      ((xm /:\ ('result, "")) { 
        case ((k1, v1), (k2, v2)) => ('result, v1+v2.toString) 
      }).mustEqual(('result, "foo2"))
      ((xm.fold('result, "")) { 
        case ((k1, v1), (k2, v2)) => ('result, v1+v2.toString) 
      }).mustEqual(('result, "foo2"))
      ((xm.filter[Int].fold('result, 0)) { 
        case ((k1, v1), (k2, v2)) => ('result, v1+v2) 
      }).mustEqual(('result, 2))


      // Collect
      (xm.collect { case kv if (kv._1 == 'test) => kv })
        .mustEqual(Map('test -> "foo"))
      (xm.collectFirst { case kv => kv }).mustEqual(Some('test -> "foo"))

      // Copy 
      val xs1 = new Array[(Symbol,Any)](2)
      xm.copyToArray(xs1)
      xs1.mustEqual(Array(('test -> "foo"),('test2 -> 2)))
      val xs2 = new collection.mutable.ArrayBuffer[(Symbol,Any)](2)
      xm.copyToBuffer(xs2)
      xs2.mustEqual(
        collection.mutable.ArrayBuffer(('test -> "foo"),('test2 -> 2)))
      val xs3 = new Array[(Symbol,Int)](2)
      UniformMap('test -> 1, 'test2 -> 2).filter[Int].copyToArray(xs3)
      xs3.mustEqual(Array(('test -> 1),('test2 -> 2)))

      // Count 
      (xm.count { kv => kv._2 == 2 }).mustEqual(1)
      (xm.filter[Int].count { kv => kv._2 > 1 }).mustEqual(1)

      // Drop
      xm.drop(1).mustEqual(UniformMap('test2 -> 2))
      xm.dropRight(1).mustEqual(UniformMap('test -> "foo"))
      xm.dropWhile(kv => kv._1 == 'test).mustEqual(UniformMap('test2 -> 2))

      // Exists
      xm.exists(kv => kv._1 == 'test).mustEqual(true)
      xm.exists(kv => kv._1 == 'test3).mustEqual(false)

      // Filter
      xm.filter(kv => kv._1 == 'test).mustEqual(UniformMap('test -> "foo"))
      xm.filter[Int].filter(kv => kv._2 > 2).mustEqual(UniformMap())
      
      // Find
      xm.find(kv => kv._1 == 'test).mustEqual(Some('test -> "foo"))
      xm.find(kv => kv._1 == 'test3).mustEqual(None)
   
      // flatMap
      xm.flatMap(kv => UniformMap(kv._1 -> (kv._2 + "test"))).mustEqual(
        Map('test -> "footest", 'test2 -> "2test"))

      // forall
      xm.forall(kv => kv._1.isInstanceOf[Symbol]).mustEqual(true)
      xm.forall(kv => kv._2.isInstanceOf[String]).mustEqual(false)

      // foreach
      val strBuf = new StringBuilder()
      xm.foreach { kv => strBuf.append(kv._2) }
      strBuf.toString.mustEqual("foo2")

      // Group by
      (UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .filter[Int].groupBy { kv => kv._2 % 2 == 0 })
        .mustEqual(Map(true -> UniformMap('test2 -> 2, 'test4 -> 4),
                       false -> UniformMap('test1 -> 1, 'test3 -> 3)))

      // group
      UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .grouped(2).next().mustEqual(UniformMap('test1 -> 1, 'test2 -> 2))

      // head
      xm.head.mustEqual(('test -> "foo"))
      xm.headOption.mustEqual(Some('test -> "foo"))

      // init
      UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3).init.mustEqual(
        UniformMap('test1 -> 1, 'test2 -> 2))
      // Iterates over all, then all - last, ... 
      UniformMap('test1 -> 1).inits.next.mustEqual(UniformMap('test1 -> 1))

      // last
      xm.last.mustEqual(('test2 -> 2))
      xm.lastOption.mustEqual(Some('test2 -> 2))
     
      // map 
      (xm.map { kv => (kv._1.name, kv._2) }).mustEqual(
        Map("test" -> "foo", "test2" -> 2))

      // partition
      (xm.partition { kv => (kv._2.isInstanceOf[String]) }).mustEqual(
        (UniformMap('test -> "foo"), UniformMap('test2 -> 2)))

      // reduceLeft
      (UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .filter[Int].reduceLeft{ (kv1, kv2) => ('result, kv1._2 + kv2._2) })
        .mustEqual(('result, 10))
      (UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .filter[Int].reduceLeftOption{(kv1,kv2) => ('result, kv1._2+kv2._2) })
        .mustEqual(Some('result, 10))
      // reduceRight
      (UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .filter[Int].reduceRight{ (kv2, kv1) => ('result, kv2._2 + kv1._2) })
        .mustEqual(('result, 10))
      (UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .filter[Int].reduceRightOption{
          (kv2,kv1) => ('result, kv2._2+kv1._2) })
        .mustEqual(Some('result, 10))
      // reduce
      (UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .filter[Int].reduce{ (kv1, kv2) => ('result, kv1._2 + kv2._2) })
        .mustEqual(('result, 10))
      (UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .filter[Int].reduceOption{ (kv1, kv2) => ('result, kv1._2 + kv2._2) })
        .mustEqual(Some('result, 10))

      // sameElements
      xm.sameElements(List(('test -> "foo"), ('test2 -> 2))).mustEqual(true)

      // Scan
      (xm.scan('start -> 0) { (kv, kv2)  => kv2 }).mustEqual(
        Map('start -> 0, 'test -> "foo", 'test2 -> 2))
      (xm.scanLeft('start -> 0 : Any) { (kv, kv2)  => kv2 }).mustEqual(
        List('start -> 0, 'test -> "foo", 'test2 -> 2))
      (xm.scanRight('start -> 0 : Any) { (kv, kv2)  => kv }).mustEqual(
        List('test -> "foo", 'test2 -> 2, 'start -> 0))

      // Slice
      xm.slice(0,1).mustEqual(UniformMap('test -> "foo"))

      // Sliding
      val s = UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .sliding(2, 2)
      s.next()
      s.next().mustEqual(UniformMap('test3 -> 3, 'test4 -> 4))
      UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3, 'test4 -> 4)
        .sliding(2).next().mustEqual(UniformMap('test1 -> 1, 'test2 -> 2))

      // Span
      (xm.span { kv => (kv._2.isInstanceOf[String]) }).mustEqual(
        (UniformMap('test -> "foo"), UniformMap('test2 -> 2)))
     
      // splitAt
      UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3)
        .splitAt(1).mustEqual(UniformMap('test1 -> 1), 
          UniformMap('test2 -> 2, 'test3 -> 3))

      // tail
      UniformMap('test1 -> 1, 'test2 -> 2, 'test3 -> 3).tail.mustEqual(
        UniformMap('test2 -> 2, 'test3 -> 3))
      // Iterates over all, then all - last, ... 
      UniformMap('test1 -> 1).tails.next.mustEqual(UniformMap('test1 -> 1))

      // take
      xm.take(1).mustEqual(UniformMap('test -> "foo"))
      xm.takeRight(1).mustEqual(UniformMap('test2 -> 2))
      xm.takeWhile(kv => kv._1 == 'test).mustEqual(UniformMap('test -> "foo"))

      // toXxx
      xm.toArray.mustEqual(Array('test -> "foo", 'test2 -> 2))
      xm.toIndexedSeq.mustEqual(IndexedSeq('test -> "foo", 'test2 -> 2))
      xm.toIterable.iterator.next().mustEqual(('test -> "foo"))
      xm.toIterator.next().mustEqual(('test -> "foo"))
      xm.toList.mustEqual(List('test -> "foo", 'test2 -> 2))
      xm.toMap.mustEqual(collection.mutable.Map('test -> "foo", 'test2 -> 2))
      xm.toSet.mustEqual(Set('test -> "foo", 'test2 -> 2))
      xm.toStream.mustEqual(Stream('test -> "foo", 'test2 -> 2))
      xm.toTraversable.mustEqual(Map('test -> "foo", 'test2 -> 2))

      // unzip
      xm.unzip._1.iterator.next().mustEqual('test)
      xm.unzip._2.iterator.next().mustEqual("foo")

      // views/monadic filters
      xm.view.iterator.next.mustEqual(('test -> "foo"))
      xm.view(1,2).iterator.next.mustEqual(('test2 -> 2))
      xm.withFilter(kv => kv._2.isInstanceOf[Int]).map(kv => kv)
        .mustEqual(Map('test2 -> 2))

      // zip
      xm.zip(List(1, 2))
        .mustEqual(Map(('test -> "foo") -> 1, ('test2 -> 2) -> 2))
      xm.zipWithIndex.mustEqual(Map(('test -> "foo") -> 0, ('test2 -> 2) -> 1))

      // Misc 
      xm.addString(new StringBuilder(), "s", "-", "e").toString().mustEqual(
        "s'test -> foo-'test2 -> 2e")
      xm.aggregate("")( ((z, kv) => z + kv._2), ((z1, z2) => z1 + "-" + z2) )
        .mustEqual("foo2")
      xm.mkString("-").toString().mustEqual("'test -> foo-'test2 -> 2")
      xm.size.mustEqual(2)
      xm.hasDefiniteSize.mustEqual(true)
      xm.isTraversableAgain.mustEqual(true)
      xm.nonEmpty.mustEqual(true)
    }
  }
}

// Test Classes
case class Test1(s: String)
case class Test2(str: String, num: Int, sht: Short, lng: Long, flt: Float, dbl: Double, ch: Char, bool: Boolean, byt: Byte)
case class Test3(xs: List[String])
case class Test4(xs1: List[String], xs2: List[Int])
case class Test5(xs: List[Test1])  // not allowed
case class Test6(xm: Map[Symbol, String])
case class Test7(xm1: Map[Symbol, String], xm2: Map[Symbol, Int])
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
