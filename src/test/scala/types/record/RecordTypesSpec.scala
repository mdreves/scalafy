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
package test.scalafy.types.record

import org.specs2.mutable.Specification

import scalafy.types.record._

/** Test specification for record package */
object RecordTypesSpec extends Specification {

  "The record types package" should {

    "support getAsType method" in {
      val t2 = Test2("test", 1, 2, 3, 1.0f, 2.0, 'a', true, 4)
      t2.getAsType[String]('str).mustEqual(Some("test"))
      t2.getAsType[Int]('str).mustEqual(None)
      t2.getAsType[String]('num).mustEqual(None)
      t2.getAsType[Int]('num).mustEqual(Some(1))
    }

    "support iteratorWithType method" in {
      val t2 = Test2("test", 1, 2, 3, 1.0f, 2.0, 'a', true, 4)
      val iter = t2.iteratorWithType[Int]
      iter.next.mustEqual('num -> 1)
      val iter2 = t2.iteratorWithType[Float]
      iter2.next.mustEqual('flt -> 1.0f)
    }

    "support withFilter[Type] method" in {
      val t2 = Test2("test", 1, 2, 3, 1.0f, 2.0, 'a', true, 4)
      t2.withFilter[Int].map(_._2 + 1).mustEqual(List(2))
      t2.withFilter(kv => kv._1 == 'bool).map(kv => kv)
        .mustEqual(Map('bool -> true))
    }


    "support apply/get/getAsType with multiple keys" in {
      val t8 = Test8(Test1("t1"), Test2("t2", 1, 2, 3, 1.0f, 2.0, 'a', true, 4))

      t8.getAsType[String]('t :: 's :: Nil).mustEqual(Some("t1"))
      t8.getAsType[Int]('t :: 'str :: Nil).mustEqual(None)
      t8.getAsType[Any]('t :: 'x :: Nil).mustEqual(None)
      t8.getAsType[Float]('t2 :: 'flt :: Nil).mustEqual(Some(1.0))
      
      t8.get('t2 :: 'dbl :: Nil).mustEqual(Some(2.0))
      t8.get('t2 :: 'bool :: Nil).mustEqual(Some(true))

      t8('t2 :: 'flt :: Nil).mustEqual(1.0)
      t8('t2 :: 'dbl :: Nil).mustEqual(2.0)
      t8('t2 :: 'none :: Nil).must(throwA[NoSuchElementException])
      
    }

    "support immutable + method" in {
      val t1 = Test1("test")
      
      // Somehow this gets converted to map... (specs2 issue or ours?) 
      (t1 + ('s -> "test2")).mustEqual(Map('s -> "test2"))

      // But we know it is actually a Record, so this works...
      val t1_2 = t1 + ('s -> "test2")
      (t1_2 == Test1("test2")).mustEqual(true)

      val t2 = Test2("test", 1, 2, 3, 1.0f, 2.0, 'a', true, 4)
      
      // Somehow this gets converted to map... (specs2 issue or ours?) 
      (t2 + ('num -> 3)).mustEqual(Map('str -> "test", 'num -> 3, 'sht -> 2,
          'lng -> 3, 'flt -> 1.0, 'dbl -> 2.0, 'ch -> 'a', 'bool -> true,
          'byt -> 4))
    }

    "support mutable += and update methods" in {
      val t9 = new Test9("test", 2)
      
      // Somehow this gets converted to map... (specs2 issue or ours?) 
      (t9 += ('i -> 3)).mustEqual(new Test9("test", 3))
      t9('i) = 5
      t9.mustEqual(new Test9("test", 5))
    }

    "support all the Map operations" in {
      val t = Test1("test")
      t.get('s).mustEqual(Some("test"))
      t('s).mustEqual("test")

      val t2 = Test2("test", 1, 2, 3, 1.0f, 2.0, 'a', true, 4)
      val iter = t2.iterator
      iter.next.mustEqual('str -> "test")
      iter.next.mustEqual('num -> 1)
      iter.next.mustEqual('sht -> 2)
      iter.next.mustEqual('lng -> 3)
      iter.next.mustEqual('flt -> 1.0f)
      iter.next.mustEqual('dbl -> 2.0)
      iter.next.mustEqual('ch -> 'a')
      iter.next.mustEqual('bool -> true)
      iter.next.mustEqual('byt -> 4)

      val t10 = new Test10() 
      (t10.empty == new Test10()).mustEqual(true)

      // treating record as function itself
      List('s).map(t).mustEqual(List("test"))

      (t2.keySet == Set('str, 'num, 'sht, 'flt, 'lng, 'dbl, 'ch, 'bool, 'byt))
        .mustEqual(true)
      t2.head.mustEqual(('str -> "test"))
      t2.last.mustEqual(('byt -> 4))
      t2.isDefinedAt('str).mustEqual(true)
      t2.isDefinedAt('foo).mustEqual(false)
      t.toMap.mustEqual(Map('s -> "test"))
      t2.values.iterator.next.mustEqual("test")
      t2.keys.iterator.next.mustEqual('str)
      t2.contains('flt).mustEqual(true)
      val dropTest = new Test10("bar", 20)
      (dropTest.drop(1) == new Test10("foo", 20)).mustEqual(true)
      val takeTest = new Test10("bar", 20)
      (takeTest.take(1) == new Test10("bar", 1)).mustEqual(true)
    }

    "support objects with nested lists/maps/objects" in {
      val t3 = Test3(List("foo", "bar"))
      t3('xs).mustEqual(List("foo", "bar"))
      
      val t4 = Test4(List("foo", "bar"), List(1,2))
      t4('xs2).mustEqual(List(1,2))

      val t5 = Test5(List(Test1("test"), Test1("test2")))
      t5('xs).mustEqual(List(Test1("test"), Test1("test2")))

      val t6 = Test6(Map('t3 -> "test", 't4 -> "test2"))
      t6('xm).mustEqual(Map('t3 -> "test", 't4 -> "test2"))

      val t7 = Test7(Map('t3 -> "test"), Map('t4 -> 2))
      t7('xm1).mustEqual(Map('t3 -> "test"))
      t7('xm2).mustEqual(Map('t4 -> 2))

      val t11 = new Test11("test")
      t11('b).mustEqual(false)
      t11.b = true
      t11('b).mustEqual(true)
    }
  }
}

// Test Classes
case class Test1(s: String) extends Record
case class Test2(str: String, num: Int, sht: Short, lng: Long, flt: Float, dbl: Double, ch: Char, bool: Boolean, byt: Byte) extends Record
case class Test3(xs: List[String]) extends Record
case class Test4(xs1: List[String], xs2: List[Int]) extends Record
case class Test5(xs: List[Test1])  extends Record
case class Test6(xm: Map[Symbol, String]) extends Record
case class Test7(xm1: Map[Symbol, String], xm2: Map[Symbol, Int]) extends Record
case class Test8(t: Test1, t2: Test2)  extends Record
class Test9(val s: String, var i: Int) extends Record {
  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[Test9]) return false
    val o = that.asInstanceOf[Test9]
    o.s == s && o.i == i
  }
}
class Test10(val s: String, var i: Int) extends Record {
  def this() = this("foo", 1)
  def this(s: String) = this(s, 1)
  def this(i: Int) = this("foo", i)

  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[Test10]) return false
    val o = that.asInstanceOf[Test10]
    o.s == s && o.i == i
  }
}
class Test11(val s: String) extends Record {
  val l = 3L
  var b = false

  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[Test11]) return false
    val o = that.asInstanceOf[Test11]
    o.s == s && o.b == b
  }
}

