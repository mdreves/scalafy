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
package test.scalafy.types

import org.specs2.mutable.Specification

import scalafy.types._
import scalafy.types.extractors._

/** Test specification for casing package */
object TypesSpec extends Specification {

  "The types package" should {
    "support extactors for matching List types" in {
      // Positive test
      (ReifiableList("foo", "bar") match { 
        case l if (ReifiableList.isListOf(l, manifest[String])) => "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test
      (ReifiableList(1, 2) match { 
        case l if (ReifiableList.isListOf(l, manifest[String])) => "match" 
        case _ => "no match"
      }).mustEqual("no match")

      // Positive test
      (ReifiableList(List("foo", "bar")) match { 
        case l if (ReifiableList.isListOf(l, manifest[List[String]])) => 
          "match"
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (ReifiableList(List(1, 2)) match { 
        case l if (ReifiableList.isListOf(l, manifest[List[String]])) => 
          "match"
        case _ => "no match"
      }).mustEqual("no match")
 
      // Positive test
      (ReifiableList(List(1, 2)) match { 
        case l if (ReifiableList.isListOf(l, manifest[List[Int]])) => 
          "match"
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (ReifiableList(List("foo", "bar")) match { 
        case l if (ReifiableList.isListOf(l, manifest[List[Int]])) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
 
      // Positive test
      ({
        val x = ReifiableList("foo", "bar")
        x match { 
          case s :: rest if (
            ReifiableList.isListOf(x, manifest[String]) && s == "foo"
          ) => 
            "match" 
          case _ => "no match"
        }
      }).mustEqual("match")
    }

    "support extactors for matching Map types" in {
      // Positive test
      (ReifiableMap("foo" -> "bar") match { 
        case m if (ReifiableMap.isMapOf(m, manifest[(String,String)])) => 
          "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (ReifiableMap(1 -> 3) match { 
        case m if (ReifiableMap.isMapOf(m, manifest[(String,String)])) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
      
      // Positive test
      (ReifiableMap("foo" -> 3) match { 
        case m if (ReifiableMap.isMapOf(m, manifest[(String, Int)])) => 
          "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (ReifiableMap(1 -> 3) match { 
        case m if (ReifiableMap.isMapOf(m, manifest[(String,Int)])) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
      
      // Positive test
      (ReifiableMap("foo" -> ReifiableMap(1 -> 2)) match { 
        case m if (
          ReifiableMap.isMapOf(m, manifest[(String, Map[Int,Int])])
        ) => 
          "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (ReifiableMap("foo" -> ReifiableMap(1 -> 2)) match { 
        case m if (
          ReifiableMap.isMapOf(m, manifest[(String, Map[String,Int])])
        ) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
    }

    "support extacting using ->" in {
      ("foo" -> "bar" match { 
        case "foo" -> "bar" => "match" 
        case _ => "no match"
      }).mustEqual("match")
    }

    "support extacting from Maps using ->" in {
      (Map("foo" -> "bar") match { 
        case Map("foo" -> "bar") => "match" 
        case _ => "no match"
      }).mustEqual("match")
    }

    "support primitive extractors" in {
      ("test" match { 
        case String(s) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (5 match { 
        case Int(i) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ((5 : Short) match { 
        case Short(s) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      (5L match { 
        case Long(l) => "match" 
        case _ => "no match"
      }).mustEqual("match")
      
      (1.0f match { 
        case Float(f) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (1.0e100d match { 
        case Double(d) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      (true match { 
        case Boolean(b) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      (false match { 
        case Boolean(b) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ('c' match { 
        case Char(c) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ((0 : Byte) match { 
        case Byte(b) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      // Negative test
      (("test" : Any) match { 
        case Int(s) => "match" 
        case _ => "no match"
      }).mustEqual("no match")
    }
     
    "support NameValuePair.toMap" in {
      NameValuePair.toMap("a: b,c\nd: e,f") 
        .mustEqual(Map("a" -> "b,c", "d" -> "e,f"))
      
      NameValuePair.toMap("a: b,c\nd: e,f\na: g") 
        .mustEqual(Map("a" -> "b,c,g", "d" -> "e,f"))
    }

    "support NameValuePair.toString" in {
      NameValuePair.toString(Map("a" -> "b,c,g", "d" -> "e,f"))
        .mustEqual("a: b,c,g\nd: e,f")
    }
 
    "support NameValuePair.addToMap" in {
      NameValuePair.addToMap(Map("a" -> "b,c,g", "d" -> "e,f"), "h", "i")
        .mustEqual(Map("a" -> "b,c,g", "d" -> "e,f", "h" -> "i"))
      
      NameValuePair.addToMap(
          Map("a" -> "b,c,g", "d" -> "e,f", "h" -> "i"), "h", "j")
        .mustEqual(Map("a" -> "b,c,g", "d" -> "e,f", "h" -> "j"))
      
      NameValuePair.addToMap(
          Map("a" -> "b,c,g", "d" -> "e,f", "h" -> "i"), "h", "j", false)
        .mustEqual(Map("a" -> "b,c,g", "d" -> "e,f", "h" -> "i,j"))
    }

    "support NameValuePair.addToString" in {
      NameValuePair.addToString("a: b,c,g\nd: e,f", "h", "i")
        .mustEqual("a: b,c,g\nd: e,f\nh: i")
      
      NameValuePair.addToString("a: b,c,g\nd: e,f\nh: i", "h", "j")
        .mustEqual("a: b,c,g\nd: e,f\nh: j")
      
      NameValuePair.addToString("a: b,c,g\nd: e,f\nh: i", "h", "j", false)
        .mustEqual("a: b,c,g\nd: e,f\nh: i,j")
    }
 
    "support NameValuePair.mergeMaps" in {
      NameValuePair.mergeMaps(
          Map("a" -> "b,c", "d" -> "e,f"),
          Map("g" -> "h", "i" -> "j") )
        .mustEqual(Map("a" -> "b,c", "d" -> "e,f", "g" -> "h", "i" -> "j"))
      
      NameValuePair.mergeMaps(
          Map("a" -> "b,c", "d" -> "e,f"),
          Map("a" -> "k", "i" -> "j") )
        .mustEqual(Map("a" -> "b,c,k", "d" -> "e,f", "i" -> "j"))
      
      NameValuePair.mergeMaps(
          Map("a" -> "b,c", "d" -> "e"),
          Map("a" -> "b", "d" -> "e"), true)
        .mustEqual(Map("a" -> "c"))
      
      NameValuePair.mergeMaps(
          Map("a" -> "b,c", "d" -> "e"),
          Map("a" -> null), true)
        .mustEqual(Map("d" -> "e"))
    }
   
    "support NameValuePair.mergeStrings" in {
      NameValuePair.mergeStrings("a: b,c\nd: e,f", "g: h\ni: j")
        .mustEqual("a: b,c\nd: e,f\ng: h\ni: j")
      
      NameValuePair.mergeStrings("a: b,c\nd: e,f", "a: k\ni: j")
        .mustEqual("a: b,c,k\nd: e,f\ni: j")

      NameValuePair.mergeStrings("a: b,c\nd: e", "a: b\nd: e", true)
        .mustEqual("a: c")

      NameValuePair.mergeStrings("a: b,c\nd: e", "a", true)
        .mustEqual("d: e")
    }
 
    "support basic type extractors" in {
      (List("foo", "bar") match { 
        case ListOfString(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (List(3, 4) match { 
        case ListOfInt(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (List[Short](3, 4) match { 
        case ListOfShort(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (List(3L, 4L) match { 
        case ListOfLong(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      (List(3.0f, 5.9f) match { 
        case ListOfFloat(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      (List(3.0d, 5.9d) match { 
        case ListOfDouble(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (List(true, false) match { 
        case ListOfBoolean(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (List('a', 'b') match { 
        case ListOfChar(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (List[Byte](0, 3) match { 
        case ListOfByte(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      // Negative test
      (List("foo", "bar") match { 
        case ListOfInt(xs @ _*) => "match" 
        case _ => "no match"
      }).mustEqual("no match")

      (Map("foo" -> "boo") match { 
        case MapOfStringToString(kvs) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (Map("foo" -> 5) match { 
        case MapOfStringToInt(kvs) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      (Map(3 -> 5) match { 
        case MapOfIntToInt(kvs) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      // Negative test
      (Map("foo" -> "bar") match { 
        case MapOfStringToInt(kvs) => "match" 
        case _ => "no match"
      }).mustEqual("no match")
    }
  }
}
