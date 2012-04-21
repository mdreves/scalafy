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
package test.scalafy.types.extractors

import org.specs2.mutable.Specification

import scalafy.types.extractors._

/** Test specification for extractors package */
object ExtractorTypesSpec extends Specification {

  "The extractor types package" should {
    "support List/Map type extractors" in {
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
