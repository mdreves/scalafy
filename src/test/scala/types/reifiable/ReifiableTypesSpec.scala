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
package test.scalafy.types.reifiable

import org.specs2.mutable.Specification

import scalafy.types.reifiable._

/** Test specification for reifiable types package */
object ReifiableTypesSpec extends Specification {

  "The reifiable types package" should {
    "support reifiable types" in {
      implicit val reifableSettings = ReifiableSettings(true)

      // Positive test
      (Reifiable(List("foo", "bar")) match { 
        case l if (l.isType[List[String]]) => "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test
      (Reifiable(List(1, 2)) match { 
        case l if (l.isType[List[String]]) => "match" 
        case _ => "no match"
      }).mustEqual("no match")

      // Positive test
      (Reifiable(List(List("foo", "bar"))) match { 
        case l if (l.isType[List[List[String]]]) => 
          "match"
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (Reifiable(List(List(1, 2))) match { 
        case l if (l.isType[List[List[String]]]) => 
          "match"
        case _ => "no match"
      }).mustEqual("no match")
 
      // Positive test
      (Reifiable(List(List(1, 2))) match { 
        case l if (l.isType[List[List[Int]]]) => 
          "match"
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (Reifiable(List(List("foo", "bar"))) match { 
        case l if (l.isType[List[List[Int]]]) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
 
      // Positive test
      ({
        val x = Reifiable(List("foo", "bar"))
        x match { 
          case s :: rest if (
            x.isType[List[String]] && s == "foo"
          ) => 
            "match" 
          case _ => "no match"
        }
      }).mustEqual("match")

      // Positive test
      (Reifiable(Map("foo" -> "bar")) match { 
        case m if (m.isType[Map[String,String]]) => 
          "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (Reifiable(Map(1 -> 3)) match { 
        case m if (m.isType[Map[String,String]]) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
      
      // Positive test
      (Reifiable(Map("foo" -> 3)) match { 
        case m if (m.isType[Map[String, Int]]) => 
          "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (Reifiable(Map(1 -> 3)) match { 
        case m if (m.isType[Map[String,Int]]) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
      
      // Positive test
      (Reifiable(Map("foo" -> Map(1 -> 2))) match { 
        case m if (
          m.isType[Map[String, Map[Int,Int]]]
        ) => 
          "match" 
        case _ => "no match"
      }).mustEqual("match")
      // Negative test 
      (Reifiable(Map("foo" -> Map(1 -> 2))) match { 
        case m if (
          m.isType[Map[String, Map[String,Int]]]
        ) => 
          "match" 
        case _ => "no match"
      }).mustEqual("no match")
    }
  }
}
