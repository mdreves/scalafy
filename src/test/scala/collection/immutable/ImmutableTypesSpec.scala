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
package test.scalafy.collection.immutable

import org.specs2.mutable.Specification

import scalafy.types.basic.->
import scalafy.collection.immutable._

/** Test specification for immutable collection package */
object ImmutableTypesSpec extends Specification {

  "Map" should {
    "support extacting using ->" in {
      (Map("foo" -> "bar") match { 
        case Map("foo" -> "bar") => "match" 
        case _ => "no match"
      }).mustEqual("match")
    }
  }

  "NameValuePair" should {
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
  }
}
