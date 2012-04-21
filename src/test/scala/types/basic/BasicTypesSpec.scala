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
package test.scalafy.types.basic

import org.specs2.mutable.Specification

import scalafy.types.basic._

/** Test specification for basic types package */
object BasicTypesSpec extends Specification {

  "The basic types package" should {
    "support extacting using ->" in {
      ("foo" -> "bar" match { 
        case "foo" -> "bar" => "match" 
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
 
      ("5" match {
        case Int(i) => "match"
        case _ => "no match"
      }).mustEqual("match")
  
      (("5": Any) match {
        case Int(i) => "match"
        case _ => "no match"
      }).mustEqual("match")

      ((5 : Short) match { 
        case Short(s) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ("5" match { 
        case Short(s) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      (5L match { 
        case Long(l) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ("5" match { 
        case Long(l) => "match" 
        case _ => "no match"
      }).mustEqual("match")
      
      (1.0f match { 
        case Float(f) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      ("1.0" match { 
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
 
      ("true" match { 
        case Boolean(b) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      (false match { 
        case Boolean(b) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ("false" match { 
        case Boolean(b) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ('c' match { 
        case Char(c) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ("c" match { 
        case Char(c) => "match" 
        case _ => "no match"
      }).mustEqual("match")
 
      ((0 : Byte) match { 
        case Byte(b) => "match" 
        case _ => "no match"
      }).mustEqual("match")

      // Multiple matches
      ("32767" match { 
        case Short(s) => "match short" 
        case Int(i) => "match int" 
        case Long(l) => "match long" 
        case _ => "no match"
      }).mustEqual("match short")
 
      ("32768" match { 
        case Short(s) => "match short" 
        case Int(i) => "match int" 
        case Long(l) => "match long" 
        case _ => "no match"
      }).mustEqual("match int")
 
      ("2147483648" match { 
        case Short(s) => "match short" 
        case Int(i) => "match int" 
        case Long(l) => "match long" 
        case _ => "no match"
      }).mustEqual("match long")
 
      ("1.0" match { 
        case Float(f) => "match float" 
        case Double(d) => "match double" 
        case _ => "no match"
      }).mustEqual("match float")
 
      ("x" match { 
        case Char(c) => "match char" 
        case String(s) => "match string" 
        case _ => "no match"
      }).mustEqual("match char")
 
      ("xx" match { 
        case Char(c) => "match char" 
        case String(s) => "match string" 
        case _ => "no match"
      }).mustEqual("match string")
 
      ("foo" match { 
        case Short(s) => "match short" 
        case Int(i) => "match int" 
        case Long(l) => "match long" 
        case Float(f) => "match float" 
        case Double(d) => "match double" 
        case Char(d) => "match double" 
        case String(s) => "match string" 
        case _ => "no match"
      }).mustEqual("match string")
 
      // Negative tests
      (("test" : Any) match { 
        case Int(s) => "match" 
        case _ => "no match"
      }).mustEqual("no match")

      ("cc" match { 
        case Char(c) => "match" 
        case _ => "no match"
      }).mustEqual("no match")
    }
  }
}
