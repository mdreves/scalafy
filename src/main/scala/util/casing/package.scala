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
package scalafy.util

/** Contains objects used for case matching and conversion.
  *
  * The following is a summary of features:
  * {{{
  * // Implicit conversions
  * "my_string".toLowerCamelCase   // myString
  * "my_string".toUpperCamelCase   // MyString
  * "My-String".toLowerSnakeCase   // my_string
  * "my_string".toUpperSnakeCase   // MY_STRING
  * "my_string".toLowerDashCase    // my-string
  * "my_string".toUpperDashCase    // My-String
  *
  * // List("strFoo", "strBar")
  * List("str_foo", "str_bar").toLowerCamelCase
  *
  * // Map("strFoo" -> 1, "strBar" -> 2)
  * Map("str_foo" -> 1, "str_bar" -> 2).toLowerCamelCase
  *
  * // Extractors
  * x match {
  *   case LowerCase(x) => println("lowercase: " + x)
  *   case UpperCase(x) => println("UPPERCASE: " + x)
  *   case LowerCamelCase(x) => println("lowerCamelCase: " + x)
  *   case UpperCamelCase(x) => println("UperCamelCase: " + x)
  *   case LowerSnakeCase(x) => println("lower_snake_case: " + x)
  *   case UpperSnakeCase(x) => println("UPPER_SNAKE_CASE: " + x)
  *   case LowerDashCase(x) => println("lower-dash-case: " + x)
  *   case UpperDashCase(x) => println("Upper-Dash-Case: " + x)
  *   case _ => println("no match")
  * }
  * }}}
  */
package object casing {

  ///////////////////////////////////////////////////////////////////////////
  // Implicits
  ///////////////////////////////////////////////////////////////////////////

  final class IterableCaseConverter(val iter: Iterable[Char]) {
    def toLowerCamelCase() = Casing.toLowerCamelCase(iter)
    def toUpperCamelCase() = Casing.toUpperCamelCase(iter)
    def toLowerSnakeCase() = Casing.toLowerSnakeCase(iter)
    def toUpperSnakeCase() = Casing.toUpperSnakeCase(iter)
    def toLowerDashCase() = Casing.toLowerDashCase(iter)
    def toUpperDashCase() = Casing.toUpperDashCase(iter)
  }

  implicit def string2CaseConverter(s: String) = new IterableCaseConverter(s)
  implicit def iterable2CaseConverter(iter: Iterable[Char]) =
    new IterableCaseConverter(iter)

  final class SeqCaseConverter(val xs: Seq[String]) {
    def toLowerCase() = Casing.toCaseOnSeq(xs, LowerCase)
    def toUpperCase() = Casing.toCaseOnSeq(xs, UpperCase)
    def toLowerCamelCase() = Casing.toCaseOnSeq(xs, LowerCamelCase)
    def toUpperCamelCase() = Casing.toCaseOnSeq(xs, UpperCamelCase)
    def toLowerSnakeCase() = Casing.toCaseOnSeq(xs, LowerSnakeCase)
    def toUpperSnakeCase() = Casing.toCaseOnSeq(xs, UpperSnakeCase)
    def toLowerDashCase() = Casing.toCaseOnSeq(xs, LowerDashCase)
    def toUpperDashCase() = Casing.toCaseOnSeq(xs, UpperDashCase)
  }

  implicit def seq2CaseConverter(xs: Seq[String]) =
    new SeqCaseConverter(xs)

  final class MapCaseConverter(val xm: Map[String, _]) {
    def toLowerCase() = Casing.toCaseOnMap(xm, LowerCase)
    def toUpperCase() = Casing.toCaseOnMap(xm, UpperCase)
    def toLowerCamelCase() = Casing.toCaseOnMap(xm, LowerCamelCase)
    def toUpperCamelCase() = Casing.toCaseOnMap(xm, UpperCamelCase)
    def toLowerSnakeCase() = Casing.toCaseOnMap(xm, LowerSnakeCase)
    def toUpperSnakeCase() = Casing.toCaseOnMap(xm, UpperSnakeCase)
    def toLowerDashCase() = Casing.toCaseOnMap(xm, LowerDashCase)
    def toUpperDashCase() = Casing.toCaseOnMap(xm, UpperDashCase)
  }

  implicit def map2CaseConverter(xm: Map[String, _]) =
    new MapCaseConverter(xm)


  ///////////////////////////////////////////////////////////////////////////
  // Case Objects
  ///////////////////////////////////////////////////////////////////////////

  sealed abstract class Casing

  case object LowerCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (s.forall { _.isLower }) Some(s) else None
    }

    override def toString() = "lower"
  }

  case object UpperCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (s.forall { _.isUpper }) Some(s) else None
    }

    override def toString() = "UPPER"
  }

  case object LowerCamelCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if(!s(0).isLower) None
      else if (s.forall { c => c != '-' && c != '_' }) Some(s) else None
    }

    override def toString() = "lowerCameCase"
  }

  case object UpperCamelCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (!s(0).isUpper) None
      else if (s.forall { c => c != '-' && c != '_' }) Some(s) else None
    }

    override def toString() = "UpperCamelCase"
  }

  case object LowerSnakeCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (s.forall { c => c == '_' || c.isLower }) Some(s) else None
    }

    override def toString() = "lower_snake_case"
  }

  case object UpperSnakeCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (s.forall { c => c == '_' || c.isUpper }) Some(s) else None
    }

    override def toString() = "UPPER_SNAKE_CASE"
  }

  case object LowerDashCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (!s(0).isLower) None
      else {
        var prevIsDash = false
        for (c <- s) {
          if (c == '_') return None
          if (prevIsDash && !c.isLower) return None
          prevIsDash = (c == '-')
        }
        Some(s)
      }
    }

    override def toString() = "lower-dash-case"
  }

  case object UpperDashCase extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (!s(0).isUpper) None
      else {
        var prevIsDash = false
        for (c <- s) {
          if (c == '_') return None
          if (prevIsDash && !c.isUpper) return None
          prevIsDash = (c == '-')
        }
        Some(s)
      }
    }

    override def toString() = "Upper-Dash-Case"
  }

  case object IgnoreCasing extends Casing {
    override def toString() = "IGNORE"
  }

  case object UnknownCasing extends Casing {
    override def toString() = "UNKOWN"
  }

  object Casing extends CasingParser

} // end package object
