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
package scalafy

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


  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  /** Casing parser */
  trait CasingParser {

    private def iter2Str(iter: Iterable[Char]) =
      iter.iterator.foldLeft("")(_ + _)

    /** Converts string casing.
      *
      * @param value a char iterator holding string data
      * @param casing casing to convert to
      * @param curCase current casing (UnknownCasing if unknown)
      * @param ignoreCasing list of names (in current case) to ignore
      * @return a string with new case
      */
    def toCase(
      value: Iterable[Char],
      casing: Casing,
      curCase: Casing = UnknownCasing,
      ignoreCasing: Seq[String] = List[String]()
    ): String = {
      if (casing == curCase || casing == IgnoreCasing) return iter2Str(value)

      if (!ignoreCasing.isEmpty) {
        val strValue = iter2Str(value)
        if (ignoreCasing.contains(strValue)) return strValue
        else return toCase(strValue, casing, curCase, Nil)
      }

      casing match {
        case UpperCase => value.foldLeft("")(_ + _.toUpper)
        case LowerCase => value.foldLeft("")(_ + _.toLower)
        case LowerSnakeCase => toLowerSnakeCase(value)
        case UpperSnakeCase => toUpperSnakeCase(value)
        case LowerDashCase => toLowerDashCase(value)
        case UpperDashCase => toUpperDashCase(value)
        case UpperCamelCase => toUpperCamelCase(value)
        case _ => toLowerCamelCase(value)
      }
    }

    /** Converts string casing for map of strings/values
      *
      * @param value a map of strings => values
      * @param casing casing to convert to
      * @param curCase current casing (UnknownCasing if unknown)
      * @param ignoreCasing list of names (in current casing) to ignore
      * @param convertValues true to convert values as well (if string value)
      * @return strings with new casing
      */
    def toCaseOnMap(
      value: Map[String, _],
      casing: Casing,
      curCase: Casing = UnknownCasing,
      ignoreCasing: Seq[String] = Nil,
      convertValues: Boolean = false
    ): Map[String, Any] = {
      if (casing == curCase || casing == IgnoreCasing) return value

      for ((k, v) <- value) yield
        (toCase(k, casing, curCase, ignoreCasing) ->
          (if (convertValues && v.isInstanceOf[String])
            toCase(v.asInstanceOf[String], casing, curCase, ignoreCasing)
          else
            v)
        )
    }

    /** Converts casing for sequence of strings
      *
      * @param value a seq of strings
      * @param casing current casing
      * @param curCase current casing (UnknownCasing if unknown)
      * @param ignoreCasing list of names to ignore casing for
      * @return strings with new casing
      */
    def toCaseOnSeq(
      value: Seq[String],
      casing: Casing,
      curCase: Casing = UnknownCasing,
      ignoreCasing: Seq[String] = Nil
    ): Seq[String] = {
      if (casing == curCase || casing == IgnoreCasing) return value

      value.map(toCase(_, casing, curCase, ignoreCasing))
    }


    /** Converts string to lowerCamelCase.
      *
      * @param value seq of chars
      * @return a string in lowerCamelCase format
      */
    def toLowerCamelCase(value: Iterable[Char]): String = {
      if (value == null) return null;

      val specialChars = List('=', '.', ',')
      var nextUpper = false
      var lastUpper = false

      processCasing(value, (result, c) =>
        if (result.length == 0 || specialChars.exists(_ == result.last)) {
          nextUpper = false
          lastUpper = c.isUpper
          result + c.toLower
        } else if (c == '-' || c == '_') {
          nextUpper = true
          result
        } else if (nextUpper) {
          nextUpper = false
          lastUpper = c.isUpper
          result + c.toUpper
        } else if (lastUpper) {
          lastUpper = c.isUpper
          result + c.toLower
        } else {
          lastUpper = c.isUpper
          result + c
        }
      )
    }

    /** Converts string to UpperCamelCase.
      *
      * @param value seq of chars
      * @return a string in UpperCamelCase format
      */
    def toUpperCamelCase(value: Iterable[Char]): String = {
      if (value == null) return null;

      val specialChars = List('=', '.', ',')
      var nextUpper = false
      var lastUpper = false

      processCasing(value, (result, c) =>
        if (result.length == 0 || specialChars.exists(_ == result.last)) {
          nextUpper = false
          lastUpper = c.isUpper
          result + c.toUpper
        } else if (c == '-' || c == '_') {
          nextUpper = true
          result
        } else if (nextUpper) {
          nextUpper = false
          lastUpper = c.isUpper
          result + c.toUpper
        } else if (lastUpper) {
          lastUpper = c.isUpper
          result + c.toLower
        } else {
          lastUpper = c.isUpper
          result + c
        }
      )
    }

    /** Converts string to lower_snake_case.
      *
      * @param value seq of chars
      * @return a string in lower_snake_case format
      */
    def toLowerSnakeCase(value: Iterable[Char]): String = {
      if (value == null) return null;

      val specialChars = List('=', '.', ',')
      var numericValue = ""
      var lastUpper = false

      var newValue = processCasing(value, (result, c) =>
        if (c.isDigit) {
          numericValue += c
          result
        } else {
          var newResult = result
          if (numericValue.length > 0) {
            if (result.length > 0 && !specialChars.exists(_ == result.last)) {
              newResult += '_'
            }
            newResult += numericValue
            numericValue = ""
          }
          if (c.isUpper) {
            if (result.length > 0 && !specialChars.exists(_ == result.last) &&
              !lastUpper && result.last != '_') {
              newResult += '_'
            }
            lastUpper = c.isUpper
            newResult + c.toLower
          } else if (c == '-') {
            lastUpper = false
            newResult + '_'
          } else {
            lastUpper = c.isUpper
            newResult + c
          }
        }
      )

      if (numericValue.length > 0) {
        if (newValue.length > 0) {
          newValue += "_"
        }
        newValue += numericValue
      }
      newValue
    }

    /** Converts string to UPPER_SNAKE_CASE.
      *
      * @param value seq of chars
      * @return a string in UPPER_SNAKE_CASE format
      */
    def toUpperSnakeCase(value: Iterable[Char]): String = {
      if (value == null) return null;

      toLowerSnakeCase(value).toUpperCase
    }

    /** Converts string to lower-dash-case.
      *
      * @param value seq of chars
      * @return a string in lower-dash-case format
      */
    def toLowerDashCase(value: Iterable[Char]): String = {
      toDashCase(value, false); 
    }

    def toUpperDashCase(value: Iterable[Char]): String = {
      toDashCase(value, true); 
    }

    def toDashCase(value: Iterable[Char], toUpper: Boolean): String = {
      if (value == null) return null;

      val specialChars = List('=', '.', ',')
      var numericValue = ""
      var lastUpper = false

      var newValue = processCasing(value, (result, c) =>
        if (c.isDigit) {
          numericValue += c
          result
        } else {
          var newResult = result
          if (numericValue.length > 0) {
            if (result.length > 0 && !specialChars.exists(_ == result.last) &&
                result.last != '-') {
              newResult += '-'
            }
            newResult += numericValue
            numericValue = ""
          }
          if (c.isUpper) {
            if (result.length > 0 && !specialChars.exists(_ == result.last) &&
                !lastUpper && result.last != '-') {
              newResult += '-'
            }
            lastUpper = c.isUpper
            if (newResult.length == 0 || newResult.last == '-' ||
                specialChars.exists(_ == result.last)) {
              newResult + (if (toUpper) c else c.toLower)
            } else {
              newResult + c.toLower
            }
          } else if (c == '_') {
            lastUpper = false
            newResult + '-'
          } else {
            lastUpper = c.isUpper
            if (result.length == 0 || specialChars.exists(_ == result.last) ||
                result.last == '-') {
              newResult + (if (toUpper) c.toUpper else c.toLower)
            } else {
              newResult + c.toLower
            }
          }
        }
      )

      if (numericValue.length > 0) {
        if (newValue.length > 0) {
          newValue += "-"
        }
        newValue += numericValue
      }
      newValue
    }

    /** Handles any escaping using double or single quotes */
    private def processCasing(
      value: Iterable[Char],
      foldLeftFn: (String, Char) => String
    ): String = {
      if (value == null) return null

      var inDoubleQuote = false
      var inSingleQuote = false
      value.foldLeft("") {
        (result, c) =>
          if (inDoubleQuote) {
            // Handle double quoted strings
            if (c == '"') inDoubleQuote = false
            result + c
          } else if (c == '"') {
            inDoubleQuote = true
            result + c
          } else if (inSingleQuote) {
            // Handle single quoted strings
            if (c == '\'') inSingleQuote = false
            result + c
          } else if (c == '\'') {
            inSingleQuote = true
            result + c
          } else {
            foldLeftFn(result, c)
          }
      }
    }
  }

} // end package object
