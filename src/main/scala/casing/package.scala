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
  * "my_string".toDashSeparated    // My-String
  * "My-String".toLowerUnderscore  // my_string
  * "my_string".toUpperUnderscore  // MY_STRING
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
  *   case LowerUnderscore(x) => println("lower_underscore: " + x)
  *   case UpperUnderscore(x) => println("UPPER_UNDERSCORE: " + x)
  *   case DashSeparated(x) => println("Dash-Separated: " + x)
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
    def toLowerUnderscore() = Casing.toLowerUnderscore(iter)
    def toUpperUnderscore() = Casing.toUpperUnderscore(iter)
    def toDashSeparated() = Casing.toDashSeparated(iter)
  }

  implicit def string2CaseConverter(s: String) = new IterableCaseConverter(s)
  implicit def iterable2CaseConverter(iter: Iterable[Char]) =
    new IterableCaseConverter(iter)

  final class SeqCaseConverter(val xs: Seq[String]) {
    def toLowerCase() = Casing.toCaseOnSeq(xs, LowerCase)
    def toUpperCase() = Casing.toCaseOnSeq(xs, UpperCase)
    def toLowerCamelCase() = Casing.toCaseOnSeq(xs, LowerCamelCase)
    def toUpperCamelCase() = Casing.toCaseOnSeq(xs, UpperCamelCase)
    def toLowerUnderscore() = Casing.toCaseOnSeq(xs, LowerUnderscore)
    def toUpperUnderscore() = Casing.toCaseOnSeq(xs, UpperUnderscore)
    def toDashSeparated() = Casing.toCaseOnSeq(xs, DashSeparated)
  }

  implicit def seq2CaseConverter(xs: Seq[String]) =
    new SeqCaseConverter(xs)

  final class MapCaseConverter(val xm: Map[String, _]) {
    def toLowerCase() = Casing.toCaseOnMap(xm, LowerCase)
    def toUpperCase() = Casing.toCaseOnMap(xm, UpperCase)
    def toLowerCamelCase() = Casing.toCaseOnMap(xm, LowerCamelCase)
    def toUpperCamelCase() = Casing.toCaseOnMap(xm, UpperCamelCase)
    def toLowerUnderscore() = Casing.toCaseOnMap(xm, LowerUnderscore)
    def toUpperUnderscore() = Casing.toCaseOnMap(xm, UpperUnderscore)
    def toDashSeparated() = Casing.toCaseOnMap(xm, DashSeparated)
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

  case object LowerUnderscore extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (s.forall { c => c == '_' || c.isLower }) Some(s) else None
    }

    override def toString() = "lower_underscore"
  }

  case object UpperUnderscore extends Casing {
    def unapply(s: String): Option[String] = {
      if (s.length == 0) Some(s)
      else if (s.forall { c => c == '_' || c.isUpper }) Some(s) else None
    }

    override def toString() = "UPPER_UNDERSCORE"
  }

  case object DashSeparated extends Casing {
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

    override def toString() = "Dash-Separated"
  }

  case object UnknownCasing extends Casing {
    override def toString() = "UNKOWN"
  }

  object Casing extends CasingHelpers


  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  /** Casing helper methods */
  trait CasingHelpers {

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
      if (casing == curCase) return iter2Str(value)

      if (!ignoreCasing.isEmpty) {
        val strValue = iter2Str(value)
        if (ignoreCasing.contains(strValue)) return strValue
        else return toCase(strValue, casing, curCase, Nil)
      }

      casing match {
        case UpperCase => value.foldLeft("")(_ + _.toUpper)
        case LowerCase => value.foldLeft("")(_ + _.toLower)
        case LowerUnderscore => toLowerUnderscore(value)
        case UpperUnderscore => toUpperUnderscore(value)
        case DashSeparated => toDashSeparated(value)
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
      if (casing == curCase) return value

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
      if (casing == curCase) return value

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

    /** Converts string to lower_underscore.
      *
      * @param value seq of chars
      * @return a string in lower_underscore format
      */
    def toLowerUnderscore(value: Iterable[Char]): String = {
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

    /** Converts string to UPPER_UNDERSCORE.
      *
      * @param value seq of chars
      * @return a string in UPPER_UNDERSCORE format
      */
    def toUpperUnderscore(value: Iterable[Char]): String = {
      if (value == null) return null;

      toLowerUnderscore(value).toUpperCase
    }

    /** Converts string to Dash-Separated.
      *
      * @param value seq of chars
      * @return a string in Dash-Separated format
      */
    def toDashSeparated(value: Iterable[Char]): String = {
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
              newResult + c
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
              newResult + c.toUpper
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
