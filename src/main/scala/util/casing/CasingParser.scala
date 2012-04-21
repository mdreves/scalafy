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
package scalafy.util.casing

/** Casing parser */
private[casing] trait CasingParser {

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
