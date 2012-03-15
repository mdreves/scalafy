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

import casing._
import types.{Reifiable, ReifiableSettings}

/** Contains utils for JSON matching and conversion.
  *
  * The main difference from other libraries is there are no JField, JInt,...
  * Everthing is parsed to standard primitives. Additional features include
  * field casing conversion, ability to use Any and have parser choose the
  * best type, ability to use Any option in combination with Reifiable.
  *
  * The following is a summary of features:
  * {{{
  * // Parsing
  * fromJson[List[Int]]("[1,2,3]")           // Option[List[Int]] 
  * toJson(List(1,2,3))                      // "[1,2,3]"
  * fromJson[List[Short]]("[1,2,3]")         // Option[List[Short]] 
  * toJson(List[Short](1,2,3))               // "[1,2,3]"
  * fromJson[List[Long]]("[1,2,3]")          // Option[List[Long]]
  * toJson(List[Long](1,2,3))                // "[1,2,3]"
  * fromJson[List[Float]]("[1.0,2.0]")       // Option[List[Float]]
  * toJson(List[Float](1.0,2.0))             // "[1.0,2.0]"
  * fromJson[List[Double]]("[1.0,2.0]")      // Option[List[Double]]
  * toJson(List[Double](1.0,2.0))            // "[1.0,2.0]"
  * fromJson[List[Boolean]]("[true, false]") // Option[List[Boolean]]
  * toJson(List[Boolean](true, false))       // "[true,false]"
  * fromJson[List[Char]]("[\"a\",\"b\"]")    // Option[List[Char]]
  * toJson(List[Char]('a','b'))              // "[\"a\",\"b\"]"
  * fromJson[List[String]]("[\"a\",\"b\"]")  // Option[List[String]]
  * toJson(List[Char]("a","b"))              // "[\"a\",\"b\"]"
  *
  * fromJson[Map[String,String]]("{\"foo\": \"bar\"}")
  * toJson(Map("foo" -> "bar"))              // "{\"foo\": \"bar\"}"
  * fromJson[Map[String,List[Int]]]("{\"foo\": [1,2,3]}")
  * toJson(Map("foo" -> [1,2,3]))            // "{\"foo\": [1,2,3]}"
  *
  * // Parsing (with field casing conversion)
  * // Some(Map("myField" -> 1))
  * fromJson[Map[String,Int]]("{\"MyField\": 1}", LowerCamelCase)
  * toJson(Map("myField" -> 1), UpperCamelCase) // "{\"MyField\": 1}"
  *
  * // Some(Map("my_field" -> 1))
  * fromJson[Map[String,Int]]("{\"MyField\": 1}", LowerSnakeCase)
  * toJson(Map("MyField" -> 1), LowerSnakeCase) // "{\"my_field\": 1}"
  *
  * // Parsing (using Any - chooses best type)
  * fromJson[Any]("[1,2,3]")         // Option[Any] (points to List[Int]) 
  * fromJson[Any]("[1,9999999999]")  // Option[Any] (points to List[Long]) 
  * fromJson[List[Any]]("[1,2,3]")   // Option[List[Any]] (points to List[Int]) 
  * fromJson[Map[String,Any]]("{\"foo\": \"bar\"}")  // ...  Map[String,String] 
  * fromJson[Any]("{\"foo\": 1}")    // Option[Any] (points to Map[String,Int]) 
  *
  * // Parsing (using Any in combination with Reifiable)
  * implicit val reifiableSettings = ReifiableSettings(true)
  *
  * val v = fromJson[Any]("[1,9999999999]") 
  * v.isTypeOf[List[Int]]            // false 
  * v.isTypeOf[List[Long]]           // true 
  *
  * val v = fromJson[Any]("{\"foo\": 1}") 
  * v.isTypeOf[Map[String,String]]   // false 
  * v.isTypeOf[Map[String,Int]]      // true
  *
  * // Parsing (using other container types)
  * fromJson[Vector[Int]]("[1,2,3]") // Option[Vector[Int]] 
  * toJson(Vector(1,2,3))            // "[1,2,3]"
  * fromJson[Seq[Int]]("[1,2,3]")    // Option[Seq[Int]] 
  * toJson(Seq(1,2,3))               // "[1,2,3]"
  *
  * // Parsing (pretty printing)
  * // "{
  * //    \"foo\": \"bar\"
  * //  }"
  * toJson(Map("foo" -> "bar"), true, 2) 
  *
  * // Extracting
  * "{\"foo\": 1}" match {
  *   case Json(xm) =>
  *     println("matched: " + xm)    // prints: Map(foo -> 1) 
  * }
  *
  * // Combining with other types
  * // Map("foo" -> "bar", "foo2" -> "bar2")
  * Map("foo" -> "bar").withJson("{\"foo2\": \"bar2\"}") { _ ++ _ }
  *
  * // Configuring default settings
  * implicit val prettyPrintSettings = PrettyPrintSettings(true, 2)
  * implicit val fieldCasing = LowerCamelCase
  * implicit val reifiableSettings = ReifiableSettings(false) 
  * }}}
  */
package object json {

  ///////////////////////////////////////////////////////////////////////////
  // vals and implicits 
  ///////////////////////////////////////////////////////////////////////////


  implicit val fieldCasing = IgnoreCasing
  implicit val reifiableSettings = ReifiableSettings(false)

  case class PrettyPrintSettings(enabled: Boolean, indent: Int)
  implicit val prettyPrintSettings = PrettyPrintSettings(false, 2)


  ///////////////////////////////////////////////////////////////////////////
  // Package Functions
  ///////////////////////////////////////////////////////////////////////////

  /** Converts RFC 4627 compatible JSON data to a primitive, List, or Map.
    *
    * Primitives supported are String, Int, Long, Double, or Boolean. Long is
    * only used if an integral value does not fit into an Int. Lists can
    * contain primitives, other Lists, or Maps.  If a List contains one more
    * Longs and the rest are Ints, the entire List will converted to a List of
    * Longs.  Maps are from String to either primitive, List, or another
    * Map.
    * 
    * Examples:
    * {{{
    * // Type is List[Int]
    * val listOfInt = fromJson[List[Int]]("[1, 2, 3]")
    *
    * // Type is Any pointing to default List[Int] type
    * val listOfInt = fromJson[Any]("[1, 2, 3]")
    *
    * // Type is Map[String, List[Int]]
    * val mapOfStringToListOfInt = 
    *   fromJson[Map[String, List[Int]]]("{ \"foo\": [1, 2, 3] }")
    * }}}
    *
    * Key names used in Maps will be automatically converted to 
    * lowerCamelCase, but a specific casing can be provided by setting 
    * the implicit fieldCasing parameter. For example:
    * {{{
    * implicit val fieldCasing = LowerSnakeCase
    * // or disable conversion
    * implicit val fieldCasing = IgnoreCasing
    * }}}
    *
    * @param json JSON string 
    * @return List, Map, or Primitive 
    */
  def fromJson[A](json: Iterable[Char])(
    implicit m: Manifest[A], 
    fieldCasing: Casing, 
    reifiableSettings: ReifiableSettings 
  ): Option[A] = Json.parser.fromJson[A](
      json)(m, fieldCasing, reifiableSettings) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }

  /** to[A] with an explicit casing for fields */
  def fromJson[A](json: Iterable[Char], fieldCasing: Casing)(
    implicit m: Manifest[A], reifiableSettings: ReifiableSettings 
  ): Option[A] = {
    fromJson[A](json)(m, fieldCasing, reifiableSettings)
  }

  /** Converts given value to an RFC 4627 compatible JSON string
    *
    * Typically the value passed will be a List of values or a Map of
    * values. However, this method will also support passing a primitive 
    *
    * Examples:
    * {{{
    * toJson(List(1,2,3))      // "[1,2,3]"
    * toJson(Map("foo" -> 1))  // "{\"foo\":1}"
    * }}}
    *
    * @param value values (typically List or Map)
    * @param pretty print optional pretty printing 
    * @return string JSON data
    */
  def toJson(x: Any)(
    implicit prettyPrintSettings: PrettyPrintSettings, fieldCasing: Casing
  ): String = 
    Json.parser.toJson(x)(prettyPrintSettings, fieldCasing)

  /** toJson with explicit pretty printing settings */
  def toJson(x: Any, prettyPrint: Boolean, indent: Int = 2)(
    implicit fieldCasing: Casing
  ): String = 
    toJson(x)(PrettyPrintSettings(prettyPrint, indent), fieldCasing)

  /** toJson with explicit casing setting */
  def toJson(x: Any, fieldCasing: Casing)(
    implicit prettyPrintSettings: PrettyPrintSettings
  ): String = 
    toJson(x)(prettyPrintSettings, fieldCasing)


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  object Json {
    private[json] val parser = new JsonParser() {}
  
    def unapply(json: String)(
      implicit fieldCasing: Casing, reifiableSettings: ReifiableSettings 
    ): Option[Any] = {
      fromJson[Any](json)(manifest[Any], fieldCasing, reifiableSettings)
    }
  }

  object IterableJson {
    /** Extractor for Json for when data input as iterable.
      *
      * NOTE: Because of type erasure this will not match on _* patterns.
      */
    def unapply(json: Iterable[Char])(
      implicit fieldCasing: Casing, reifiableSettings: ReifiableSettings 
    ): Option[Any] = {
      fromJson[Any](json)(manifest[Any], fieldCasing, reifiableSettings)
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  implicit def any2JsonPimp[A](x: A)(implicit 
    m: Manifest[A], 
    fieldCasing: Casing, 
    reifiableSettings: ReifiableSettings,
    prettyPrintSettings: PrettyPrintSettings
  ) = new JsonPimp[A](x, m, fieldCasing, reifiableSettings, prettyPrintSettings)

  class JsonPimp[A](
    val cur: A,
    val m: Manifest[A], 
    val fieldCasing: Casing,
    val reifiableSettings: ReifiableSettings,
    val prettyPrintSettings: PrettyPrintSettings 
  ) {
    /** Applies operator to existing type and type extracted from JSON
      * 
      * @param json json data
      * @param op operation taking current type and type converted from JSON
      * @return type from apply operation or current type if conversion fails
      */
    def withJson(json: Iterable[Char])(op: (A, A) => A): A = {
      withJson(json, fieldCasing)(op)
    }
  
    /** withJson with an explicit casing for fields */
    def withJson(
      json: Iterable[Char], fieldCasing: Casing
    )(op: (A,A) => A): A = {
      fromJson[A](json)(m, fieldCasing, reifiableSettings) match {
        case Some(v) => op(cur, v) 
        case _ => cur
      }
    }

    def toJson(): String = 
      scalafy.json.toJson(cur)(prettyPrintSettings, fieldCasing)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  /** JSON parser */
  trait JsonParser {
    val errorPrefix = "JSON parse error :: "

    /** Parser for fromJson package function
      *
      * @return either String error msg or data of given type
      */
    def fromJson[A](json: Iterable[Char])(
      implicit m : Manifest[A], 
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ): Either[String, A] = {

      var readCount = 0

      def formatError(msg: String) = 
        errorPrefix + msg + " (chars read: " + readCount + ")"

      // Wrap iterator in another iterator so we can count chars read
      val bufIter = new BufferedIterator[Char] () {
        val iterator = json.iterator.buffered

        def hasNext = iterator.hasNext
        def head = iterator.head
        def next() = {
          readCount += 1
          iterator.next()
        }
      }

      val isWhitespace = (c: Char) => 
        c == ' ' || c == '\n' || c == '\t' || c == '\r'
        
      // Skip over any leading whitespace
      while (bufIter.hasNext && isWhitespace(bufIter.head)) bufIter.next()

      if (!bufIter.hasNext) return Left(errorPrefix + "empty input")
      
      bufIter.head match {
        case '[' => 
          parseJsonList(bufIter)(m, fieldCasing, reifiableSettings) match {
            case Right(xs) => Right(xs.getObj().asInstanceOf[A])
            case Left(l) => Left(formatError(l)) 
          }

        case '{' => 
          parseJsonObject(bufIter)(m, fieldCasing, reifiableSettings) match {
            case Right(mx) => Right(mx.getObj().asInstanceOf[A])
            case Left(l) => Left(formatError(l)) 
          }

        case '"' => 
          parseJsonString(bufIter)(m) match {
            case Right(r) => Right(r.asInstanceOf[A]) 
            case Left(l) => Left(formatError(l))
          }

        case 'n' => 
          parseJsonNull(bufIter)(m) match {
            case Right(r) => Right(r.asInstanceOf[A])
            case Left(l) => Left(formatError(l))
          }

        case c: Char if (c == '-' || c.isDigit) => 
          parseJsonNumber(bufIter)(m) match {
            case Right(r) => Right(r.asInstanceOf[A])
            case Left(l) => Left(formatError(l))
          }

        case c: Char if (c == 't' || c == 'f') => 
          parseJsonBoolean(bufIter)(m) match {
            case Right(r) => Right(r.asInstanceOf[A])
            case Left(l) => Left(formatError(l))
          }

        case o => Left(formatError("unexpected data: " + bufIter.mkString))
      }
    }

    /** Parser for toJson package function */
    def toJson(value: Any)(
      implicit prettyPrintSettings: PrettyPrintSettings, fieldCasing: Casing
    ): String = {
      val prettyPrint = prettyPrintSettings.enabled
      val indent = prettyPrintSettings.indent

      // Prints output to buffer
      def printToBuf(buf: StringBuilder, value: Any, offset: Int) {
        if (value == null) append(buf, "null", 0)
        else if (value.isInstanceOf[String] || value.isInstanceOf[Char]) {
          append(buf, "\"" + value.toString + "\"", 0)
        } else if (value.isInstanceOf[Int] || value.isInstanceOf[Short] ||
            value.isInstanceOf[Long] || value.isInstanceOf[Float] ||
            value.isInstanceOf[Double] || value.isInstanceOf[Byte] ||
            value.isInstanceOf[Boolean]) {
          append(buf, value.toString, 0)
        } else if (value.isInstanceOf[Seq[_]]) {
          append(buf, "[", 0)
          for (x <- value.asInstanceOf[Seq[_]]) { 
            printToBuf(buf, x, 0) 
            append(buf, ",", 0)
            if (prettyPrint) append(buf, " ", 0)
          }
          if (prettyPrint) buf.length -= 2 else buf.length -= 1
          append(buf, "]", 0)
        } else if (value.isInstanceOf[Map[_,_]]) {
          append(buf, "{", 0)
          if (prettyPrint) append(buf, "\n", 0)
          for ((k,v) <- value.asInstanceOf[Map[_,_]]) {
            val name = 
              if (fieldCasing != IgnoreCasing)
                Casing.toCase(k.toString, fieldCasing) 
              else k.toString

            append(buf, "\"" + name + "\":", offset + indent)
            if (prettyPrint) append(buf, " ", 0)

            printToBuf(buf, v, offset + indent)
            append(buf, ",", 0)
            if (prettyPrint) append(buf, "\n", 0)
          }
          if (prettyPrint) buf.length -= 2 else buf.length -= 1
          if (prettyPrint) append(buf, "\n", 0)
          append(buf, "}", offset)
        }
        else throw new Error("toJson not supported for type: " + value) 
      }

      // Adds spaces
      def append(buf: StringBuilder, s: String, offset: Int) =  {
        if (prettyPrint) for (i <- 0 until offset) buf.append(" ")
        buf.append(s)
      }

      val buf = new StringBuilder()
      printToBuf(buf, value, 0)
      buf.toString
    }

    // Internal helpers

    /** Parses JSON string (can be parsed as String or Char) */
    private def parseJsonString[A : Manifest](
      iter: BufferedIterator[Char]
    ): Either[String, A] = {
      if (!(manifest[A] >:> manifest[String]) && 
          manifest[A] != manifest[Char]) {
        return Left("type mismatch: expecting " + manifest[A] + " not String")
      }

      if (!iter.hasNext || iter.head != '"') 
        return Left("invalid String") 

      iter.next()
      var s = new StringBuilder
      var isEscape = false
      var quoteCount = 1
      while(iter.hasNext && quoteCount > 0) {
        val c = iter.next()
        if (isEscape) { isEscape = false; s + c }
        else if (c == '\\') isEscape = true
        else if (c == '"') {
          quoteCount -= 1
          if (quoteCount > 0) s + c
        } else s + c
      }
      
      if (quoteCount == 0) {
        if (manifest[A] == manifest[Char]) {
          if (s.length == 1) Right(s.toString()(0).asInstanceOf[A])
          else Left("type mismatch: expecting Char not String")
        } else Right(s.toString().asInstanceOf[A])
      }
      else Left("invalid String '" + s.toString + "'") 
    }

    /** Parses JSON Number */
    private def parseJsonNumber[A : Manifest](
      iter: BufferedIterator[Char]
    ): Either[String, A] = {
      val typeName =
        if (manifest[A] == manifest[Any]) "Number"
        else if (manifest[A] == manifest[Nothing]) "Number"
        else if (manifest[A] == manifest[Int]) "Int"
        else if (manifest[A] == manifest[Short]) "Short"
        else if (manifest[A] == manifest[Long]) "Long"
        else if (manifest[A] == manifest[Float]) "Float"
        else if (manifest[A] == manifest[Double]) "Double"
        else if (manifest[A] == manifest[Byte]) "Byte"
        else return Left(
          "type mismatch expecting " + manifest[A] + " not number")

      if (!iter.hasNext || !(iter.head == '-' || iter.head.isDigit)) 
        return Left("empty " + typeName)

      var s = new StringBuilder()

      // Skip over minus sign (NOTE: json does not allow starting with +)
      if (iter.head == '-') {
        s + iter.head
        if (iter.hasNext) iter.next() 
        else return Left("invalid " + typeName) 
      }

      val isDoubleChar = (c: Char) =>
        c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+'

      var next = iter.head
      var isDouble = false
      while(iter.hasNext && (next.isDigit || isDoubleChar(next))) {
        if (isDoubleChar(next)) isDouble = true
        s + iter.next()
        if (iter.hasNext) next = iter.head
      }

      // Handle case where Any was passed and we must find best match ourselves
      if (manifest[A] == manifest[Any] || manifest[A] == manifest[Nothing]) {
        if (isDouble) parseNumber[Double](s.toString) match {
          case Right(d) => Right(d.asInstanceOf[A])
          case Left(l) => Left("invalid Double, " + l)
        } else parseNumber[Int](s.toString) match {
          case Right(i) => Right(i.asInstanceOf[A])
          case Left(l) => parseNumber[Long](s.toString) match { // try long
            case Right(l) => Right(l.asInstanceOf[A])
            case Left(l) => Left("invalid Int, " + l)
          }
        }
      } else parseNumber[A](s.toString) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left("invalid " + typeName + ", " + l)
      }
    }

    /** Parses JSON true or false */
    private def parseJsonBoolean[A : Manifest](
      iter: BufferedIterator[Char]
    ): Either[String, Boolean] = {
      if (!(manifest[A] >:> manifest[Boolean])) {
        return Left("type mismatch expecting " + manifest[A] + " not Boolean")
      }

      if (!iter.hasNext || !(iter.head == 't' || iter.head == 'f')) 
        return Left("invalid Boolean") 

      val isTrue = iter.head == 't'
      var expectedChars = if (isTrue) Iterator('t','r','u','e').buffered
                          else Iterator('f','a','l','s','e').buffered
      while(iter.hasNext && expectedChars.hasNext &&
            iter.head == expectedChars.head) {
        iter.next()
        expectedChars.next()
      }
      if (expectedChars.hasNext) Left("invalid Boolean") else Right(isTrue)
    }

    /** Parses JSON null */
    private def parseJsonNull[A : Manifest](
      iter: BufferedIterator[Char]
    ): Either[String, A] = {
      if (manifest[A] <:< manifest[AnyVal]) {
        return Left("type mismatch expecting " + manifest[A] + " not null")
      }

      if (!iter.hasNext || iter.head != 'n') 
        return Left("invalid characters for null") 

      var expectedChars = Iterator('n','u','l','l').buffered
      while(iter.hasNext && expectedChars.hasNext &&
            iter.head == expectedChars.head) {
        iter.next()
        expectedChars.next()
      }
      if (expectedChars.hasNext) Left("invalid characters for null") 
      else Right(null).asInstanceOf[Either[String,A]]
    }

    /** Parses JSON List */
    private def parseJsonList[A](
      iter: BufferedIterator[Char]
    )(implicit 
      m: Manifest[A], 
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ): Either[String, ObjectWithManifest[A]] = {
      val itemType = getItemType(m) match {
        case Some(v) => v
        case None => return Left("invalid list type: " + m) // error
      }

      // If specific type not given, default to List
      val xsEither =
        if (m == manifest[Any] || m == manifest[Nothing])
          ObjectWithManifest[List[Any]]()
        else ObjectWithManifest()(m, reifiableSettings)

      var xs = xsEither match {
        case Right(v) => v  // success
        case Left(l) => return Left(l) // error
      }

      // Now parse the list...
      parseJsonListOrObject(iter, {
        case x => validateType(x, itemType) match {
          case None => xs.add(x); None // success 
          case Some(v) => Some(v) // error
        }
      })(m, fieldCasing, reifiableSettings) match {
        case None => Right(xs.asInstanceOf[ObjectWithManifest[A]])  // success
        case Some(v) => Left(v) // error
      }
    }

    /** Parses JSON Object */
    private def parseJsonObject[A](
      iter: BufferedIterator[Char]
    )(implicit 
      m: Manifest[A], 
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ): Either[String, ObjectWithManifest[A]] = {
      val itemType = getItemType(m) match {
        case Some(v) => v
        case None => return Left("invalid object type: " + m)  // error
      }

      // If specific type not given, default to Map 
      val xmEither =
        if (m == manifest[Any] || m == manifest[Nothing])
          ObjectWithManifest[Map[String, Any]]()
        else ObjectWithManifest()(m, reifiableSettings)

      var xm = xmEither match {
        case Right(v) => v  // success
        case Left(l) => return Left(l) // error
      }

      // Now parse the object...
      parseJsonListOrObject(iter, {
        case (n: String, x) => validateType(x, itemType) match {
          case None => xm.add(n, x); None // success
          case Some(v) => Some(v) // error
        }
        case o => Some("unexpected data: " + o) 
      })(m, fieldCasing, reifiableSettings) match {
        case None => Right(xm.asInstanceOf[ObjectWithManifest[A]])  // success
        case Some(v) => Left(v) // error
      }
    }

    /** Parses JSON List or Object 
     * 
     * @return None if success String if error
     */
    private def parseJsonListOrObject[A](
      iter: BufferedIterator[Char], 
      pf: PartialFunction[Any,Option[String]]  // String if error, else None
    )(implicit 
      m: Manifest[A], 
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ): Option[String] = {
      if (!iter.hasNext || !(iter.head == '[' || iter.head == '{')) 
        return Some("missing input") 

      val isList = iter.head == '['
      iter.next() // Skip '{' or '['
      var braceCount = 1
      var error: Option[String] = None 
      var name: String = null

      val itemType = getItemType(m) match {
        case Some(v) => v
        case None => return Some("invalid object type: " + m)
      }

      while (iter.hasNext && braceCount > 0 && error == None) iter.head match {
        // end of obj
        case '}' if (!isList) => iter.next(); braceCount -= 1

        // end of list
        case ']' if (isList) => iter.next(); braceCount -= 1

        // whitespace around structure characters (ignore)
        case ' ' => iter.next()
        case '\t' => iter.next()
        case '\n' => iter.next()
        case '\r' => iter.next()

        // list/obj value separator (ignore)
        case ',' => iter.next()

        // obj field name
        case '"' if (!isList && name == null) =>
          // Convert to proper field casing at same time we parse
          fieldCasing match {
            case fc: Casing if (fc != IgnoreCasing) => {
              val fieldIter = new FieldIterable(iter)
              name = Casing.toCase(fieldIter, fc, null, Nil)
              name = name.stripSuffix("\"")
              if (fieldIter.iterator.isInvalidData) 
                error = Some("invalid field name '" + name + "'")
            }
            case _ => parseJsonString(iter)(manifest[String]) match {
              case Right(value) => name = value
              case Left(l) => error = Some("invalid field name") 
            }
          }
          if (error == None) {
            if (!iter.hasNext || iter.head != ':')
              error = Some("invalid field: missing ':' after " + name) 
            else iter.next() // skip over ':'
          }

        // obj/list string value
        case '"' =>
          parseJsonString(iter)(itemType) match {
            case Right(value) => {
              if (isList) error = pf(value)
              else { error = pf((name, value)); name = null }
            }
            case Left(l) => error = Some(l) 
          }

        // if not list, name must be set
        case _ if (!isList && name == null) =>
          error = Some("invalid field: missing name")

        // obj/list number value
        case c: Char if (c == '-' || c.isDigit) =>
          parseJsonNumber(iter)(itemType) match {
            case Right(value) => 
              if (isList) error = pf(value)
              else { error = pf((name, value)); name = null }
            case Left(l) => error = Some(l) 
          }

        // obj/list boolean value
        case c: Char if (c == 't' || c == 'f') =>
          parseJsonBoolean(iter)(itemType) match {
            case Right(value) => {
              if (isList) error = pf(value)
              else { error = pf((name, value)); name = null }
            }
            case Left(l) => error = Some(l) 
          }

        // object/list null value
        case 'n' =>
          parseJsonNull(iter)(itemType) match {
            case Right(value) => {
              if (isList) error = pf(value)
              else { error = pf((name, value)); name = null }
            }
            case Left(l) => error = Some(l) 
          }

        // start of embedded object
        case '{' =>
          parseJsonObject(iter)(itemType, fieldCasing, reifiableSettings) match{
            case Right(value) => {
              if (isList) error = pf(value)
              else { error = pf((name, value)); name = null }
            }
            case Left(l) => error = Some(l) 
          }

        // start of embedded list
        case '[' =>
          parseJsonList(iter)(itemType, fieldCasing, reifiableSettings) match {
            case Right(value) => {
              if (isList) error = pf(value)
              else { error = pf((name, value)); name = null }
            }
            case Left(l) => error = Some(l) 
          }

        case x => error = Some("unexpected character: " + x) 
      }

      if (error == None && braceCount != 0) error = Some("missing brace")
      
      error
    }

    // Internal helper methods

    /** Parses number */
    private def parseNumber[A : Manifest](s: String): Either[String, A] = try {
      if (manifest[A] == manifest[Short]) 
        Right(s.toShort.asInstanceOf[A])
      else if (manifest[A] == manifest[Int]) 
        Right(s.toInt.asInstanceOf[A])
      else if (manifest[A] == manifest[Long]) 
        Right(s.toLong.asInstanceOf[A])
      else if (manifest[A] == manifest[Float]) 
        Right(s.toFloat.asInstanceOf[A])
      else if (manifest[A] == manifest[Double]) 
        Right(s.toDouble.asInstanceOf[A])
      else if (manifest[A] == manifest[Byte]) 
        Right(s.toByte.asInstanceOf[A])
      else Left("unsupported type: " + manifest[A]) // Can't happen
    } catch {
      case e: Exception => Left(e.getMessage())
    }

    /** Gets item type given manifest for List or Map */ 
    private def getItemType(m: Manifest[_]): Option[Manifest[_]] = {
      if (m == manifest[Any] || m == manifest[Nothing]) Some(m)
      else if (m.typeArguments.length == 2) Some(m.typeArguments(1)) // Map
      else if (m.typeArguments.length == 1) Some(m.typeArguments(0)) // List
      else None
    }

    /** Validates type against manifest.
      *
      * @return None if success full or String if error
      */
    private def validateType[A](x: A, m: Manifest[_]): Option[String] = {
      if (m == manifest[Any] || m == manifest[Nothing]) None // check early
      else if (x == null) {  // Check null
        if (!isNullAllowed(m)) 
          Some("type mismatch: expecting " + m + " not null")
        else None
      }
      else if (x.isInstanceOf[String]) {
        // Special case for Chars since Char data passed as Strings
        if (m == manifest[Char] && x.asInstanceOf[String].length != 1)
          Some("type mismatch: expecting Char not String") 
        else if (m != manifest[String]) 
          Some("type mismatch: expecting " + m + " not String")
        else None
      }
      else if (x.isInstanceOf[Short] && m != manifest[Short]) 
        Some("type mismatch: expecting " + m + " not Short") 
      else if (x.isInstanceOf[Int] && m != manifest[Int]) 
        Some("type mismatch: expecting " + m + " not Int") 
      else if (x.isInstanceOf[Long] && m != manifest[Long]) 
        Some("type mismatch: expecting " + m + " not Long") 
      else if (x.isInstanceOf[Float] && m != manifest[Float]) 
        Some("type mismatch: expecting " + m + " not Float") 
      else if (x.isInstanceOf[Double] && m != manifest[Double]) 
        Some("type mismatch: expecting " + m + " not Double") 
      else if (x.isInstanceOf[Boolean] && m != manifest[Boolean]) 
        Some("type mismatch: expecting " + m + " not Boolean")
      else if (x.isInstanceOf[ObjectWithManifest[_]]) {
        // Have ObjectWithManifest do the checking 
        x.asInstanceOf[ObjectWithManifest[_]].validateType(m)
      }
      else None
    }

    /** Return true if type allowed to be null */
    private def isNullAllowed(m: Manifest[_]): Boolean = m <:< manifest[AnyRef] 

    /**
     * Encapsulates an object (List, Map, ...) along with its required manifest
     * and the manifest for the types actually read.
     *
     * By using this class, callers of the fromJson function can pass Any
     * for the type and we will automatically choose the best type for
     * them based on what data was actually read. If the reifiableSettings
     * flag is also set then the actual manifest information can be stored for
     * comparisons later.
     */
    private class ObjectWithManifest[A](
      private var obj: A, 
      val objType: Manifest[A],  // type required for object[item]
      val reifiableSettings: ReifiableSettings
    ) {
      private var actualItemType: Manifest[_] = null // actual item type used

      /** Gets object.
        *
        * NOTE: If a Any was used, then the type returned is still of
        *       type List[Any], etc. Only the manifest is updated to
        *       a track the more specific type so that a safe cast can
        *       be done.
        */
      def getObj(): A = {
        // Reverse the List if List used
        val result = 
          if (obj.isInstanceOf[List[_]]) obj.asInstanceOf[List[_]].reverse
          else obj

        if (reifiableSettings.enabled) Reifiable(getManifest(), result)
        result.asInstanceOf[A]
      }

      /** Manifest of actual data stored.
        * 
        * NOTE: This will only be different than the object's type if
        *       Any was used AND a better type was determined.
        */
      def getManifest() = {
        // get is safe, we know we must be a container type
        if (getItemType(objType).get != manifest[Any] &&
            actualItemType != manifest[Any]) {
          // We were given a specific type or Any is as good as it gets
          objType
        } else {
          upgradeType(actualItemType)
        }
      }

      /** Add for lists */
      def add[T](item: T): Unit = {
        // Unpack any embedded objects
        val newItem = 
          if (item.isInstanceOf[ObjectWithManifest[_]]) 
            item.asInstanceOf[ObjectWithManifest[_]].getObj() 
          else item

        // Add to our container
        if (obj.isInstanceOf[List[_]]) {
          obj = (newItem :: obj.asInstanceOf[List[_]]).asInstanceOf[A]
        } else if (obj.isInstanceOf[Vector[_]]) {
          obj = (obj.asInstanceOf[Vector[_]] :+ newItem).asInstanceOf[A]
        } else if (obj.isInstanceOf[Seq[_]]) {
          obj = (obj.asInstanceOf[Seq[_]] :+ newItem).asInstanceOf[A] 
        } else {
          throw new Error("Unknown type")  // can't happen
        }

        updateActualType(item)
      }

      /** Add for maps */
      def add[T](name: String, item: T): Unit = {
        // Unpack any embedded objects
        val newItem = 
          if (item.isInstanceOf[ObjectWithManifest[_]]) 
            item.asInstanceOf[ObjectWithManifest[_]].getObj() 
          else item

        // Add to our container
        if (obj.isInstanceOf[Map[_,_]]) {
          obj = (obj.asInstanceOf[Map[String,Any]] + 
            (name -> newItem)).asInstanceOf[A]  
        } else {
          throw new Error("Unknown type")  // can't happen
        }

        updateActualType(item)
      }

      /** Updates actual type to what was read */
      def updateActualType[T](item: T): Unit = item match {
        // We only parse either String, Int, Long, Double, or Boolean
        case _: String =>
          if (actualItemType == null) 
            actualItemType = manifest[String]
          else if (actualItemType != manifest[String]) 
            actualItemType = manifest[Any]
        case _: Int => 
          if (actualItemType == null) 
            actualItemType = manifest[Int]
          else if (actualItemType == manifest[Long])
            actualItemType = manifest[Long]
          else if (actualItemType != manifest[Int]) 
            actualItemType = manifest[Any]
        case _: Long =>
          if (actualItemType == null) 
            actualItemType = manifest[Long]
          else if (actualItemType == manifest[Int])
            actualItemType = manifest[Long]
          else if (actualItemType != manifest[Long])
            actualItemType = manifest[Any]
        case _: Double => 
          if (actualItemType == null) 
            actualItemType = manifest[Double]
          else if (actualItemType != manifest[Double]) 
            actualItemType = manifest[Any]
        case _: Boolean => 
          if (actualItemType == null) 
            actualItemType = manifest[Boolean]
          else if (actualItemType != manifest[Boolean]) 
            actualItemType = manifest[Any]
        case o: ObjectWithManifest[_] =>
          if (actualItemType == null)
            actualItemType = o.getManifest()
          else if (actualItemType != o.getManifest())
            actualItemType = manifest[Any]
        case x =>
          // At this point the validation checks have passed so 
          // we will only get types that are valid or don't match
          // the above. Although an valid type might be more specific 
          // the actual type no longer matters so set to Any 
          // (if null wait for a more specific type)
          if (x != null) actualItemType = manifest[Any]
      }

      /** Verifies manifest is of this container type */
      def validateType(m: Manifest[_]): Option[String] = {
        // Helper function to compare manifests
        def compareTypes(a: Manifest[_], b: Manifest[_]): Boolean = {
          val itemTypeAOpt = getItemType(a)
          if (itemTypeAOpt.isEmpty) return a == b  // at end, must be same
        
          val itemTypeA = itemTypeAOpt.get

          val itemTypeBOpt = getItemType(b) 
          if (itemTypeBOpt.isEmpty) return false // different member count

          // Now check for compatible container types
          if (a <:< manifest[List[_]] && !(b <:< manifest[List[_]])) 
            false 
          else if (a <:< manifest[Vector[_]] && !(b <:< manifest[Vector[_]])) 
            false 
          else if (a <:< manifest[Seq[_]] && !(b <:< manifest[Seq[_]])) 
            // NOTE: This test ignored for embedded Seqs (manifest issue) 
            false
          else if (a <:< manifest[Map[String,_]] && 
              !(b <:< manifest[Map[String,_]]))
            // NOTE: This test ignored for embedded Map (manifest issue)
            false
          // If left type is Any then all is well...
          else if (itemTypeA == manifest[Any] || itemTypeA == manifest[Nothing])
            true
          else
            compareTypes(itemTypeA, itemTypeBOpt.get) // recursively compare 
        }

        if (compareTypes(objType, m)) None
        else Some("type mismatch: expecting "+ objType + " not " + m)
      }

      // Upgrades type from Any to a more specific type
      def upgradeType[A : Manifest] = {
        // Would like to pass these as higher level functions to constructor
        // to avoid same checks, but implicit manifests are static not dynamic.
        if (objType <:< manifest[List[_]]) manifest[List[A]]
        else if (objType <:< manifest[Vector[_]]) manifest[Vector[A]] 
        // m <:< manifest[Seq[_]] does not work for embedded Seqs, so 
        // assume Seq was requested if the typeArguments lenth is 1
        else if (objType.typeArguments.length == 1) manifest[Seq[A]] 
        // m <:< manifest[Map[String,_]] does not work for embedded Maps, so 
        // assume Map was requested if the typeArguments length is 2
        else if (objType.typeArguments.length == 2) manifest[Map[String, A]] 
        else throw new Error("Unknown type") 
      }
    }

    private object ObjectWithManifest {
      // Creates object
      def apply[A]()(
        implicit m: Manifest[A], reifiableSettings: ReifiableSettings
      ): Either[String, ObjectWithManifest[A]] = {
        val itemType = getItemType(manifest[A]) match {
          case Some(v) => v
          case _ => return Left("unsupported type: " + manifest[A])
        }

        if (manifest[A] <:< manifest[List[_]]) { 
          Right(new ObjectWithManifest(createList(itemType).asInstanceOf[A],
              manifest[A], reifiableSettings))
        } else if (manifest[A] <:< manifest[Vector[_]]) {
          Right(new ObjectWithManifest(createVector(itemType).asInstanceOf[A],
              manifest[A], reifiableSettings))
        } else if (manifest[A].typeArguments.length == 1) {
          // m <:< manifest[Seq[_]] does not work for embedded Seqs, so 
          // assume Seq was requested if the typeArguments length is 1
          Right(new ObjectWithManifest(createSeq(itemType).asInstanceOf[A],
              manifest[A], reifiableSettings))
        } else if (manifest[A].typeArguments.length == 2) {
          // m <:< manifest[Map[String,_]] does not work for embedded Maps, so 
          // assume Map was requested if the typeArguments length is 2
          Right(new ObjectWithManifest(createMap(itemType).asInstanceOf[A],
              manifest[A], reifiableSettings))
        } else {
          Left("unsupported type: " + manifest[A]) 
        }
      }

      // Object creation methods using manifests
      def createList[A : Manifest] = List[A]()
      def createVector[A : Manifest] = Vector[A]()
      def createSeq[A : Manifest] = Seq[A]()
      def createMap[A : Manifest] = Map[String, A]()
    }
  }

  /**
   * Wrapper iterator that will iterate over main iterator until end of
   * quoted string. Used to iterate over field names and perform case
   * conversion at the same time.
   */
  private class FieldIterator(val iter: BufferedIterator[Char])
      extends Iterator[Char] {
    private var prev: Char = 0
    private var firstQuote = true 
    private var invalidData = false

    def hasNext = {
      if (iter.hasNext) {
        // If hit unescaped non-double quote, then done with this field
        if (prev != '\\' && iter.head == '"') {
          iter.next()
          if (firstQuote) {
            firstQuote = false 
            hasNext   // Skip over first quote
          } else {
            false
          } 
        } else true
      } else {
        invalidData = true
        false
      }
    }

    def next() = {
      prev = iter.head
      iter.next()
      prev
    }

    def isInvalidData = invalidData
  }

  /** Iterable wrapper for Field Iterator */
  private class FieldIterable(
      private val iter: BufferedIterator[Char]) extends Iterable[Char] {
    val iterator = new FieldIterator(iter); 
  }

} // end package object
