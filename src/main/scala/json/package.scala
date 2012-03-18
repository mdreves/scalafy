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
  * Everything is parsed to standard primitives. Additional features:
  *  <li>Ability to parse data lazily (Stream, Iterator, Iterable)</li>
  *  <li>Ability to do field casing conversion during parsing</li>
  *  <li>Ability to parse to any collection type (immutable and mutable)</li> 
  *  <li>Ability to use any primitive type (Map[Int,Int],Map[Char,Int],...)</li>
  *  <li>Ability to parse from any class that is supported by reflection</li>
  *  <li>Ability to parse classes with embedded Lists/Maps (within limits)</li> 
  *  <li>Ability to parse classes with multiple constructors</li>
  *  <li>Ability to use Any and have parser choose the best List/Map type</li>
  *  <li>Ability to use Any in combination with Reifiable.</li>
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
  * fromJson[Map[Symbol,String]]("{\"foo\": \"bar\"}")
  * toJson(Map('foo -> "bar"))              // "{\"foo\": \"bar\"}"
  * fromJson[Map[Symbol,List[Int]]]("{\"foo\": [1,2,3]}")
  * toJson(Map('foo -> [1,2,3]))            // "{\"foo\": [1,2,3]}"
  *
  * // Parsing (Maps from any primitive type) 
  * fromJson[Map[String,String]]("{\"foo\": \"bar\"}")
  * toJson(Map("foo" -> "bar"))             // "{\"foo\": \"bar\"}"
  * fromJson[Map[Int,Int]]("{\"1\": 2, \"2\": 4}")
  * toJson(Map(1 -> 2, 2 -> 4))             // "{\"1\": 2, \"2\": 4}"
  *
  * // Parsing (lazy parsing)
  * fromJson[Stream[Int]]("[1,2,3]").get take 2 print // 1, 2 
  * fromJson[Iterable[Int]]("[1,2,3]").get.iterator   // hasNext/next...
  * fromJson[Iterator[Int]]("[1,2,3]").get            // hasNext/next...
  *
  * // convenience function for above - handles errors gracefully
  * val iter = jsonIterator[Int]("[1,2,3]").map(_ + 1) 
  * iter.next               // 2 
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
  * fromJson[Map[String,Any]]("{\"foo\": \"bar\"}")  // ...  Map[Symbol,String] 
  * fromJson[Any]("{\"foo\": 1}")    // Option[Any] (points to Map[Symbol,Int]) 
  *
  * // Parsing (using reflection)
  * case class Foo(s: String, i: Int)
  * fromJson[List[Foo]]("[{\"s\": \"foo\", \"i\": 1}]")  // Option[List[Foo]]
  * toJson(List(Foo("foo", 1)))
  * fromJson[Map[Int,Foo]]("{\"1\": {\"s\": \"foo\", \"i\": 1}}")
  * toJson(Map(1 -> Foo("foo", 1)))
  *
  * val iter = jsonIterator[Foo]("[{\"s\": \"foo\", \"i\": 1}]")
  * iter.next               // Foo("foo", 1) 
  *
  * // Parsing (using reflection - different constructors)
  * class Foo(s: String, i: Int) {
  *   def this(s: String) = this(s, 1)
  *   def this(i: Int) = this("bar", i)
  * }
  * fromJson[List[Foo]]("[{\"s\": \"foo\"}]")  // List(Foo("foo", 1)) 
  * fromJson[List[Foo]]("[{\"i\": 10}]")       // List(Foo("bar", 10)) 
  * 
  * // Parsing (using reflection - embedded objects)
  * case class Bar(f: Foo)
  * fromJson[List[Bar]]("[{\"f\": {\"s\": \"foo\", \"i\": 1}}]")
  *
  * // Parsing (using reflection - embedded Lists/Maps)
  * case class Baz(xs: List[Int])
  * fromJson[List[Baz]]("[{\"xs\": [1,2,3]}]") // List(Baz("xs", List(1,2,3))) 
  *
  * NOTE: Due to type erasure, only Lists/Maps of the default types are
  *       supported (String, Int (Long), Double, and Boolean). Maps are from
  *       Symbol to these types. Use of other types will return WITHOUT error
  *
  * // Parsing (using Any in combination with Reifiable)
  * implicit val reifiableSettings = ReifiableSettings(true)
  *
  * val v = fromJson[Any]("[1,9999999999]") 
  * v.isTypeOf[List[Int]]            // false 
  * v.isTypeOf[List[Long]]           // true 
  *
  * val v = fromJson[Any]("{\"foo\": 1}") 
  * v.isTypeOf[Map[Symbol,String]]   // false 
  * v.isTypeOf[Map[Symbol,Int]]      // true
  *
  * // Parsing (using other container types - any scala mutable/immutable type)
  * fromJson[Vector[Int]]("[1,2,3]") // Option[Vector[Int]] 
  * toJson(Vector(1,2,3))            // "[1,2,3]"
  * fromJson[Seq[Int]]("[1,2,3]")    // Option[Seq[Int]] 
  * toJson(Seq(1,2,3))               // "[1,2,3]"
  * fromJson[ListBuffer[Int]]("[1,2,3]") // Option[ListBuffer[Int]] 
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

  /** Converts RFC 4627 compatible JSON data to a specified type.
    *
    * If the type is set to Any then the most appropriate type will be
    * choosen automatically. The primitives String, Int (Long), Double, and 
    * Boolean are used. Lists can contain primitives, other Lists, or Maps. 
    * Maps are from Symbol to either a primitive, List, or another Map.
    * If a List or Map contains one ore more Longs and the rest are Ints, the
    * entire List or Map will be updated to a List or Map of Longs. 
    *
    * Specific types may also be passed. In this case, any scala container type
    * List, Vector, Seq, Map, ... (both immutable and mutable) may be used
    * in combination with any primitive type (String, Symbol, Int, Short, Long,
    * Float, Double, Boolean, Char, or Byte).
    *
    * Other object types are supported as long as a conversion is possible
    * using reflection. This limits conversions to objects make up of 
    * primitive types or other objects created from primitive types. Lists
    * and Maps are supported within objects, but due to type erasure only the
    * Lists/Maps that are converted from Any as specified above are supported.
    *
    * When reflection is used, the JSON field names must match the object field
    * names. They must also be specified in the order the constructor takes
    * them. Multiple constructors may be used as long as a constructor matching
    * the fields passed exists.
    * 
    * Examples:
    * {{{
    * // Type is List[Int]
    * val listOfInt = fromJson[List[Int]]("[1, 2, 3]")
    *
    * // Type is Any pointing to default List[Int] type
    * val listOfInt = fromJson[Any]("[1, 2, 3]")
    *
    * // Type is Map[Symbol, List[Int]]
    * val mapOfSymbolToListOfInt = 
    *   fromJson[Map[Symbol, List[Int]]]("{ \"foo\": [1, 2, 3] }")
    *
    * // Type is List[Foo]
    * case class Foo(s: String, i: Int)
    * fromJson[List[Foo]]("[{\"s\": \"foo\", \"i\": 1}]") 
    * }}}
    *
    * Symbol/String names used in Maps can have an automatic casing convertion 
    * done by setting the implicit fieldCasing parameter. For example:
    * {{{
    * implicit val fieldCasing = LowerCamelCase
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

  /** fromJson with an explicit casing for fields */
  def fromJson[A](json: Iterable[Char], fieldCasing: Casing)(
    implicit m: Manifest[A], reifiableSettings: ReifiableSettings 
  ): Option[A] = {
    fromJson[A](json)(m, fieldCasing, reifiableSettings)
  }

  /** Convenience method for fromJson[Iterator[A]]("...").get 
    *
    * Handles errors gracefully by returning false for hasNext
    */
  def jsonIterator[A](json: Iterable[Char])(
    implicit m: Manifest[A], 
    fieldCasing: Casing, 
    reifiableSettings: ReifiableSettings 
  ): Iterator[A] = {
    Json.parser.fromJson[Iterator[A]](json)(
        manifest[Iterator[A]], fieldCasing, reifiableSettings) match {
      case Right(r) => r 
      case Left(l) => new Iterator[A]() { 
        def hasNext = false
        def next() = throw new NoSuchElementException("next on empty iterator")
      }
    }
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
  def toJson(x: Any)(implicit 
    prettyPrintSettings: PrettyPrintSettings, 
    fieldCasing: Casing
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
    private val errorPrefix = "JSON parse error :: "

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

      parseNext(bufIter)(m, fieldCasing, reifiableSettings) match {
        case Left(l) => Left(formatError(l))
        case Right(r) => r match {
          case Some(v) =>
            // Unwrap objects stored with their manifests 
            if (v.isInstanceOf[ObjectWithManifest[_,_]]) {
               v.asInstanceOf[ObjectWithManifest[_,_]].getObj() match {
                 case Right(v) => Right(v.asInstanceOf[A])
                 case Left(l) => Left(formatError(l))
               }
            } else {
              Right(v.asInstanceOf[A])
            }
          case None => Left(errorPrefix + "empty input")
        }
      }
    }

    /** Parser for toJson package function */
    def toJson(value: Any)(implicit 
      prettyPrintSettings: PrettyPrintSettings, 
      fieldCasing: Casing
    ): String = {
      val prettyPrint = prettyPrintSettings.enabled
      val indent = prettyPrintSettings.indent
      
      // Prints output to buffer
      def printToBuf(buf: StringBuilder, value: Any, offset: Int) {
        // Null
        if (value == null) append(buf, "null", 0)

        // String/Symbol
        else if (value.isInstanceOf[String] || value.isInstanceOf[Symbol] ||
            value.isInstanceOf[Char]) {
          append(buf, "\"" + value.toString + "\"", 0)
        } 
       
        // Primitive
        else if (value.isInstanceOf[Int] || value.isInstanceOf[Short] ||
            value.isInstanceOf[Long] || value.isInstanceOf[Float] ||
            value.isInstanceOf[Double] || value.isInstanceOf[Byte] ||
            value.isInstanceOf[Boolean]) {
          append(buf, value.toString, 0)
        } 
        
        // Map (check Map before List so Iterable get matched)
        else if (isMapType(value.getClass)) {
          append(buf, "{", 0)
          if (prettyPrint) append(buf, "\n", 0)

          for ((k,v) <- value.asInstanceOf[Iterable[_]]) {
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

      
        // List
        else if (classOf[Iterable[_]].isAssignableFrom(value.getClass) ||
            classOf[Iterator[_]].isAssignableFrom(value.getClass)) {
          append(buf, "[", 0)

          val iter = 
            if (classOf[Iterable[_]].isAssignableFrom(value.getClass))
              value.asInstanceOf[Iterable[_]]
            else value.asInstanceOf[Iterator[_]]

          for (x <- iter) { 
            printToBuf(buf, x, 0) 
            append(buf, ",", 0)
            if (prettyPrint) append(buf, " ", 0)
          }

          if (prettyPrint) buf.length -= 2 else buf.length -= 1
          append(buf, "]", 0)
        } 

        // Object (use reflection)
        else {
          append(buf, "{", 0)
          if (prettyPrint) append(buf, "\n", 0)

          value.getClass.getDeclaredFields.map { field =>
            // Find  method with same name
            try {
              val fieldValue = value.getClass.getMethod(
                  field.getName).invoke(value)

              val fieldName = 
                if (fieldCasing != IgnoreCasing)
                  Casing.toCase(field.getName, fieldCasing) 
                else field.getName 

              append(buf, "\"" + fieldName + "\":", offset + indent)
              if (prettyPrint) append(buf, " ", 0)

              printToBuf(buf, fieldValue, offset + indent)
              append(buf, ",", 0)
              if (prettyPrint) append(buf, "\n", 0)
            } catch {
              case e: NoSuchMethodException => {} 
            }
          }

          if (prettyPrint) buf.length -= 2 else buf.length -= 1
          if (prettyPrint) append(buf, "\n", 0)
          append(buf, "}", offset)
        }
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

    /** parseNext for use with non-object based parsing */
    private def parseNext[A](
      iter: BufferedIterator[Char]
    )(implicit 
      m: Manifest[A], 
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ): Either[String, Option[A]] = {
      parseNext(iter, null, null)(m, fieldCasing, reifiableSettings)
    }

    /** Parses next item 
     *
     * This method can be used to parse the next value of a list, a primtive
     * type or an object. If an object is being parsed, all the objects
     * fields and values will be read (including any embedded objects/lists)
     *
     * Two special parameters are provided for when data is being parsed
     * as an object. The first is the wrapper object that the objects fields
     * are to be stored in (ObjectWithManifest). This is passed  so that we 
     * can question it about what types it is expecting for different fields
     * and parse appropriately. The second special parameter is a partial
     * function that will be called for each name/value that is parsed. The
     * caller must keep track of these names/values itself in order to
     * construct the object later (they will not be returned from this method).
     * The partial function should return String if the caller rejects the 
     * name/value given or None if the name/value was accepted.
     *
     * These two special methods are not needed for List based parsing or
     * primitive parsing.
     *
     * @return Right(value), Right(None) - no more data, or Left(error message)
     */
    private def parseNext[A](
      iter: BufferedIterator[Char],
      obj: ObjectWithManifest[_, _], // Container name/values will be stored in
      pf: PartialFunction[Tuple2[Any,Any],Option[String]] // For each name/value
    )(implicit 
      m: Manifest[A], 
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ): Either[String, Option[A]] = {

      var braceCount = 1
      var name: Any = null

      // Breaks out after a primitive/object is parsed or error found
      while (iter.hasNext && braceCount > 0) iter.head match {
        // end of obj
        case '}' if (obj != null) => iter.next(); braceCount -= 1

        // end of list
        case ']' if (obj == null) => iter.next(); braceCount -= 1

        // whitespace around structure characters (ignore)
        case ' ' => iter.next()
        case '\t' => iter.next()
        case '\n' => iter.next()
        case '\r' => iter.next()

        // list/obj value separator (ignore)
        case ',' => iter.next()

        // obj field name
        case '"' if (obj != null && name == null) =>
          // Convert to proper field casing at same time we parse
          fieldCasing match {
            case fc: Casing if (fc != IgnoreCasing &&  
                (obj.getKeyManifest == manifest[String] || 
                 obj.getKeyManifest == manifest[Symbol])) 
            => {
              val fieldIter = new FieldIterable(iter)

              val parsedValue = 
                Casing.toCase(fieldIter, fc, null, Nil).stripSuffix("\"")

              if (fieldIter.iterator.isInvalidData) {
                return Left("invalid field name '" + parsedValue + "'")
              } 
              
              // Need to do our own conversion since we parsed ourselves
              if (obj.getKeyManifest == manifest[Symbol]) 
                name = Symbol(parsedValue)
              else name = parsedValue
            }
            case _ => parseJsonString(iter)(obj.getKeyManifest) match {
              case Right(value) => name = value
              case Left(l) => return Left("invalid field name") 
            }
          }

          if (!iter.hasNext || iter.head != ':') {
            return Left("invalid field: missing ':' after " + name) 
          }
          iter.next() // skip over ':'

        // obj/list string value
        case '"' =>
          val itemType = if (obj == null) m else obj.getItemType(name) 

          parseJsonString(iter)(itemType) match {
            case Left(l) => return Left(l) 
            case Right(value) => {
              if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

              pf((name, value)) match {
                case Some(e) => return Left(e)
                case None => name = null  // success, reset
              }
            }
          }

        // if object, name must be set
        case _ if (obj != null && name == null) =>
          return Left("invalid field: missing name")

        // obj/list number value
        case c: Char if (c == '-' || c.isDigit) =>
          val itemType = if (obj == null) m else obj.getItemType(name) 

          parseJsonNumber(iter)(itemType) match {
            case Left(l) => return Left(l) 
            case Right(value) =>  {
              if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

              pf((name, value)) match {
                case Some(e) => return Left(e)
                case None => name = null  // success, reset
              }
            }
          }

        // obj/list boolean value
        case c: Char if (c == 't' || c == 'f') =>
          val itemType = if (obj == null) m else obj.getItemType(name) 

          parseJsonBoolean(iter)(itemType) match {
            case Left(l) => return Left(l) 
            case Right(value) => {
              if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

              pf(name, value) match {
                case Some(e) => return Left(e)
                case None => name = null  // success, reset
              }
            }
          }

        // object/list null value
        case 'n' =>
          val itemType = if (obj == null) m else obj.getItemType(name) 

          parseJsonNull(iter)(itemType) match {
            case Left(l) => return Left(l) 
            case Right(value) => {
              if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

              pf((name, value)) match {
                case Some(e) => return Left(e)
                case None => name = null  // success, reset
              }
            }
          }

        // start of object
        case '{' =>
          val itemType = if (obj == null) m else obj.getItemType(name) 

          parseJsonObject(iter)(itemType, fieldCasing, reifiableSettings) match{
            case Left(l) => return Left(l) 
            case Right(value) => {
              if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

              pf((name, value)) match {
                case Some(e) => return Left(e)
                case None => name = null  // success, reset
              }
            }
          }

        // start of list
        case '[' =>
          val itemType = if (obj == null) m else obj.getItemType(name) 

          parseJsonList(iter)(itemType, fieldCasing, reifiableSettings) match {
            case Left(l) => return Left(l) 
            case Right(value) => {
              if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

              pf((name, value)) match {
                case Some(e) => return Left(e)
                case None => name = null  // success, reset
              }
            }
          }

        case x => return Left("unexpected character: " + x) 
      }

      if (braceCount != 0) return Left("missing brace")
      
      Right(None)   // End of data
    }

    /** Parses JSON string (can be parsed as any primitive) */
    private def parseJsonString[A : Manifest](
      iter: BufferedIterator[Char]
    ): Either[String, A] = {
        if (!(manifest[A] >:> manifest[String] || 
            manifest[A] >:> manifest[Symbol] || 
            manifest[A] <:< manifest[AnyVal])) {
        return Left("type mismatch: expecting " + manifest[A] + " not String")
      }

      if (!iter.hasNext || iter.head != '"') 
        return Left("invalid String") 

      iter.next()  // Skip first "

      if (manifest[A] == manifest[Int] || manifest[A] == manifest[Short] ||
          manifest[A] == manifest[Long] || manifest[A] == manifest[Float] ||
          manifest[A] == manifest[Double] || manifest[A] == manifest[Byte]) {
        // If numeric type requested, pass off to parseJsonNumber
        parseJsonNumber(iter)(manifest[A]) match {
          case Right(r) => 
            // Skip trailing "
            if (iter.hasNext && iter.head == '"') iter.next()
            Right(r.asInstanceOf[A])
          case Left(l) => Left(l) // error
        }
      } else if (manifest[A] == manifest[Boolean]) {
        // If boolean type requested, pass off to parseJsonBoolean
        parseJsonBoolean(iter)(manifest[A]) match {
          case Right(r) => 
            // Skip trailing "
            if (iter.hasNext && iter.head == '"') iter.next()
            Right(r.asInstanceOf[A])
          case Left(l) => Left(l) // error
        }
      } else {
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
          } else if (manifest[A] == manifest[Symbol]) {
            Right(Symbol(s.toString).asInstanceOf[A])
          } else Right(s.toString().asInstanceOf[A])
        }
        else {
          val typeName =
            if (manifest[A] == manifest[Char]) "Char"
            else if (manifest[A] == manifest[Symbol]) "Symbol"
            else "String"

          Left("invalid " + typeName + " '" + s.toString + "'") 
        }
      }
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
    ): Either[String, ObjectWithManifest[A,_]] = {
      if (!iter.hasNext || iter.head != '[') return Left("missing input")
      iter.next() // Skip over '['

      // Special handling for Iterable and Iterator
      if (m <:< manifest[Iterable[_]] || m <:< manifest[Iterator[_]]) {
        Right(new IterableWithManifest(
            iter, manifest[A], fieldCasing, reifiableSettings)) 
      } 
      
      // Special handling for Streams 
      else if (m <:< manifest[Stream[_]]) {
        Right(new StreamWithManifest(
            iter, manifest[A], fieldCasing, reifiableSettings)) 
      } 
      
      // Any other list type... 
      else {
        // Create list
        val xs = 
          if (m == manifest[Any] || m == manifest[Nothing]) {
            // If specific type not given, default to List
            new ListWithManifest(manifest[List[Any]], reifiableSettings)
          } else if (isListType(m)) {
            new ListWithManifest(manifest[A],reifiableSettings)
          } else return Left("type mismatch: expecting List not " + m) 

        // Now parse the list...
        while (true) {
          parseNext(iter)(
              xs.getItemType(), fieldCasing, reifiableSettings) match {
            case Left(l) => return Left(l)
            case Right(r) => r match { 
              case Some(v) => xs.add(v)
              case None =>  // Done
                return Right(xs.asInstanceOf[ObjectWithManifest[A,_]])
            }
          }
        }
        // Can't get here...
        Left("Not possible")
      }
    }

    /** Parses JSON Object */
    private def parseJsonObject[A](
      iter: BufferedIterator[Char]
    )(implicit 
      m: Manifest[A], 
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ): Either[String, ObjectWithManifest[A,_]] = {
      if (!iter.hasNext || iter.head != '{') return Left("missing input")
      iter.next() // Skip over '{'

      // Create object
      val xm = 
        if (m == manifest[Any] || m == manifest[Nothing]) {
          // If specific type not given, default to Map 
          new MapWithManifest(
            manifest[Map[Symbol, Any]], manifest[Symbol], reifiableSettings)
        } else if (isMapType(m)) {
          val keyType = m.typeArguments(0)
          // Type must be a basic type
          if (!(keyType == manifest[String] || keyType == manifest[Symbol] ||
              keyType <:< manifest[AnyVal])) {
            return Left(
              "field types of " + m.typeArguments(0) + " not supported")
          } else new MapWithManifest(manifest[A], keyType, reifiableSettings)
        } else new ObjectFromReflection(manifest[A], reifiableSettings)

      // Now parse the object...
      parseNext(iter, xm, {
        case (name, value) => xm.add(name, value) match {
          case Some(v) => Some(v)
          case None => None // success
        }
        case o => Some("unexpected data: " + o) 
      })(m, fieldCasing, reifiableSettings) match {
        case Left(e) => Left(e) // error
        case _ => Right(xm.asInstanceOf[ObjectWithManifest[A,_]])  // success
      }
    }

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

    /**
     * Encapsulates an object (List, Map, ...) along with its required manifest
     * and the manifest for the types actually read.
     *
     * By using this class, callers of the fromJson function can pass Any
     * for the type and we will automatically choose the best type for
     * them based on what data was actually read. If the reifiableSettings
     * flag is also set then the actual manifest information can be stored for
     * comparisons later. This class is also overridden when reflection is used.
     *
     * The types may be a bit confusing, but A is for the object type
     * (List[Int], Map[Symbol, Int], ...) and B is for the type used to
     * lookup values in that object type. For lists this is N/A. For
     * reflection based objects this is Symbol, but for Maps this could be
     * Symbol, String, ... (depends on what was requested). In other words,
     * if the object type is Map[Double,Int], the keyType should be Double.
     */
    private abstract class ObjectWithManifest[A, B](
      val objType: Manifest[A],  // Type of Object
      val keyType: Manifest[B],  // Type to lookup values in obj (N/A for Lists)
      val reifiableSettings: ReifiableSettings
    ) {
      private var actualItemType: Manifest[_] = null // actual item type used

      /** Gets the item type contained in this container.
        *
        * This must be overridden by single item containers (e.g. List).
        */
      def getItemType(): Manifest[_] = throw new Error("Not supported")

      /** Gets the item type contained in this container for given field.
        *
        * This must be overridden by multi-item containers (e.g. Map/Object) 
        */
      def getItemType(name: Any): Manifest[_] = throw new Error("Not supported")

      /** Returns true if this container is upgradeable to more specific type */
      protected def isContainerUpgradeable(): Boolean = false
      
      /** Gets object.
        *
        * This must be overriden by subclasses.
        *
        * NOTE: If a Any was used, then the type returned is still of
        *       type List[Any], etc. Only the manifest is updated to
        *       a track the more specific type so that a safe cast can
        *       be done.
        * 
        * @return object or String error message if error occurred
        */
      def getObj(): Either[String, A]

      def getKeyManifest() = keyType

      /** Manifest of actual data stored.
        * 
        * NOTE: This will only be different than the object's type if
        *       Any was used AND a better type was determined.
        */
      def getManifest() = {
        if (!isContainerUpgradeable() || actualItemType == manifest[Any]) {
          // We were given a specific type or Any is as good as it gets
          objType
        } else {
          // Upgrade from Any to more specific type
          createManifestUsing(actualItemType)
        }
      }

      /** Add for single item container types - DO NOT override
        *
        * @return None if successful else string error
        */
      def add(item: Any): Option[String] = {
        // Unpack any embedded objects
        val newItem = 
          if (item.isInstanceOf[ObjectWithManifest[_,_]]) { 
            item.asInstanceOf[ObjectWithManifest[_,_]].getObj() match {
              case Right(r) => r
              case Left(l) => return Some(l) // error
            }
          } else item

        internalAdd(newItem) match {
          case Some(v) => Some(v)  // error
          case None => updateActualType(item); None
        }
      }

      /** Internal add for single item container types 
        *
        * This should be overriden by subclasses such as Lists. Subclasses
        * are responsible for validating the items type (the validateType
        * method may be used to help).
        * 
        * @return None if successful else string error
        */
      protected def internalAdd(item: Any): Option[String] = None 

      /** Add for multi-item container types - DO NOT override
        *
        * @return None if successful else string error
        */
      def add(name: Any, item: Any): Option[String] = {
        // Unpack any embedded objects
        val newItem = 
          if (item.isInstanceOf[ObjectWithManifest[_,_]]) { 
            item.asInstanceOf[ObjectWithManifest[_,_]].getObj() match {
              case Right(r) => r
              case Left(l) => return Some(l) // error
            }
          } else item
       
        internalAdd(name, newItem) match {
          case Some(v) => Some(v) 
          case None => updateActualType(item); None 
        }
      }

      /** Internal add for multi-item container types
        *
        * This should be overridden by subclasses that are Maps or
        * other objects. Subclasses are responsible for validating
        * the item type (the validateType method may be used to help).
        *
        * @return None if successful else string error
        */
      protected def internalAdd(name: Any, item: Any): Option[String] = None 

      /** Creates a new manifest using the given type - override
        * 
        * This is used when upgrading from Any to a more specific type.
        * and should be overidden by List or Map implementations.
        */
      protected def createManifestUsing[A : Manifest]: Manifest[_] = {
         throw new Error("Not supported") 
      }

      /** Updates actual type to what was read */
      protected def updateActualType(item: Any): Unit = item match {
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
        case o: ObjectWithManifest[_,_] =>
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

      /** Helper method to validate item type against manifest for type.
        *
        * @return None if successfull or String if error
        */
      protected def validateType[A](x: A, m: Manifest[_]): Option[String] = {
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
        else if (x.isInstanceOf[Symbol] && m != manifest[Symbol]) 
          Some("type mismatch: expecting " + m + " not Symbol") 
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
        else if (x.isInstanceOf[ObjectWithManifest[_,_]]) {
          val o = x.asInstanceOf[ObjectWithManifest[_,_]]
          // Object in Object 
          if (recursiveCompare(o.objType, m)) None
          else Some("type mismatch: expecting "+ o.objType + " not " + m)
        }
        else None
      }

      // Helper function to recursively compare manifests
      protected def recursiveCompare(
          a: Manifest[_], b: Manifest[_]): Boolean = {
        if (a.typeArguments.size != b.typeArguments.size) return false

        // A must be able to hold B
        if (!a.erasure.isAssignableFrom(b.erasure)) return false

        if (a.typeArguments.size == 0) return true // at end
        else if (a.typeArguments.size == 1) {  // containers of one item
          recursiveCompare(a.typeArguments(0), b.typeArguments(0)) 
        } else if (a.typeArguments.size == 2) {
          // The only two element containers we know are Maps where first
          // param type is Symbol, so just use second param type
          recursiveCompare(a.typeArguments(1), b.typeArguments(1))
        } else false  // Don't know any types...
      }

  
      /** Return true if type allowed to be null */
      protected def isNullAllowed(m: Manifest[_]): Boolean = 
        m <:< manifest[AnyRef] 
    }

    /** Wrapper for Iterable sequences */
    private class IterableWithManifest[A](
      val iter: BufferedIterator[Char],  // character stream 
      iterableType: Manifest[A],
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ) extends ObjectWithManifest[A, Nothing](
      iterableType, manifest[Nothing], reifiableSettings
    ) {

      private var obj: A = 
        if (objType <:< manifest[Iterable[_]])
          createIterable(objType.typeArguments(0)).asInstanceOf[A]
        else if (objType <:< manifest[Iterator[_]])
          createIterator(objType.typeArguments(0)).asInstanceOf[A]
        else throw new Error("Unsupported type: " + objType) // can't happen 

      override def getItemType(): Manifest[_] = objType.typeArguments(0)

      override protected def isContainerUpgradeable(): Boolean = 
        getItemType() == manifest[Any]

      def getObj(): Either[String, A] = {
        if (reifiableSettings.enabled) Reifiable(getManifest(), obj)
        Right(obj)
      }

      override protected def createManifestUsing[A : Manifest]: Manifest[_] = {
        if (objType <:< manifest[Iterable[_]]) manifest[Iterable[A]]
        else if (objType <:< manifest[Iterator[_]]) manifest[Iterator[A]]
        else throw new Error("Unsupported type: " + objType) // can't happen 
      }

      def createIterable[B : Manifest]: Iterable[B] = { 
        // Annonymous iterable
        new Iterable[B]() { 
          override def iterator: Iterator[B] = createIterator[B] 
        }
     }

      def createIterator[B : Manifest]: Iterator[B] =  {
        // Annonymous iterator
        new Iterator[B]() {
          private var cur: Any = null
          private var done = false 

          def hasNext: Boolean = {
            if (done) return false 
            if (cur != null) return true 

            parseNext(iter)(
                getItemType(), fieldCasing, reifiableSettings) match {
              case Left(l) => done = true; false
              case Right(r) => r match { 
                case Some(v) =>
                  // Unwrap object if inside another container
                  if (v.isInstanceOf[ObjectWithManifest[_,_]]) {
                    v.asInstanceOf[ObjectWithManifest[_,_]].getObj match {
                      case Right(r) => cur = r; true
                      case Left(l) => done = true; false
                    }
                  } else { cur = v; true }
                case None =>  done = true; false // Done
              }
            }
          }

          def next(): B = {
            if (cur == null) hasNext // trigger retrieval, if haven't already
            if (done || cur == null) {
              throw new NoSuchElementException("next on empty iterator")
            }

            val result = cur
            cur = null
            result.asInstanceOf[B] 
          }
        }
      }
    }

    /** Wrapper for Streams */
    private class StreamWithManifest[A](
      val iter: BufferedIterator[Char],  // character stream 
      streamType: Manifest[A],
      fieldCasing: Casing, 
      reifiableSettings: ReifiableSettings
    ) extends ObjectWithManifest[A, Nothing](
      streamType, manifest[Nothing], reifiableSettings
    ) {

      private var obj: A = 
        createStream(objType.typeArguments(0)).asInstanceOf[A]

      override def getItemType(): Manifest[_] = objType.typeArguments(0)

      override protected def isContainerUpgradeable(): Boolean = 
        getItemType() == manifest[Any]

      def getObj(): Either[String, A] = {
        if (reifiableSettings.enabled) Reifiable(getManifest(), obj)
        Right(obj)
      }

      override protected def createManifestUsing[A : Manifest]: Manifest[_] = {
        manifest[Stream[A]]
      }

      /** Recursive stream definition... */
      private def createStream[B : Manifest]: Stream[B] = {
        parseNext(iter)(getItemType(), fieldCasing, reifiableSettings) match {
          case Left(l) => Stream.empty.asInstanceOf[Stream[B]]
          case Right(r) => r match { 
            case Some(v) =>
              // Unwrap object if inside another container
              if (v.isInstanceOf[ObjectWithManifest[_,_]]) {
                v.asInstanceOf[ObjectWithManifest[_,_]].getObj match {
                  case Right(r) => Stream.cons(r.asInstanceOf[B], createStream) 
                  case Left(l) => Stream.empty.asInstanceOf[Stream[B]]
                }
              } else { 
                Stream.cons(v.asInstanceOf[B], createStream) 
              }
            case None => Stream.empty.asInstanceOf[Stream[B]]
          }
        }
      }
    }

    /** Wrapper for List like sequences */
    private class ListWithManifest[A](
      listType: Manifest[A], 
      reifiableSettings: ReifiableSettings
    ) extends ObjectWithManifest[A, Nothing](
      listType, manifest[Nothing], reifiableSettings
    ) {

      private var obj: Any = { // Using Any so can use ListBuffer for List
        // Special case for List/Seq, since we need to reverse it, we will
        // use a ListBuffer until the list is requested
        val clazz = 
          if (objType.erasure == classOf[List[_]] || 
              objType.erasure == classOf[::[_]] || 
              objType.erasure == classOf[Seq[_]] || 
              objType.erasure == classOf[collection.immutable.LinearSeq[_]]) {
            classOf[collection.mutable.ListBuffer[_]]
          } else objType.erasure

        createList(clazz)(objType.typeArguments(0)) match {
          case Right(r) => r
          case Left(l) => throw new Error(l)
        }
      }

      override def getItemType(): Manifest[_] = objType.typeArguments(0)

      override protected def isContainerUpgradeable(): Boolean = 
        getItemType() == manifest[Any]

      def getObj(): Either[String, A] = {
        // Get List from ListBuffer if List used
        val result = 
          if (objType.erasure == classOf[List[_]] ||
              objType.erasure == classOf[::[_]]) {
            obj.asInstanceOf[collection.mutable.ListBuffer[_]].toList
          } else if (objType.erasure == classOf[Seq[_]]) {
            obj.asInstanceOf[collection.mutable.ListBuffer[_]].toSeq
          } else if (objType.erasure == 
              classOf[collection.immutable.LinearSeq[_]]) {
            obj.asInstanceOf[collection.mutable.ListBuffer[_]].toSeq
          } else obj

        if (reifiableSettings.enabled) Reifiable(getManifest(), result)
        Right(result.asInstanceOf[A])
      }

      override protected def internalAdd(item: Any): Option[String] = {
        validateType(item, getItemType()) match {
          case Some(v) => return Some(v) // error
          case None => {}
        }

        updateList(obj, item) match {
          case Right(r) => obj = r; None
          case Left(l) => Some(l)
        }
      }

      override protected def createManifestUsing[A : Manifest]: Manifest[_] = {
        createListManifest(objType.erasure)(manifest[A]) match {
          case Right(m) => m
          case Left(l) => throw new Error(l)
        }
      }
    }

    /** Wrapper for Map like sequences */
    private class MapWithManifest[A, B](
      mapType: Manifest[A],    // Map[X, Y] 
      mapKeyType: Manifest[B], // X
      reifiableSettings: ReifiableSettings
    ) extends ObjectWithManifest[A, B](mapType, mapKeyType, reifiableSettings) {
      
      private var obj: A = 
        createMap(objType.typeArguments(0), 
            objType.typeArguments(1), objType.erasure) match {
          case Right(r) => r.asInstanceOf[A]
          case Left(l) => throw new Error(l)
        }

      override def getItemType(): Manifest[_] = objType.typeArguments(1)

      override def getItemType(name: Any): Manifest[_] = 
        objType.typeArguments(1)

      override protected def isContainerUpgradeable(): Boolean = 
        objType.typeArguments(1) == manifest[Any]

      def getObj(): Either[String, A] = {
        if (reifiableSettings.enabled) Reifiable(getManifest(), obj)
        Right(obj)
      }

      override protected def internalAdd(
          name: Any, item: Any): Option[String] = {
        validateType(item, getItemType(name)) match {
          case Some(v) => return Some(v) // error
          case None => {}
        }

        updateMap(obj, name, item) match {
          case Right(r) => obj = r.asInstanceOf[A]; None
          case Left(l) => Some(l)
        }
      }

      override protected def createManifestUsing[A : Manifest]: Manifest[_] = {
        createMapManifest(objType.erasure)(manifest[A]) match {
          case Right(m) => m
          case Left(l) => throw new Error(l)
        }
      }
    }

    /** Wrapper for Object read from reflection */
    private class ObjectFromReflection[A](
      reflectionType: Manifest[A],
      reifiableSettings: ReifiableSettings
    ) extends ObjectWithManifest[A, Symbol](
      reflectionType, manifest[Symbol], reifiableSettings
    ) {

      val fieldTypes = ObjectFromReflection.getFieldTypes(objType.erasure) 
      var fieldValues = collection.mutable.LinkedHashMap[Symbol,Any]()  

      override def getItemType(): Manifest[_] = manifest[Any]

      override def getItemType(name: Any): Manifest[_] = {
        val fieldType = fieldTypes.get(name.asInstanceOf[Symbol])
        if (fieldType.isEmpty) {
          // Not a type we know, return Any for now, we will catch the
          // error later when we try to convert to object
          manifest[Any]
        } else {
          toManifest(fieldType.get)
        }
      }

      def getObj(): Either[String, A] = {
        // Find constructors matching less than or equal to our value count
        val ctors = objType.erasure.getDeclaredConstructors.filter {
          ctor => ctor.getParameterTypes.size <= fieldValues.size 
        }
        if (ctors.isEmpty) {
          Left("missing fields: " +
              fieldTypes.keySet.diff(fieldValues.keySet).mkString(", ") + 
              " for " + objType) 
        } else {
          val ctor = if (ctors.size == 1) ctors(0) else {
            // Find ctor matching our args
            val ctorOpt = ctors.find { ctor => 
              val matchingTypes = fieldValues.keys.map { 
                name => fieldTypes.get(name).get
              }
              ctor.getParameterTypes.zip(matchingTypes).forall {
                pair => pair._1 == pair._2
              }
            }
            if (ctorOpt.isEmpty) { 
              return Left("no matching constructor for: " + objType)
            } else {
              ctorOpt.get
            }
          }

          try {
            val numParams = ctor.getParameterTypes.size

            val newObj = ctor.newInstance(
                toRefArray(fieldValues.values.take(numParams)) : _*)
             
            // Set any fields not used during construction that were passed
            for ((name, value) <- fieldValues.drop(numParams)) {
              // Need to find method with name_$eq
              val method = objType.erasure.getDeclaredMethods.find {
                m => m.getName == (name.name + "_$eq")
              }
              if (!method.isEmpty) { 
                method.get.invoke(newObj, toRefArray(Seq(value)) : _*)
              }
            }

            Right(newObj.asInstanceOf[A])
          } catch {
            case e => Left("object construction failed: " + e.getMessage())
          }
        }
      }

      override protected def internalAdd(
          name: Any, item: Any): Option[String] = {

        // Validate type 
        val fieldType = fieldTypes.get(name.asInstanceOf[Symbol])
        if (fieldType.isEmpty) return Some("unexpected field name: " + name)
        if (item == null) {
          if (!isNullAllowed(fieldType.get)) {
            return Some("type mismatch: expecting "+fieldType.get + " not null")
          }

        } else if (!fieldType.get.isAssignableFrom(toClass(item))) {
          return Some("type mismatch: expecting " + fieldType.get + 
              " not " + toClass(item))
        }

        // Add to our container
        fieldValues += (name.asInstanceOf[Symbol] -> item)
        None
      }

      /** Creates manifest that goes with class */
      private def toManifest(clazz: Class[_]): Manifest[_] = {
        if (clazz == classOf[String]) manifest[String]
        else if (clazz == classOf[Symbol]) manifest[Symbol]
        else if (clazz == classOf[Int]) manifest[Int]
        else if (clazz == classOf[Short]) manifest[Short]
        else if (clazz == classOf[Long]) manifest[Long]
        else if (clazz == classOf[Float]) manifest[Float]
        else if (clazz == classOf[Double]) manifest[Double]
        else if (clazz == classOf[Boolean]) manifest[Boolean]
        else if (clazz == classOf[Char]) manifest[Char]
        else if (clazz == classOf[Byte]) manifest[Byte]
        else if (clazz == classOf[List[_]]) manifest[List[Any]]
        else if (clazz == classOf[Vector[_]]) manifest[Vector[Any]]
        else if (clazz == classOf[Seq[_]]) manifest[Seq[Any]]
        else if (clazz == classOf[Map[_,_]]) manifest[Map[String,Any]]
        else new Manifest[ObjectFromReflection[_]]() { 
          def erasure = clazz
        }
      }

      /** Ensures that boxed types are returned as primitive types */
      private def toClass(x: Any): Class[_] = x match {
        case _: Int => java.lang.Integer.TYPE
        case _: Short => java.lang.Short.TYPE
        case _: Long => java.lang.Long.TYPE
        case _: Float => java.lang.Float.TYPE
        case _: Double => java.lang.Double.TYPE
        case _: Char => java.lang.Character.TYPE
        case _: Boolean => java.lang.Boolean.TYPE
        case _: Byte => java.lang.Byte.TYPE
        case _ => x.getClass
      }

      /** Return true if type allowed to be null */
      private def isNullAllowed(clazz: java.lang.Class[_]): Boolean = {
        if (clazz == classOf[Int]) false
        else if (clazz == classOf[Short]) false
        else if (clazz == classOf[Long]) false
        else if (clazz == classOf[Float]) false
        else if (clazz == classOf[Double]) false
        else if (clazz == classOf[Boolean]) false
        else if (clazz == classOf[Char]) false
        else if (clazz == classOf[Byte]) false
        else true
      }

      /** Converts type to Refs (only boxed types can be used with ctor) */
      private def toRefArray(xs: Iterable[Any]): Array[AnyRef] = {
        // Helper to convert to Ref types
        def toRef(x: Any): AnyRef = x match {
          case i: Int => Int.box(i) 
          case s: Short => Short.box(s)
          case l: Long => Long.box(l)
          case f: Float => Float.box(f)
          case d: Double => Double.box(d)
          case b: Boolean => Boolean.box(b)
          case c: Char => Char.box(c)
          case b: Byte => Byte.box(b) 
          case x => x.asInstanceOf[AnyRef]
        }

        val refArray = new Array[AnyRef](xs.size)
        var i = 0
        for (x <- xs) {
          refArray(i) = toRef(x)
          i += 1
        }
        refArray
      }
    }

    private object ObjectFromReflection {
      def getFieldTypes(clazz: java.lang.Class[_]) = {
        var fieldTypes = Map[Symbol, java.lang.Class[_]]()
        for (f <- clazz.getDeclaredFields) {
          fieldTypes += (Symbol(f.getName()) -> f.getType()) 
        }
        fieldTypes
      }
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


  ///////////////////////////////////////////////////////////////////////////
  // List/Map Implementations 
  ///////////////////////////////////////////////////////////////////////////

  private def getOrdering(m: Manifest[_]): Option[Ordering[_]] = {
    if (m == manifest[String]) Some(Ordering[String])
    else if (m == manifest[Int]) Some(Ordering[Int])
    else if (m == manifest[Short]) Some(Ordering[Short])
    else if (m == manifest[Long]) Some(Ordering[Long])
    else if (m == manifest[Float]) Some(Ordering[Float])
    else if (m == manifest[Double]) Some(Ordering[Double])
    else if (m == manifest[Boolean]) Some(Ordering[Boolean])
    else if (m == manifest[Char]) Some(Ordering[Char])
    else if (m == manifest[Byte]) Some(Ordering[Byte])
    else None
  }

  private def isListType(m: Manifest[_]): Boolean = {
    isListType(m.erasure)
  }

  private def isListType(clazz: java.lang.Class[_]): Boolean = {
    ( classOf[List[_]].isAssignableFrom(clazz) ||
      classOf[::[_]].isAssignableFrom(clazz) ||
      classOf[Vector[_]].isAssignableFrom(clazz) ||
      classOf[IndexedSeq[_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.LinearSeq[_]].isAssignableFrom(clazz) ||
      classOf[Seq[_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.HashSet[_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.TreeSet[_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.SortedSet[_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.ListSet[_]].isAssignableFrom(clazz) ||
      classOf[Set[_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.Stack[_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.Queue[_]].isAssignableFrom(clazz) ||
      classOf[Stream[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.ListBuffer[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.PriorityQueue[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.Queue[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.HashSet[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.LinkedHashSet[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.Set[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.ArrayBuffer[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.ResizableArray[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.ArrayStack[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.Stack[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.LinkedList[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.DoubleLinkedList[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.MutableList[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.ArraySeq[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.IndexedSeq[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.LinearSeq[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.Seq[_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.Buffer[_]].isAssignableFrom(clazz) || 
      classOf[collection.mutable.UnrolledBuffer[_]].isAssignableFrom(clazz) )
  }

  private def createListManifest[A : Manifest](
    clazz: java.lang.Class[_]
  ): Either[String, Manifest[_]] = {
    if (clazz == classOf[List[_]]) {
      Right(manifest[List[A]]) 
    } else if (clazz == classOf[::[_]]) {
      Right(manifest[::[A]]) 
    } else if (clazz == classOf[Vector[_]]) {
      Right(manifest[Vector[A]]) 
    } else if (clazz == classOf[Seq[_]]) {
      Right(manifest[Seq[A]]) 
    } else if (clazz == classOf[IndexedSeq[_]]) {
      Right(manifest[IndexedSeq[A]]) 
    } else if (clazz == classOf[collection.immutable.LinearSeq[_]]) {
      Right(manifest[collection.immutable.LinearSeq[A]]) 
    } else if (clazz == classOf[collection.immutable.HashSet[_]]) {
      Right(manifest[collection.immutable.HashSet[A]]) 
    } else if (clazz == classOf[collection.immutable.TreeSet[_]]) {
      Right(manifest[collection.immutable.TreeSet[A]]) 
    } else if (clazz == classOf[collection.immutable.SortedSet[_]]) {
      Right(manifest[collection.immutable.SortedSet[A]]) 
    } else if (clazz == classOf[collection.immutable.ListSet[_]]) {
      Right(manifest[collection.immutable.ListSet[A]]) 
    } else if (clazz == classOf[Set[_]]) {
      Right(manifest[Set[A]]) 
    } else if (clazz == classOf[collection.immutable.Stack[_]]) {
      Right(manifest[collection.immutable.Stack[A]]) 
    } else if (clazz == classOf[collection.immutable.Queue[_]]) {
      Right(manifest[collection.immutable.Queue[A]]) 
    } else if (clazz == classOf[Stream[_]]) {
      Right(manifest[Stream[A]]) 
    } else if (clazz == classOf[collection.mutable.ListBuffer[_]]) {
      Right(manifest[collection.mutable.ListBuffer[A]]) 
    } else if (clazz == classOf[collection.mutable.PriorityQueue[_]]) {
      Right(manifest[collection.mutable.PriorityQueue[A]]) 
    } else if (clazz == classOf[collection.mutable.Queue[_]]) {
      Right(manifest[collection.mutable.Queue[A]]) 
    } else if (clazz == classOf[collection.mutable.HashSet[_]]) {
      Right(manifest[collection.mutable.HashSet[A]]) 
    } else if (clazz == classOf[collection.mutable.LinkedHashSet[_]]) {
      Right(manifest[collection.mutable.LinkedHashSet[A]]) 
    } else if (clazz == classOf[collection.mutable.Set[_]]) {
      Right(manifest[collection.mutable.Set[A]]) 
    } else if (clazz == classOf[collection.mutable.ArrayBuffer[_]]) {
      Right(manifest[collection.mutable.ArrayBuffer[A]]) 
    } else if (clazz == classOf[collection.mutable.ResizableArray[_]]) {
      Right(manifest[collection.mutable.ResizableArray[A]]) 
    } else if (clazz == classOf[collection.mutable.ArrayStack[_]]) {
      Right(manifest[collection.mutable.ArrayStack[A]]) 
    } else if (clazz == classOf[collection.mutable.Stack[_]]) {
      Right(manifest[collection.mutable.Stack[A]]) 
    } else if (clazz == classOf[collection.mutable.LinkedList[_]]) {
      Right(manifest[collection.mutable.LinkedList[A]]) 
    } else if (clazz == classOf[collection.mutable.DoubleLinkedList[_]]) {
      Right(manifest[collection.mutable.DoubleLinkedList[A]]) 
    } else if (clazz == classOf[collection.mutable.MutableList[_]]) {
      Right(manifest[collection.mutable.MutableList[A]]) 
    } else if (clazz == classOf[collection.mutable.ArraySeq[_]]) {
      Right(manifest[collection.mutable.ArraySeq[A]]) 
    } else if (clazz == classOf[collection.mutable.IndexedSeq[_]]) {
      Right(manifest[collection.mutable.IndexedSeq[A]]) 
    } else if (clazz == classOf[collection.mutable.LinearSeq[_]]) {
      Right(manifest[collection.mutable.LinearSeq[A]]) 
    } else if (clazz == classOf[collection.mutable.Seq[_]]) {
      Right(manifest[collection.mutable.Seq[A]]) 
    } else if (clazz == classOf[collection.mutable.UnrolledBuffer[_]]) {
      Right(manifest[collection.mutable.UnrolledBuffer[A]]) 
    } else if (clazz == classOf[collection.mutable.Buffer[_]]) {
      Right(manifest[collection.mutable.Buffer[A]]) 
    } else { 
      Left("Unknown type: " + clazz)
    }
  }

  private def createList[A : Manifest](
    clazz: java.lang.Class[_]
  ): Either[String, Any] = {
    if (clazz == classOf[List[_]]) {
      Right(List[A]())
    } else if (clazz == classOf[Vector[_]]) {
      Right(Vector[A]())
    } else if (clazz == classOf[::[_]]) {
      Right(Nil)
    } else if (clazz == classOf[IndexedSeq[_]]) {
      Right(IndexedSeq[A]())
    } else if (clazz == classOf[collection.immutable.LinearSeq[_]]) {
      Right(collection.immutable.LinearSeq[A]())
    } else if (clazz == classOf[Seq[_]]) {
      Right(Seq[A]())
    } else if (clazz == classOf[collection.immutable.HashSet[_]]) {
      Right(collection.immutable.HashSet[A]())
    } else if (clazz == classOf[collection.immutable.TreeSet[_]]) {
      val ordering = getOrdering(manifest[A])
      if (!ordering.isEmpty) {
        Right(collection.immutable.TreeSet[A](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("TreeSet not supported for type: " + manifest[A])
      }
    } else if (clazz == classOf[collection.immutable.SortedSet[_]]) {
      val ordering = getOrdering(manifest[A])
      if (!ordering.isEmpty) {
        Right(collection.immutable.SortedSet[A](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("SortedSet not supported for type: " + manifest[A])
      }
    } else if (clazz == classOf[collection.immutable.ListSet[_]]) {
      Right(collection.immutable.ListSet[A]()) 
    } else if (clazz == classOf[Set[_]]) {
      Right(Set[A]())
    } else if (clazz == classOf[collection.immutable.Stack[_]]) {
      Right(collection.immutable.Stack[A]())
    } else if (clazz == classOf[collection.immutable.Queue[_]]) {
      Right(collection.immutable.Queue[A]())
    } else if (clazz == classOf[Stream[_]]) {
      Right(Stream[A]()) 
    } else if (clazz == classOf[collection.mutable.ListBuffer[_]]) {
      Right(collection.mutable.ListBuffer[A]()) 
    } else if (clazz == classOf[collection.mutable.PriorityQueue[_]]) {
      val ordering = getOrdering(manifest[A])
      if (!ordering.isEmpty) {
        Right(collection.mutable.PriorityQueue[A](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("PriorityQueue not supported for type: " + manifest[A])
      }
    } else if (clazz == classOf[collection.mutable.Queue[_]]) {
      Right(collection.mutable.Queue[A]()) 
    } else if (clazz == classOf[collection.mutable.HashSet[_]]) {
      Right(collection.mutable.HashSet[A]()) 
    } else if (clazz == classOf[collection.mutable.LinkedHashSet[_]]) {
      Right(collection.mutable.LinkedHashSet[A]()) 
    } else if (clazz == classOf[collection.mutable.Set[_]]) {
      Right(collection.mutable.Set[A]()) 
    } else if (clazz == classOf[collection.mutable.ArrayBuffer[_]]) {
      Right(collection.mutable.ArrayBuffer[A]()) 
    } else if (clazz == classOf[collection.mutable.ResizableArray[_]]) {
      Right(collection.mutable.ResizableArray[A]()) 
    } else if (clazz == classOf[collection.mutable.ArrayStack[_]]) {
      Right(collection.mutable.ArrayStack[A]()) 
    } else if (clazz == classOf[collection.mutable.Stack[_]]) {
      Right(collection.mutable.Stack[A]()) 
    } else if (clazz == classOf[collection.mutable.LinkedList[_]]) {
      Right(collection.mutable.LinkedList[A]()) 
    } else if (clazz == classOf[collection.mutable.DoubleLinkedList[_]]) {
      Right(collection.mutable.DoubleLinkedList[A]()) 
    } else if (clazz == classOf[collection.mutable.MutableList[_]]) {
      Right(collection.mutable.MutableList[A]()) 
    } else if (clazz == classOf[collection.mutable.ArraySeq[_]]) {
      Right(collection.mutable.ArraySeq[A]()) 
    } else if (clazz == classOf[collection.mutable.IndexedSeq[_]]) {
      Right(collection.mutable.IndexedSeq[A]())
    } else if (clazz == classOf[collection.mutable.LinearSeq[_]]) {
      Right(collection.mutable.LinearSeq[A]()) 
    } else if (clazz == classOf[collection.mutable.Seq[_]]) {
      Right(collection.mutable.Seq[A]()) 
    } else if (clazz == classOf[collection.mutable.UnrolledBuffer[_]]) {
      Right(collection.mutable.UnrolledBuffer[A]()) 
    } else if (clazz == classOf[collection.mutable.Buffer[_]]) {
      Right(collection.mutable.Buffer[A]()) 
    } else {
      Left("Unknown type: " + clazz)
    }
  }

  private def updateList(obj: Any, item: Any): Either[String, Any] = {
    if (obj.isInstanceOf[List[_]]) {
      Right(item :: obj.asInstanceOf[List[_]])
    } else if (obj.isInstanceOf[::[_]]) {
      Right(item :: obj.asInstanceOf[::[_]])
    } else if (obj.isInstanceOf[Vector[_]]) {
      Right(obj.asInstanceOf[Vector[_]] :+ item)
    } else if (obj.isInstanceOf[Seq[_]]) {
      Right(obj.asInstanceOf[Seq[_]] :+ item)
    } else if (obj.isInstanceOf[IndexedSeq[_]]) {
      Right(obj.asInstanceOf[IndexedSeq[_]] :+ item)
    } else if (obj.isInstanceOf[collection.immutable.LinearSeq[_]]) {
      Right(obj.asInstanceOf[collection.immutable.LinearSeq[_]] :+ item)
    } else if (obj.isInstanceOf[collection.immutable.HashSet[_]]) {
      Right(obj.asInstanceOf[collection.immutable.HashSet[Any]] + item)
    } else if (obj.isInstanceOf[collection.immutable.TreeSet[_]]) {
      Right(obj.asInstanceOf[collection.immutable.TreeSet[Any]] + item)
    } else if (obj.isInstanceOf[collection.immutable.SortedSet[_]]) {
      Right(obj.asInstanceOf[collection.immutable.SortedSet[Any]] + item)
    } else if (obj.isInstanceOf[collection.immutable.ListSet[_]]) {
      Right(obj.asInstanceOf[collection.immutable.ListSet[Any]] + item)
    } else if (obj.isInstanceOf[Set[_]]) {
      Right(obj.asInstanceOf[Set[Any]] + item)
    } else if (obj.isInstanceOf[collection.immutable.Stack[_]]) {
      Right(item +: obj.asInstanceOf[collection.immutable.Stack[_]])
    } else if (obj.isInstanceOf[collection.immutable.Queue[_]]) {
      Right(obj.asInstanceOf[collection.immutable.Queue[_]] :+ item)
    } else if (obj.isInstanceOf[Stream[_]]) {
      Right(item #:: obj.asInstanceOf[Stream[Any]])
    } else if (obj.isInstanceOf[collection.mutable.ListBuffer[_]]) {
      Right(obj.asInstanceOf[collection.mutable.ListBuffer[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.PriorityQueue[_]]) {
      Right(obj.asInstanceOf[collection.mutable.PriorityQueue[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.Queue[_]]) {
      Right(obj.asInstanceOf[collection.mutable.Queue[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.HashSet[_]]) {
      Right(obj.asInstanceOf[collection.mutable.HashSet[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.LinkedHashSet[_]]) {
      Right(obj.asInstanceOf[collection.mutable.LinkedHashSet[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.Set[_]]) {
      Right(obj.asInstanceOf[collection.mutable.Set[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.ArrayBuffer[_]]) {
      Right(obj.asInstanceOf[collection.mutable.ArrayBuffer[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.ResizableArray[_]]) {
      Right(obj.asInstanceOf[collection.mutable.ResizableArray[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.ArrayStack[_]]) {
      Right(item +: obj.asInstanceOf[collection.mutable.ArrayStack[Any]])
    } else if (obj.isInstanceOf[collection.mutable.Stack[_]]) {
      Right(item +: obj.asInstanceOf[collection.mutable.Stack[Any]])
    } else if (obj.isInstanceOf[collection.mutable.LinkedList[_]]) {
      Right(obj.asInstanceOf[collection.mutable.LinkedList[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.DoubleLinkedList[_]]) {
      Right(obj.asInstanceOf[collection.mutable.DoubleLinkedList[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.MutableList[_]]) {
      Right(obj.asInstanceOf[collection.mutable.MutableList[Any]] += item)
    } else if (obj.isInstanceOf[collection.mutable.ArraySeq[_]]) {
      Right(obj.asInstanceOf[collection.mutable.ArraySeq[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.IndexedSeq[_]]) {
      Right(obj.asInstanceOf[collection.mutable.IndexedSeq[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.LinearSeq[_]]) {
      Right(obj.asInstanceOf[collection.mutable.LinearSeq[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.Seq[_]]) {
      Right(obj.asInstanceOf[collection.mutable.Seq[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.UnrolledBuffer[_]]) {
      Right(obj.asInstanceOf[collection.mutable.UnrolledBuffer[Any]] :+ item)
    } else if (obj.isInstanceOf[collection.mutable.Buffer[_]]) {
      Right(obj.asInstanceOf[collection.mutable.Buffer[Any]] :+ item)
    } else {
      Left("Unknown type: " + obj.getClass) 
    }
  }

  private def isMapType(m: Manifest[_]): Boolean = {
    isMapType(m.erasure)
  }

  private def isMapType(clazz: Class[_]): Boolean = {
    ( classOf[Map[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.HashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.TreeMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.SortedMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.immutable.ListMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.Map[_,_]].isAssignableFrom(clazz) || 
      classOf[collection.mutable.HashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.WeakHashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.LinkedHashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.OpenHashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[collection.mutable.ListMap[_,_]].isAssignableFrom(clazz) ) 
  }

  private def createMapManifest[A : Manifest](
    clazz: java.lang.Class[_]
  ): Either[String, Manifest[_]] = {
    if (clazz == classOf[Map[_,_]]) {
      Right(manifest[Map[Symbol, A]])
    } else if (clazz == classOf[collection.immutable.HashMap[_,_]]) {
      Right(manifest[collection.immutable.HashMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.immutable.TreeMap[_,_]]) {
      Right(manifest[collection.immutable.TreeMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.immutable.SortedMap[_,_]]) {
      Right(manifest[collection.immutable.SortedMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.immutable.ListMap[_,_]]) {
      Right(manifest[collection.immutable.ListMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.mutable.Map[_,_]]) {
      Right(manifest[collection.mutable.Map[Symbol, A]]) 
    } else if (clazz == classOf[collection.mutable.HashMap[_,_]]) {
      Right(manifest[collection.mutable.HashMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.mutable.WeakHashMap[_,_]]) {
      Right(manifest[collection.mutable.WeakHashMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.mutable.LinkedHashMap[_,_]]) {
      Right(manifest[collection.mutable.LinkedHashMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.mutable.OpenHashMap[_,_]]) {
      Right(manifest[collection.mutable.OpenHashMap[Symbol, A]]) 
    } else if (clazz == classOf[collection.mutable.ListMap[_,_]]) {
      Right(manifest[collection.mutable.ListMap[Symbol, A]]) 
    } else {
      Left("Unknown type: " + clazz)
    }
  }

  private def createMap[A, B](
    a: Manifest[A],
    b: Manifest[B],
    clazz: java.lang.Class[_]
  ): Either[String, Any] = {a
    if (clazz == classOf[Map[_,_]]) {
      Right(Map[A, B]())
    } else if (clazz == classOf[collection.immutable.HashMap[_,_]]) {
      Right(collection.immutable.HashMap[A, B]()) 
    } else if (clazz == classOf[collection.immutable.TreeMap[_,_]]) {
      val ordering = getOrdering(a)
      if (!ordering.isEmpty) {
        Right(collection.immutable.TreeMap[A,B](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("TreeMap not supported for type: " + a)
      }
    } else if (clazz == classOf[collection.immutable.SortedMap[_,_]]) {
      val ordering = getOrdering(a)
      if (!ordering.isEmpty) {
        Right(collection.immutable.SortedMap[A,B](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("SortedMap not supported for type: " + a)
      }
    } else if (clazz == classOf[collection.immutable.ListMap[_,_]]) {
      Right(collection.immutable.ListMap[A, B]()) 
    } else if (clazz == classOf[collection.mutable.Map[_,_]]) {
      Right(collection.mutable.Map[A, B]()) 
    } else if (clazz == classOf[collection.mutable.HashMap[_,_]]) {
      Right(collection.mutable.HashMap[A, B]()) 
    } else if (clazz == classOf[collection.mutable.WeakHashMap[_,_]]) {
      Right(collection.mutable.WeakHashMap[A, B]()) 
    } else if (clazz == classOf[collection.mutable.LinkedHashMap[_,_]]) {
      Right(collection.mutable.LinkedHashMap[A, B]()) 
    } else if (clazz == classOf[collection.mutable.OpenHashMap[_,_]]) {
      Right(collection.mutable.OpenHashMap[A, B]())
    } else if (clazz == classOf[collection.mutable.ListMap[_,_]]) {
      Right(collection.mutable.ListMap[A, B]()) 
    } else {
      Left("Unknown type: " + clazz)
    }
  }

  private def updateMap(obj: Any, name: Any, item: Any): Either[String, Any] = {
    if (obj.isInstanceOf[Map[_,_]]) {
      Right(obj.asInstanceOf[Map[Any,Any]] + (name -> item))
    } else if (obj.isInstanceOf[collection.immutable.HashMap[_,_]]) {
      Right(obj.asInstanceOf[collection.immutable.HashMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.immutable.TreeMap[_,_]]) {
      Right(obj.asInstanceOf[collection.immutable.TreeMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.immutable.SortedMap[_,_]]) {
      Right(obj.asInstanceOf[collection.immutable.SortedMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.immutable.ListMap[_,_]]) {
      Right(obj.asInstanceOf[collection.immutable.ListMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.mutable.Map[_,_]]) {
      Right(obj.asInstanceOf[collection.mutable.Map[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.mutable.HashMap[_,_]]) {
      Right(obj.asInstanceOf[collection.mutable.HashMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.mutable.WeakHashMap[_,_]]) {
      Right(obj.asInstanceOf[collection.mutable.WeakHashMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.mutable.LinkedHashMap[_,_]]) {
      Right(obj.asInstanceOf[collection.mutable.LinkedHashMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.mutable.OpenHashMap[_,_]]) {
      Right(obj.asInstanceOf[collection.mutable.OpenHashMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[collection.mutable.ListMap[_,_]]) {
      Right(obj.asInstanceOf[collection.mutable.ListMap[Any,Any]] + 
          (name -> item))
    } else {
      Left("Unknown type: " + obj.getClass)
    }
  }

} // end package object
