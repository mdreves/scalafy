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

import scalafy.types.meta.OpaqueDataSettings
import scalafy.types.reifiable.ReifiableSettings
import scalafy.util.casing._
import scalafy.util.parser.ParserSettings

/** Contains utils for JSON parsing.
  *
  * The main difference from other libraries is there are no JField, JInt,...
  * Everything is parsed to standard primitives. Additional features:
  *  <li>Ability to parse data lazily (Stream, Iterator, Iterable)</li>
  *  <li>Ability to do field casing conversion during parsing</li>
  *  <li>Ability to parse to any collection type (immutable and mutable)</li> 
  *  <li>Ability to use any primitive type (Map[Int,Int],Map[Char,Int],...)</li>
  *  <li>Ability to parse from any class that is supported by reflection</li>
  *  <li>Ability to parse classes with embedded Seqs/Maps (within limits)</li> 
  *  <li>Ability to parse classes with multiple constructors</li>
  *  <li>Ability to use Any and have parser choose the best Seq/Map type</li>
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
  * // Parsing (tuple data)
  * fromJson[Tuple2[Int,Int]]("[1,2]")
  * toJson(1 -> 2)                          // "[1,2]"
  * fromJson[List[Tuple2[Symbol,Int]]]("[[x,1],[y,2]]")
  * toJson(List('x -> 1, 'y -> 2))          // "[[x,1],[y,2]]"
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
  * class Foo(val s: String, val i: Int) {
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
  * // Parsing (using reflection - embedded Seqs/Maps)
  * case class Baz(xs: List[Int])
  * fromJson[List[Baz]]("[{\"xs\": [1,2,3]}]") // List(Baz("xs", List(1,2,3))) 
  *
  * NOTE: Due to type erasure, only Lists/Maps of the default types are
  *       supported (String, Int (Long), Double, and Boolean). Maps are from
  *       Symbol to these types. Use of other types will return WITHOUT error
  *
  * // Parsing (using Uniform types)
  * fromJson[UniformMap[Any]]("{\"foo\": 1}")  // Option[UniformMap[Any]]
  * toJson(UniformMap('foo -> 1))              // "{\"foo\":1}"
  * fromJson[UniformList[UniformList[Any]]]("[[1,2],[3,4]]")
  * toJson(UniformList(UniformList(1,2),UniformList(3,4))) // "[[1,2],[3,4]]"
  * 
  * // Parsing (using Any in combination with Reifiable)
  * implicit val jsonSettings = JsonSettings(
  *   IgnoreCasing,
  *   PrettyPrintSettings(true, 2),
  *   OpaqueDataSettings(false),
  *   ReifiableSettings(true)                 // enable reifiable
  * )
  *
  * val v = fromJson[Any]("[1,9999999999]") 
  * v.isType[List[Int]]              // false 
  * v.isType[List[Long]]             // true 
  *
  * val v = fromJson[Any]("{\"foo\": 1}") 
  * v.isType[Map[Symbol,String]]     // false 
  * v.isType[Map[Symbol,Int]]        // true
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
  * implicit val jsonSettings = JsonSettings(
  *   IgnoreCasing,                 // ignore casing (default)
  *   PrettyPrintSettings(true, 2), // pretty print with 2 spaces (default)
  *   OpaqueDataSettings(false),    // disable opaque data store (default)
  *   ReifiableSettings(false)      // disable reifiable types (default)
  * )
  * }}}
  */
package object json {

  ///////////////////////////////////////////////////////////////////////////
  // vals and implicits 
  ///////////////////////////////////////////////////////////////////////////

  case class JsonSettings(
    fieldCasing: Casing, 
    prettyPrintSettings: PrettyPrintSettings,
    opaqueDataSettings: OpaqueDataSettings,
    reifiableSettings: ReifiableSettings
  ) extends ParserSettings

  implicit val jsonSettings = JsonSettings(
    IgnoreCasing, 
    PrettyPrintSettings(false, 2), 
    OpaqueDataSettings(false),
    ReifiableSettings(false))


  ///////////////////////////////////////////////////////////////////////////
  // Package Functions
  ///////////////////////////////////////////////////////////////////////////

  /** Converts RFC 4627 compatible JSON data to a specified type.
    *
    * If the type is set to Any then the most appropriate type will be
    * choosen automatically. The primitives String, Int (Long), Double, and 
    * Boolean are used. Seqs can contain primitives, other Seqs, or Maps. 
    * Maps are from Symbol to either a primitive, Seq, or another Map.
    * If a Seq or Map contains one ore more Longs and the rest are Ints, the
    * entire Seq or Map will be updated to a Seq or Map of Longs. 
    *
    * Specific types may also be passed. In this case, any scala container type
    * List, Vector, Seq, Map, ... (both immutable and mutable) may be used
    * in combination with any primitive type (String, Symbol, Int, Short, Long,
    * Float, Double, Boolean, Char, or Byte).
    *
    * Other object types are supported as long as a conversion is possible
    * using reflection. This limits conversions to objects make up of 
    * primitive types or other objects created from primitive types. Seq
    * and Maps are supported within objects, but due to type erasure only the
    * Seq/Maps that are converted from Any as specified above are supported.
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
    implicit m: Manifest[A], settings: JsonSettings 
  ): Option[A] = JsonParser.fromJsonOption[A](json, settings) 

  /** fromJson with an explicit casing for fields */
  def fromJson[A](json: Iterable[Char], fieldCasing: Casing)(
    implicit m: Manifest[A], settings: JsonSettings 
  ): Option[A] = {
    JsonParser.fromJsonOption(json,
      JsonSettings(fieldCasing, settings.prettyPrintSettings, 
        settings.opaqueDataSettings, settings.reifiableSettings))
  }

  /** fromJson with an explicit reifiable setting */
  def fromJson[A](json: Iterable[Char], useReifiableType: Boolean)(
    implicit m: Manifest[A], settings: JsonSettings 
  ): Option[A] = {
    JsonParser.fromJsonOption(json,
      JsonSettings(settings.fieldCasing, settings.prettyPrintSettings, 
        settings.opaqueDataSettings, ReifiableSettings(useReifiableType)))
  }

  /** Convenience method for fromJson[Iterator[A]]("...").get 
    *
    * Handles errors gracefully by returning false for hasNext
    */
  def jsonIterator[A](json: Iterable[Char])(
    implicit m: Manifest[A], settings: JsonSettings 
  ): Iterator[A] = {
    JsonParser.fromJson[Iterator[A]](
        json, settings)(manifest[Iterator[A]]) match {
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
  def toJson(x: Any)(implicit settings: JsonSettings): String = 
    JsonParser.toJson(x, settings)

  /** toJson with explicit pretty printing settings */
  def toJson(x: Any, prettyPrint: Boolean, indent: Int = 2)(
    implicit settings: JsonSettings
  ): String = 
    JsonParser.toJson(x, 
      JsonSettings(settings.fieldCasing, 
        PrettyPrintSettings(prettyPrint, indent), 
        settings.opaqueDataSettings,
        settings.reifiableSettings))

  /** toJson with explicit casing setting */
  def toJson(x: Any, fieldCasing: Casing)(
    implicit settings: JsonSettings
  ): String = 
    JsonParser.toJson(x, 
      JsonSettings(fieldCasing, settings.prettyPrintSettings, 
        settings.opaqueDataSettings, settings.reifiableSettings))

  /** toJson with explicit meta/data tags setting */
  def toJson(x: Any, metaTags: Seq[Symbol], dataTags: Seq[Symbol])(
    implicit settings: JsonSettings
  ): String = 
    JsonParser.toJson(x, 
      JsonSettings(settings.fieldCasing, settings.prettyPrintSettings, 
        settings.opaqueDataSettings, settings.reifiableSettings))

  /** toJson with explicit casing and pretty print settings */
  def toJson(
    x: Any, fieldCasing: Casing, prettyPrint: Boolean, indent: Int
  ): String = 
    JsonParser.toJson(x, 
      JsonSettings(fieldCasing, PrettyPrintSettings(prettyPrint, indent), 
        jsonSettings.opaqueDataSettings, jsonSettings.reifiableSettings))


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  object Json {
    def unapply(json: String)(
      implicit settings: JsonSettings 
    ): Option[Any] = {
      JsonParser.fromJsonOption(json, settings)(manifest[Any])
    }
  }

  object IterableJson {
    /** Extractor for Json for when data input as iterable.
      *
      * NOTE: Because of type erasure this will not match on _* patterns.
      */
    def unapply(json: Iterable[Char])(
      implicit settings: JsonSettings 
    ): Option[Any] = {
      JsonParser.fromJsonOption(json, settings)(manifest[Any])
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  implicit def any2JsonPimp[A](x: A)(
    implicit m: Manifest[A], settings: JsonSettings
  ) = new JsonPimp[A](x, m, settings)

  final class JsonPimp[A](
    val cur: A,
    val m: Manifest[A], 
    val settings: JsonSettings
  ) {
    /** Applies operator to existing type and type extracted from JSON
      * 
      * @param json json data
      * @param op operation taking current type and type converted from JSON
      * @return type from apply operation or current type if conversion fails
      */
    def withJson(json: Iterable[Char])(op: (A, A) => A): A = {
      JsonParser.fromJsonOption(json, settings)(m) match {
        case Some(v) => op(cur, v) 
        case _ => cur
      }
    }
  
    /** withJson with an explicit casing for fields */
    def withJson(
      json: Iterable[Char], fieldCasing: Casing
    )(op: (A,A) => A): A = {
      JsonParser.fromJsonOption(json, JsonSettings(fieldCasing, 
          settings.prettyPrintSettings, settings.opaqueDataSettings, 
          settings.reifiableSettings))(m) match {
        case Some(v) => op(cur, v) 
        case _ => cur
      }
    }

    def toJson(): String = JsonParser.toJson(cur, settings)
  }

} // end package object
