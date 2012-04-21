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

/** Contains utils for XML parsing.
  *
  * The XML must conform to the following format:
  *
  * 

  * The rest of the features are similar to the JSON utilities. The following
  * is a summary of features:
  * {{{
  * // Parsing
  * fromXml[List[Int]]("[1,2,3]")           // Option[List[Int]] 
  * toXml(List(1,2,3))                      // "[1,2,3]"
  * fromXml[List[Short]]("[1,2,3]")         // Option[List[Short]] 
  * toXml(List[Short](1,2,3))               // "[1,2,3]"
  * fromXml[List[Long]]("[1,2,3]")          // Option[List[Long]]
  * toXml(List[Long](1,2,3))                // "[1,2,3]"
  * fromXml[List[Float]]("[1.0,2.0]")       // Option[List[Float]]
  * toXml(List[Float](1.0,2.0))             // "[1.0,2.0]"
  * fromXml[List[Double]]("[1.0,2.0]")      // Option[List[Double]]
  * toXml(List[Double](1.0,2.0))            // "[1.0,2.0]"
  * fromXml[List[Boolean]]("[true, false]") // Option[List[Boolean]]
  * toXml(List[Boolean](true, false))       // "[true,false]"
  * fromXml[List[Char]]("[\"a\",\"b\"]")    // Option[List[Char]]
  * toXml(List[Char]('a','b'))              // "[\"a\",\"b\"]"
  * fromXml[List[String]]("[\"a\",\"b\"]")  // Option[List[String]]
  * toXml(List[Char]("a","b"))              // "[\"a\",\"b\"]"
  *
  * fromXml[Map[Symbol,String]]("{\"foo\": \"bar\"}")
  * toXml(Map('foo -> "bar"))              // "{\"foo\": \"bar\"}"
  * fromXml[Map[Symbol,List[Int]]]("{\"foo\": [1,2,3]}")
  * toXml(Map('foo -> [1,2,3]))            // "{\"foo\": [1,2,3]}"
  *
  * // Parsing (Maps from any primitive type) 
  * fromXml[Map[String,String]]("{\"foo\": \"bar\"}")
  * toXml(Map("foo" -> "bar"))             // "{\"foo\": \"bar\"}"
  * fromXml[Map[Int,Int]]("{\"1\": 2, \"2\": 4}")
  * toXml(Map(1 -> 2, 2 -> 4))             // "{\"1\": 2, \"2\": 4}"
  *
  * // Parsing (lazy parsing)
  * fromXml[Stream[Int]]("[1,2,3]").get take 2 print // 1, 2 
  * fromXml[Iterable[Int]]("[1,2,3]").get.iterator   // hasNext/next...
  * fromXml[Iterator[Int]]("[1,2,3]").get            // hasNext/next...
  *
  * // Parsing (tuple data)
  * fromXml[Tuple2[Int,Int]]("[1,2]")
  * toXml(1 -> 2)                          // "[1,2]"
  * fromXml[List[Tuple2[Symbol,Int]]]("[[x,1],[y,2]]")
  * toXml(List('x -> 1, 'y -> 2))          // "[[x,1],[y,2]]"
  *
  * // convenience function for above - handles errors gracefully
  * val iter = xmlIterator[Int]("[1,2,3]").map(_ + 1) 
  * iter.next               // 2 
  *
  * // Parsing (with field casing conversion)
  * // Some(Map("myField" -> 1))
  * fromXml[Map[String,Int]]("{\"MyField\": 1}", LowerCamelCase)
  * toXml(Map("myField" -> 1), UpperCamelCase) // "{\"MyField\": 1}"
  *
  * // Some(Map("my_field" -> 1))
  * fromXml[Map[String,Int]]("{\"MyField\": 1}", LowerSnakeCase)
  * toXml(Map("MyField" -> 1), LowerSnakeCase) // "{\"my_field\": 1}"
  *
  * // Parsing (using Any - chooses best type)
  * fromXml[Any]("[1,2,3]")         // Option[Any] (points to List[Int]) 
  * fromXml[Any]("[1,9999999999]")  // Option[Any] (points to List[Long]) 
  * fromXml[List[Any]]("[1,2,3]")   // Option[List[Any]] (points to List[Int]) 
  * fromXml[Map[String,Any]]("{\"foo\": \"bar\"}")  // ...  Map[Symbol,String] 
  * fromXml[Any]("{\"foo\": 1}")    // Option[Any] (points to Map[Symbol,Int]) 
  *
  * // Parsing (using reflection)
  * case class Foo(s: String, i: Int)
  * fromXml[List[Foo]]("[{\"s\": \"foo\", \"i\": 1}]")  // Option[List[Foo]]
  * toXml(List(Foo("foo", 1)))
  * fromXml[Map[Int,Foo]]("{\"1\": {\"s\": \"foo\", \"i\": 1}}")
  * toXml(Map(1 -> Foo("foo", 1)))
  *
  * val iter = xmlIterator[Foo]("[{\"s\": \"foo\", \"i\": 1}]")
  * iter.next               // Foo("foo", 1) 
  *
  * // Parsing (using reflection - different constructors)
  * class Foo(val s: String, val i: Int) {
  *   def this(s: String) = this(s, 1)
  *   def this(i: Int) = this("bar", i)
  * }
  * fromXml[List[Foo]]("[{\"s\": \"foo\"}]")  // List(Foo("foo", 1)) 
  * fromXml[List[Foo]]("[{\"i\": 10}]")       // List(Foo("bar", 10)) 
  * 
  * // Parsing (using reflection - embedded objects)
  * case class Bar(f: Foo)
  * fromXml[List[Bar]]("[{\"f\": {\"s\": \"foo\", \"i\": 1}}]")
  *
  * // Parsing (using reflection - embedded Seqs/Maps)
  * case class Baz(xs: List[Int])
  * fromXml[List[Baz]]("[{\"xs\": [1,2,3]}]") // List(Baz("xs", List(1,2,3))) 
  *
  * NOTE: Due to type erasure, only Lists/Maps of the default types are
  *       supported (String, Int (Long), Double, and Boolean). Maps are from
  *       Symbol to these types. Use of other types will return WITHOUT error
  *
  * // Parsing (using Uniform types)
  * fromXml[UniformMap[Any]]("{\"foo\": 1}")  // Option[UniformMap[Any]]
  * toXml(UniformMap('foo -> 1))              // "{\"foo\":1}"
  * fromXml[UniformList[UniformList[Any]]]("[[1,2],[3,4]]")
  * toXml(UniformList(UniformList(1,2),UniformList(3,4))) // "[[1,2],[3,4]]"
  * 
  * // Parsing (using Any in combination with Reifiable)
  * implicit val xmlSettings = XmlSettings(
  *   IgnoreCasing,
  *   PrettyPrintSettings(true, 2),
  *   OpaqueDataSettings(false),
  *   ReifiableSettings(true)                 // enable reifiable
  * )
  *
  * val v = fromXml[Any]("[1,9999999999]") 
  * v.isType[List[Int]]              // false 
  * v.isType[List[Long]]             // true 
  *
  * val v = fromXml[Any]("{\"foo\": 1}") 
  * v.isType[Map[Symbol,String]]     // false 
  * v.isType[Map[Symbol,Int]]        // true
  *
  * // Parsing (using other container types - any scala mutable/immutable type)
  * fromXml[Vector[Int]]("[1,2,3]") // Option[Vector[Int]] 
  * toXml(Vector(1,2,3))            // "[1,2,3]"
  * fromXml[Seq[Int]]("[1,2,3]")    // Option[Seq[Int]] 
  * toXml(Seq(1,2,3))               // "[1,2,3]"
  * fromXml[ListBuffer[Int]]("[1,2,3]") // Option[ListBuffer[Int]] 
  *
  * // Parsing (pretty printing)
  * // "{
  * //    \"foo\": \"bar\"
  * //  }"
  * toXml(Map("foo" -> "bar"), true, 2) 
  *
  * // Extracting
  * "{\"foo\": 1}" match {
  *   case Xml(xm) =>
  *     println("matched: " + xm)    // prints: Map(foo -> 1) 
  * }
  *
  * // Combining with other types
  * // Map("foo" -> "bar", "foo2" -> "bar2")
  * Map("foo" -> "bar").withXml("{\"foo2\": \"bar2\"}") { _ ++ _ }
  *
  * // Configuring default settings
  * implicit val xmlSettings = XmlSettings(
  *   IgnoreCasing,                 // ignore casing (default)
  *   PrettyPrintSettings(true, 2), // pretty print with 2 spaces (default)
  *   "result"                      // root element called "result" (default)
  *   "item"                        // array items tagged with "item" (default)
  *   OpaqueDataSettings(false),    // disable opaque data store (default)
  *   ReifiableSettings(false)      // disable reifiable types (default)
  * )
  * }}}
  */
package object xml {

  ///////////////////////////////////////////////////////////////////////////
  // vals and implicits 
  ///////////////////////////////////////////////////////////////////////////

  case class XmlSettings(
    fieldCasing: Casing, 
    prettyPrintSettings: PrettyPrintSettings,
    rootTag: String,
    arrayItemTag: String,
    opaqueDataSettings: OpaqueDataSettings,
    reifiableSettings: ReifiableSettings
  ) extends ParserSettings

  implicit val xmlSettings = XmlSettings(
    IgnoreCasing, 
    PrettyPrintSettings(false, 2),
    "result",
    "item",
    OpaqueDataSettings(false),
    ReifiableSettings(false))


  ///////////////////////////////////////////////////////////////////////////
  // Package Functions
  ///////////////////////////////////////////////////////////////////////////

  /** Converts RFC 4627 compatible XML data to a specified type.
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
    * When reflection is used, the XML field names must match the object field
    * names. They must also be specified in the order the constructor takes
    * them. Multiple constructors may be used as long as a constructor matching
    * the fields passed exists.
    * 
    * Examples:
    * {{{
    * // Type is List[Int]
    * val listOfInt = fromXml[List[Int]]("[1, 2, 3]")
    *
    * // Type is Any pointing to default List[Int] type
    * val listOfInt = fromXml[Any]("[1, 2, 3]")
    *
    * // Type is Map[Symbol, List[Int]]
    * val mapOfSymbolToListOfInt = 
    *   fromXml[Map[Symbol, List[Int]]]("{ \"foo\": [1, 2, 3] }")
    *
    * // Type is List[Foo]
    * case class Foo(s: String, i: Int)
    * fromXml[List[Foo]]("[{\"s\": \"foo\", \"i\": 1}]") 
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
    * @param xml XML string 
    * @return List, Map, or Primitive 
    */
  def fromXml[A](xml: Iterable[Char])(
    implicit m: Manifest[A], settings: XmlSettings 
  ): Option[A] = XmlParser.fromXmlOption[A](xml, settings) 

  /** fromXml with an explicit casing for fields */
  def fromXml[A](xml: Iterable[Char], fieldCasing: Casing)(
    implicit m: Manifest[A], settings: XmlSettings 
  ): Option[A] = {
    XmlParser.fromXmlOption(xml,
      XmlSettings(fieldCasing, settings.prettyPrintSettings, settings.rootTag, 
        settings.arrayItemTag, settings.opaqueDataSettings, 
        settings.reifiableSettings))
  }

  /** fromXml with an explicit reifiable setting */
  def fromXml[A](xml: Iterable[Char], useReifiableType: Boolean)(
    implicit m: Manifest[A], settings: XmlSettings 
  ): Option[A] = {
    XmlParser.fromXmlOption(xml,
      XmlSettings(settings.fieldCasing, settings.prettyPrintSettings,
        settings.rootTag, settings.arrayItemTag, settings.opaqueDataSettings,
        ReifiableSettings(useReifiableType)))
  }

  /** Convenience method for fromXml[Iterator[A]]("...").get 
    *
    * Handles errors gracefully by returning false for hasNext
    */
  def xmlIterator[A](xml: Iterable[Char])(
    implicit m: Manifest[A], settings: XmlSettings 
  ): Iterator[A] = {
    XmlParser.fromXml[Iterator[A]](
        xml, settings)(manifest[Iterator[A]]) match {
      case Right(r) => r 
      case Left(l) => new Iterator[A]() { 
        def hasNext = false
        def next() = throw new NoSuchElementException("next on empty iterator")
      }
    }
  }

  /** Converts given value to an RFC 4627 compatible XML string
    *
    * Typically the value passed will be a List of values or a Map of
    * values. However, this method will also support passing a primitive 
    *
    * Examples:
    * {{{
    * toXml(List(1,2,3))      // "[1,2,3]"
    * toXml(Map("foo" -> 1))  // "{\"foo\":1}"
    * }}}
    *
    * @param value values (typically List or Map)
    * @param pretty print optional pretty printing 
    * @return string XML data
    */
  def toXml(x: Any)(implicit settings: XmlSettings): String = 
    XmlParser.toXml(x, settings)

  /** toXml with explicit pretty printing settings */
  def toXml(x: Any, prettyPrint: Boolean, indent: Int = 2)(
    implicit settings: XmlSettings
  ): String = 
    XmlParser.toXml(x, 
      XmlSettings(settings.fieldCasing, 
        PrettyPrintSettings(prettyPrint, indent),
        settings.rootTag, settings.arrayItemTag,
        settings.opaqueDataSettings, settings.reifiableSettings))

  /** toXml with explicit casing setting */
  def toXml(x: Any, fieldCasing: Casing)(
    implicit settings: XmlSettings
  ): String = 
    XmlParser.toXml(x, 
      XmlSettings(fieldCasing, settings.prettyPrintSettings, settings.rootTag,
        settings.arrayItemTag, settings.opaqueDataSettings, 
        settings.reifiableSettings))

  /** toXml with explicit meta/data tags setting */
  def toXml(x: Any, metaTags: Seq[Symbol], dataTags: Seq[Symbol])(
    implicit settings: XmlSettings
  ): String = 
    XmlParser.toXml(x, 
      XmlSettings(settings.fieldCasing, settings.prettyPrintSettings, 
        settings.rootTag, settings.arrayItemTag, settings.opaqueDataSettings,
        settings.reifiableSettings))

  /** toXml with explicit casing and pretty print settings */
  def toXml(
    x: Any, fieldCasing: Casing, prettyPrint: Boolean, indent: Int
  ): String = 
    XmlParser.toXml(x, 
      XmlSettings(fieldCasing, PrettyPrintSettings(prettyPrint, indent), 
        xmlSettings.rootTag, xmlSettings.arrayItemTag, 
        xmlSettings.opaqueDataSettings, xmlSettings.reifiableSettings))


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  object Xml {
    def unapply(xml: String)(
      implicit settings: XmlSettings 
    ): Option[Any] = {
      XmlParser.fromXmlOption(xml, settings)(manifest[Any])
    }
  }

  object IterableXml {
    /** Extractor for Xml for when data input as iterable.
      *
      * NOTE: Because of type erasure this will not match on _* patterns.
      */
    def unapply(xml: Iterable[Char])(
      implicit settings: XmlSettings 
    ): Option[Any] = {
      XmlParser.fromXmlOption(xml, settings)(manifest[Any])
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  implicit def any2XmlPimp[A](x: A)(
    implicit m: Manifest[A], settings: XmlSettings
  ) = new XmlPimp[A](x, m, settings)

  final class XmlPimp[A](
    val cur: A,
    val m: Manifest[A], 
    val settings: XmlSettings
  ) {
    /** Applies operator to existing type and type extracted from XML
      * 
      * @param xml xml data
      * @param op operation taking current type and type converted from XML
      * @return type from apply operation or current type if conversion fails
      */
    def withXml(xml: Iterable[Char])(op: (A, A) => A): A = {
      XmlParser.fromXmlOption(xml, settings)(m) match {
        case Some(v) => op(cur, v) 
        case _ => cur
      }
    }
  
    /** withXml with an explicit casing for fields */
    def withXml(
      xml: Iterable[Char], fieldCasing: Casing
    )(op: (A,A) => A): A = {
      XmlParser.fromXmlOption(xml, XmlSettings(fieldCasing, 
          settings.prettyPrintSettings, settings.rootTag, 
          settings.arrayItemTag, settings.opaqueDataSettings, 
          settings.reifiableSettings))(m) match {
        case Some(v) => op(cur, v) 
        case _ => cur
      }
    }

    def toXml(): String = XmlParser.toXml(cur, settings)
  }

} // end package object
