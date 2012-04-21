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

import java.io.Writer
import java.text.DateFormat
import java.text.SimpleDateFormat

import scala.io.Source

import scalafy.types.meta.OpaqueDataSettings
import scalafy.types.reifiable.ReifiableSettings
import scalafy.util.casing._
import scalafy.util.parser.ParserSettings

/** Utils for XML parsing.
  *
  * The XML must follow certain guidelines. XML attributes are not used except
  * for specifying nil values and when escaping is required. The elements are
  * always placed under a single root element called 'result'. Lists items are
  * always called 'item' and are added in the order they appear in the list. Do
  * not mix list items with non-list items. In cases where a field name
  * contains characters reserved by XML (e.g. <, >, ', ", or &), the XML
  * element name will be set to '__escaped__' and the element will contain a
  * '__value__' attribute containing the name that would have been used but
  * escaped using proper entity encodings (e.g. & lt;, & gt;, & quot;, & amp;).
  * The following are examples of valid XML:
  * {{{
  * // Classes
  * case class Foo(s: String, i: Int)
  * toXml(List(Foo("test", 3), Foo("test2", 10)))
  *
  * <result>
  *  <item>
  *    <s>test</s>
  *    <i>3</i>
  *  </item>
  *  <item>
  *    <s>test2</s>
  *    <i>10</i>
  *  </item>
  * </result>
  *
  * // Maps
  * toXml(Map(5 -> "test", 20 -> null))
  *
  * <result>
  *  <__escaped __value="5">test</__escaped__>
  *  <__escaped __value="20" xsi:nil="true"></__escaped__>
  * </result>
  * }}}
  *
  * The rest of the features are similar to the [[scalafy.util.json]]
  * utilities (primitive/list/map/object conversion, lazy parsing, field casing
  * conversion, reifaible type support, uniform types, pretty printing,
  * extration, ...). Just substitute fromJson with fromXml and toJson with
  * toXml in the examples provided. Note the XmlSettings supports two
  * additional settings for the root element tag and array item tag names.
  */
package object xml {

  ///////////////////////////////////////////////////////////////////////////
  // Settings 
  ///////////////////////////////////////////////////////////////////////////

  case class XmlSettings(
    fieldCasing: Casing, 
    prettyPrintSettings: PrettyPrintSettings,
    rootTag: String,
    arrayItemTag: String,
    dateFormatter: DateFormat,
    typeHintSettings: TypeHintSettings,
    classLoader: ClassLoader,
    opaqueDataSettings: OpaqueDataSettings,
    reifiableSettings: ReifiableSettings
  ) extends ParserSettings

  implicit val xmlSettings = XmlSettings(
    IgnoreCasing, 
    PrettyPrintSettings(false, 2),
    "result",
    "item",
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"),
    TypeHintSettings(),
    null, 
    OpaqueDataSettings(false),
    ReifiableSettings(false))


  ///////////////////////////////////////////////////////////////////////////
  // Package Functions
  ///////////////////////////////////////////////////////////////////////////

  /** Converts XML text to a specified type.
    *
    * See [[scalafy.util.json]] for a list of examples (substitute
    * fromJson with fromXml)
    *
    * @param xml XML string 
    * @return object of given type A
    */
  def fromXml[A](xml: Iterable[Char])(
    implicit m: Manifest[A], settings: XmlSettings 
  ): Option[A] = XmlParser.fromXml[A](xml, settings) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }

  def fromXml[A](source: Source)(
    implicit m: Manifest[A], settings: XmlSettings 
  ): Option[A] = XmlParser.fromXml[A](source, settings) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }


  /** fromXml with an explicit casing for fields */
  def fromXml[A](xml: Iterable[Char], fieldCasing: Casing)(
    implicit m: Manifest[A], settings: XmlSettings 
  ): Option[A] = {
    fromXml(xml)(m,
      XmlSettings(fieldCasing, settings.prettyPrintSettings, settings.rootTag, 
        settings.arrayItemTag, settings.dateFormatter, 
        settings.typeHintSettings, settings.classLoader, 
        settings.opaqueDataSettings, settings.reifiableSettings))
  }

  /** fromXml with an explicit typeHintSetting */
  def fromXml[A](
    xml: Iterable[Char], typeHintSettings: TypeHintSettings 
  )(
    implicit m: Manifest[A], settings: XmlSettings
  ): Option[A] = {
    fromXml(xml)(m,
      XmlSettings(settings.fieldCasing, settings.prettyPrintSettings,
        settings.rootTag, settings.arrayItemTag, settings.dateFormatter,
        typeHintSettings, settings.classLoader, 
        settings.opaqueDataSettings, settings.reifiableSettings))
  }

  /** fromXml with explicit enumeration type hints */
  def fromXml[A](
    xml: Iterable[Char], enumTypeHints: List[Enumeration] 
  )(
    implicit m: Manifest[A], settings: XmlSettings
  ): Option[A] = {
    fromXml(xml)(m,
      XmlSettings(settings.fieldCasing, settings.prettyPrintSettings,
        settings.rootTag, settings.arrayItemTag, settings.dateFormatter,
        TypeHintSettings(enumTypeHints, settings.typeHintSettings.classes), 
        settings.classLoader, settings.opaqueDataSettings, 
        settings.reifiableSettings))
  }

  /** fromXml with explicit class type hints */
  def fromXml[A](
    xml: Iterable[Char], classTypeHints: Map[Class[_], Map[Symbol,Manifest[_]]] 
  )(
    implicit m: Manifest[A], settings: XmlSettings
  ): Option[A] = {
    fromXml(xml)(m,
      XmlSettings(settings.fieldCasing, settings.prettyPrintSettings,
        settings.rootTag, settings.arrayItemTag, settings.dateFormatter,
        TypeHintSettings(settings.typeHintSettings.enums, classTypeHints), 
        settings.classLoader, settings.opaqueDataSettings, 
        settings.reifiableSettings))
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

  /** Converts given value to an XML string
    * 
    * See [[scalafy.util.json]] for a list of examples (substitute
    * toJson with toXml)
    *
    * @return string XML data
    */
  def toXml(x: Any)(implicit settings: XmlSettings): String = 
    XmlParser.toXml(x, settings)

  /** toXml using Writer */
  def toXml(writer: Writer, x: Any)(implicit settings: XmlSettings): Unit = 
    XmlParser.toXml(writer, x, settings)

  /** toXml with explicit pretty printing settings */
  def toXml(x: Any, prettyPrint: Boolean, indent: Int = 2)(
    implicit settings: XmlSettings
  ): String = 
    XmlParser.toXml(x, 
      XmlSettings(settings.fieldCasing, 
        PrettyPrintSettings(prettyPrint, indent),
        settings.rootTag, settings.arrayItemTag, settings.dateFormatter,
        settings.typeHintSettings, settings.classLoader, 
        settings.opaqueDataSettings, settings.reifiableSettings))

  /** toXml with explicit casing setting */
  def toXml(x: Any, fieldCasing: Casing)(
    implicit settings: XmlSettings
  ): String = 
    XmlParser.toXml(x, 
      XmlSettings(fieldCasing, settings.prettyPrintSettings, settings.rootTag,
        settings.arrayItemTag, settings.dateFormatter, 
        settings.typeHintSettings, settings.classLoader,
        settings.opaqueDataSettings, settings.reifiableSettings))

  /** toXml with explicit casing and pretty print settings */
  def toXml(
    x: Any, fieldCasing: Casing, prettyPrint: Boolean, indent: Int
  )(
    implicit settings: XmlSettings
  ): String = 
    XmlParser.toXml(x, 
      XmlSettings(fieldCasing, PrettyPrintSettings(prettyPrint, indent), 
        settings.rootTag, settings.arrayItemTag, 
        settings.dateFormatter, settings.typeHintSettings, settings.classLoader,
        settings.opaqueDataSettings, settings.reifiableSettings))


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  object Xml {
    def unapply(xml: String)(
      implicit settings: XmlSettings 
    ): Option[Any] = {
      fromXml(xml)(manifest[Any], settings)
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
      fromXml(xml)(manifest[Any], settings)
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
      XmlParser.fromXml(xml, settings)(m) match {
        case Right(v) => op(cur, v) 
        case _ => cur
      }
    }
  
    /** withXml with an explicit casing for fields */
    def withXml(
      xml: Iterable[Char], fieldCasing: Casing
    )(op: (A,A) => A): A = {
      XmlParser.fromXml(xml, XmlSettings(fieldCasing, 
          settings.prettyPrintSettings, settings.rootTag, 
          settings.arrayItemTag, settings.dateFormatter, 
          settings.typeHintSettings, settings.classLoader,
          settings.opaqueDataSettings, settings.reifiableSettings))(m) match {
        case Right(v) => op(cur, v) 
        case _ => cur
      }
    }

    def toXml(): String = XmlParser.toXml(cur, settings)
  }

} // end package object
