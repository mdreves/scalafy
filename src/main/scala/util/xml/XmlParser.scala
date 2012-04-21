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
package scalafy.util.xml

import scala.io.Source
import scala.xml._
import scala.xml.parsing.XhtmlEntities
import scala.xml.pull._

import scalafy.collection.mutable.ChunkedIterator
import scalafy.collection.mutable.DefaultChunkedIterator
import scalafy.collection.mutable.StringChunkedIterator
import scalafy.types.meta._
import scalafy.types.uniform._
import scalafy.util._
import scalafy.util.casing._
import scalafy.util.parser._
import scalafy.util.parser.TextParser._

/** XML parser */
object XmlParser {
  private val errorPrefix = "XML parse error :: "

  private val entityRefRegex = "&([^&;]*);".r

  private val escapedTag = "__escaped__"
  private val escapedTagValueAttr = "__value__"

  private lazy val reverseXhtmlEntityMap: Map[Char, String] = {
    var reverse = Map[Char,String]()
    for ((k,v) <- XhtmlEntities.entMap) reverse += (v -> k) 
    reverse
  }

  /** Parser for fromXml package function
    *
    * @return either String error msg or data of given type
    */
  def fromXml[A : Manifest](
    xml: Iterable[Char], settings: XmlSettings
  ): Either[String, A] = {
    val source = Source.fromIterable(xml)
    val xmlReader = new XMLEventReader(source)

    if (!xmlReader.hasNext) return Left("empty input")

    // Skip over root tag
    xmlReader.next match {
      case EvElemStart(_, settings.rootTag, attrs, _) => 
        if (isNil(attrs)) {
          xmlReader.stop()
          if (isNullAllowed(manifest[A].erasure))
            return Right(null).asInstanceOf[Either[String,A]]
          else return Left("expecting " + manifest[A] + " but recieved null")
        }

      case _ => 
        xmlReader.stop()
        return Left("invalid input: expecting <" + settings.rootTag + ">")
    }

    val result = TextParser.fromText(
        createParserIterator(xmlReader.buffered, settings), settings) match {
      case Left(l) =>
        // TODO add wrapping iterator to count chars read
        Left(errorPrefix + l + " (chars read: " + "unknown" + ")")
      case Right(r) => Right(r)
    }

    xmlReader.stop()
    result
  }

  /** fromXml returning Option */
  def fromXmlOption[A : Manifest](
    xml: Iterable[Char], settings: XmlSettings
  ): Option[A] = fromXml(xml, settings) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }

  /** Parser for toXml package function */
  def toXml(
    value: Any, 
    settings: XmlSettings 
  ): String = {
    if (value == null) "<" + settings.rootTag + " xsi:nil=\"true\"/>"
    else {
      "<" + settings.rootTag + ">" +
        TextParser.toText(value, createParserConverter(settings), settings) +
      "</" + settings.rootTag + ">"
    }
  }


  // Helpers

  protected[xml] def createParserIterator[A : Manifest](
    iter: BufferedIterator[XMLEvent], settings: XmlSettings
  ): Iterator[ParseEvent] = new Iterator[ParseEvent] { 

    private var prevEvent: XMLEvent = _ 
    private var addFieldName = false 
    private var fieldName: String = _ 
    private var addFieldSep = false
    private var addNil = false
 
    def hasNext = 
      (addFieldName || addFieldSep || prevEvent != null || iter.hasNext) 

    def next(): ParseEvent = {
      if (addFieldName) {
        addFieldName = false
        addFieldSep = true
        StringValue(fieldName)

      } else if (addFieldSep) {
        addFieldSep = false
        ObjectNameValueSeparator  // simulate separator between name/value
      
      } else if (addNil) {
        addNil = false
        NullValue("null")

      } else if (!iter.hasNext && prevEvent != null) {
        ObjectEnd     // final close
      
      } else {
        val nextEvent = iter.next 

        val result = nextEvent match {

          case EvElemStart(_, label, attrs, _) =>
            // Handle possible tag label encoding
            val tag = 
              if (label == escapedTag) getEscapedTag(attrs.copy(attrs)) match {
                case Right(t) => t
                case Left(l) => return ParseError(l) 
              } else label 

            if (tag == settings.arrayItemTag) {
              if (isNil(attrs.copy(attrs))) addNil = true 

              if (isPrevArrayItem()) Whitespace // same array 
              else ArrayStart
            } else {
              if (isNil(attrs.copy(attrs))) {
                addFieldSep = true  // add simulated field separator
                addNil = true       // field is null
              }

              if (isPrevField()) {
                addFieldSep = true  // add simulated field separator
                StringValue(tag)    // object field name
              } else {
                addFieldName = true // queue up the adding of field name
                fieldName = tag
                ObjectStart         // trigger start of new object
              }
            }

          case EvElemEnd(_, tag) =>
            if (tag == settings.arrayItemTag) {
              // If next is <item> then continuation of array
              if (isNextArrayItem()) Whitespace
              else ArrayEnd
            } else {
              if (isNextField()) Whitespace // closing tag for field
              else ObjectEnd
            }

          case e: EvText =>
            if (iter.hasNext && iter.head.isInstanceOf[EvElemStart]) {
              Whitespace // this is just whitespace between elements
            } else readText(e) match {
              case Right(r) => chooseBestEvent(r)
              case Left(l) => ParseError(l)
            }
          
          case e: EvEntityRef => 
            readText(e) match {
              case Right(r) => chooseBestEvent(r)
              case Left(l) => ParseError(l)
            }

         
          case EvComment(_) => 
            Whitespace 

          case EvProcInstr(_,_) => 
            Whitespace
        }
      
        prevEvent = nextEvent
        result
      }
    }

    private def chooseBestEvent(input: String): ParseEvent = {
      val text = decodeXml(input) match {
        case Right(r) => r
        case Left(l) => return ParseError("could not decode: " + l)
      }

      if (manifest[A] == manifest[String] || 
          manifest[A] == manifest[Symbol] || 
          manifest[A] == manifest[Char]) {
        StringValue(text)
      } else if (text.length > 0 && (text(0) == '-' || text(0).isDigit)) {
        NumberValue(text)
      } else if (text.equalsIgnoreCase("true") || 
          text.equalsIgnoreCase("false")) {
        BooleanValue(text.toLowerCase)
      } else StringValue(text)
    }

    /** Recursively reads the text and entity ref data */
    private def readText(e: XMLEvent): Either[String,String] = {
      if (e.isInstanceOf[EvText]) {
        if (!isNextText()) Right(e.asInstanceOf[EvText].text)
        else readText(iter.next) match {
          case Right(r) => Right(e.asInstanceOf[EvText].text + r)
          case Left(l) => return Left(l)
        }
      }
      else if (e.isInstanceOf[EvEntityRef]) {
        val entity = e.asInstanceOf[EvEntityRef].entity
        XhtmlEntities.entMap.get(entity) match {
          case Some(v) => 
            if (!isNextText()) Right(v.toString)
            else readText(iter.next) match {
              case Right(r) => Right(v + r)
              case Left(l) => return Left(l)
            }
          case None => return Left("unknown entityref: " + entity)
        }
      }
      else Left("expecting text, but text not found")
    }

    private def isNextText(): Boolean =
      (iter.hasNext && (iter.head.isInstanceOf[EvText] || 
        iter.head.isInstanceOf[EvEntityRef]))

    private def isPrevArrayItem(): Boolean =
      (prevEvent != null && prevEvent.isInstanceOf[EvElemEnd] &&
         prevEvent.asInstanceOf[EvElemEnd].label == settings.arrayItemTag)

    private def isNextArrayItem(): Boolean =
      (iter.hasNext && iter.head.isInstanceOf[EvElemStart] &&
         iter.head.asInstanceOf[EvElemStart].label == settings.arrayItemTag) 

    private def isPrevField(): Boolean =
      (prevEvent != null && prevEvent.isInstanceOf[EvElemEnd])

    private def isNextField(): Boolean =
      (iter.hasNext && iter.head.isInstanceOf[EvElemStart])
  }

  protected[xml] def createParserConverter(
    settings: XmlSettings
  ): (ParseEvent) => String = {

    var fieldNames = List[String]() 
    var nextIsFieldName = false
    var tagAttr: String = "" 

    def toString(value: Iterable[Char]) = {
      if (!fieldNames.isEmpty) {
        openTag(fieldNames.head, tagAttr) + 
          value.toString + 
        closeTag(fieldNames.head)
      } else value.toString
    }

    { // Partial function encluding closure around state vars
      case StringValue(value) => 
        if (nextIsFieldName) {
          if (!isValidTag(value.toString)) {
            tagAttr = encodeXml(value.toString) match {
              case Right(r) => " " + escapedTagValueAttr + "=\"" + r + "\""
              case Left(l) => throw new Error("Cannot encode XML: " + value)
            }
            fieldNames ::= escapedTag
          } else {
            fieldNames ::= value.toString
            tagAttr = ""
          }
          nextIsFieldName = false
          ""
        } else encodeXml(value.toString) match {
          case Right(r) => toString(r)
          case Left(l) => throw new Error("Cannot encode XML: " + value)
        }

      case NumberValue(value) => toString(value) 

      case BooleanValue(value) => toString(value) 

      case NullValue(value) => 
        "<" + fieldNames.head + " xsi:nil=\"true\"/>" 

      case ArrayStart =>
        if (fieldNames.isEmpty) { 
          fieldNames ::= settings.arrayItemTag
          tagAttr = ""
          ""
        } else {
          val tag = fieldNames.head
          val attr = tagAttr 
          fieldNames ::= settings.arrayItemTag
          tagAttr = ""
          openTag(tag, tagAttr)
        }

      case ArrayItemSeparator => ""

      case ArrayEnd =>
        if (!fieldNames.isEmpty) fieldNames = fieldNames.tail
        if (!fieldNames.isEmpty) closeTag(fieldNames.head)
        else ""

      case ObjectStart =>
        nextIsFieldName = true
        if (fieldNames.isEmpty) ""
        else openTag(fieldNames.head, tagAttr)

      case ObjectNameValueSeparator => ""

      case ObjectNameValuePairSeparator =>
        if (!fieldNames.isEmpty) {
          fieldNames = fieldNames.tail
          nextIsFieldName = true
        }
        ""

      case ObjectEnd =>
        if (!fieldNames.isEmpty) {
          val tag = fieldNames.head
          // Special case for array items at root (don't want to remove tag)
          if (tag != settings.arrayItemTag) fieldNames = fieldNames.tail
          closeTag(tag)
        } else ""

      case e => throw new Error("Unknown parse event: " + e) 
    }
  }

  private[xml] def isNil(attrs: MetaData): Boolean = {
    if (attrs.key == "nil") attrs.value.toString.equalsIgnoreCase("true")
    else if (attrs.hasNext) isNil(attrs.next)
    else false
  }

  /** Gets tag that was escaped as an attribute value.
    *
    * This support was added so that you can serialize numbers, etc as tags.
    */
  private[xml] def getEscapedTag(attrs: MetaData): Either[String,String] = {
    if (attrs.key == escapedTagValueAttr) decodeXml(attrs.value.toString)
    else if (attrs.hasNext) getEscapedTag(attrs.next)
    else Left("invalid escaped tag: missing " + escapedTagValueAttr)
  }

  private [xml] def isValidTag(tag: String): Boolean = {
    if (tag.length > 0) {
      if (tag(0).isDigit) return false
      if (tag(0) == '-') return false
    }

    for (x <- tag) {
      if (x == '"') return false
      
      reverseXhtmlEntityMap.get(x) match {
        case Some(ref) => return false 
        case None => {}
      }
    }

    true
  }

  /** Encodes XML with proper entity references */
  private[xml] def encodeXml(s: String): Either[String,String] = {
    val text = decodeXml(s) match {
      case Right(r) => r
      case Left(l) => return Left(l)
    }

    // Decode any previous so we don't double encode
    val buf = new StringBuilder
    for (x <- text) {
      reverseXhtmlEntityMap.get(x) match {
        case Some(ref) => buf.append("&" + ref + ";")
        case None => buf.append(x)
      }
    }

    Right(buf.toString)
  }

  /** Decodes XML that may have entity references */
  private[xml] def decodeXml(text: String): Either[String, String] = {
    val refs = entityRefRegex.findAllIn(text).map { ref =>
      XhtmlEntities.entMap.get(ref.substring(1,ref.length - 1)) match {
        case Some(v) => v
        case None => return Left("Unknown entity ref: " + ref) 
      }
    }
    if (refs.isEmpty) return Right(text)

    val nonRefs = entityRefRegex.split(text)

    val joined = nonRefs.zipAll(refs.toSeq, "", "").flatMap { 
      case (x,y) => List(x,y) 
    }
    Right(joined.mkString)
  }

  private [xml] def openTag(tag: String, attr: String) = "<" + tag + attr + ">"

  private [xml] def closeTag(tag: String) = "</" + tag + ">"
}
