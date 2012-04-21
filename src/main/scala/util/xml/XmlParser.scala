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

import java.io.StringWriter
import java.io.Writer
import java.util.Date
import java.util.TimeZone

import scala.io.Source
import scala.xml._
import scala.xml.parsing.XhtmlEntities
import scala.xml.pull._

import scalafy.collection.mutable.ChunkedIterator
import scalafy.collection.uniform._
import scalafy.types.meta._
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

  def fromXml[A : Manifest](
    xml: Iterable[Char], settings: XmlSettings
  ): Either[String, A] = {
    fromXml(Source.fromIterable(xml), settings)
  }

  /** Returns either String error msg or data of given type. */
  def fromXml[A : Manifest](
    source: Source, settings: XmlSettings
  ): Either[String, A] = {
    val xmlReader = new XMLEventReader(source)

    if (!xmlReader.hasNext) {
      xmlReader.stop()
      return Left("empty input")
    }

    val iter = xmlReader.buffered

    // Skip over root tag
    iter.next match {
      case EvElemStart(_, settings.rootTag, attrs, _) => 
        if (isNil(attrs)) {
          xmlReader.stop()
          if (isNullAllowed(manifest[A].erasure))
            return Right(null).asInstanceOf[Either[String,A]]
          else return Left("expecting " + manifest[A] + " but recieved null")
        }

        if (!iter.hasNext) {
          xmlReader.stop()
          return Left("missing closing tag")
        }

        if (iter.head.isInstanceOf[EvElemEnd] && 
            iter.head.asInstanceOf[EvElemEnd].label == settings.rootTag) {
          xmlReader.stop()
          if (manifest[A].erasure == classOf[Option[_]])
            return Right(None.asInstanceOf[A])
          else
            return Left("unexpected end of input while parsing: " + manifest[A])
        }

      case _ => 
        xmlReader.stop()
        return Left("invalid input: expecting <" + settings.rootTag + ">")
    }

    val result = TextParser.fromText(
        createParserIterator(iter, xmlReader, settings), settings) match {
      case Left(l) =>
        // TODO add wrapping iterator to count chars read
        Left(errorPrefix + l + " (chars read: " + "unknown" + ")")
      case Right(r) => Right(r)
    }

    result
  }

  def toXml(
    value: Any, 
    settings: XmlSettings 
  ): String = {
    val writer = new StringWriter()
    toXml(writer, value, settings)
    writer.toString
  }
  
  def toXml(
    writer: Writer,
    value: Any, 
    settings: XmlSettings 
  ): Unit = {
    if (value == null || value == None) 
      writer.write("<" + settings.rootTag + " xsi:nil=\"true\"/>")
    else {
      writer.write("<" + settings.rootTag + ">")
      TextParser.toText(
        writer, value, createParserConverter(settings), settings)
      writer.write("</" + settings.rootTag + ">")
    }
  }


  // Helpers

  protected[xml] def createParserIterator[A : Manifest](
    iter: BufferedIterator[XMLEvent], 
    xmlReader: XMLEventReader, 
    settings: XmlSettings
  ): Iterator[ParseEvent] = new Iterator[ParseEvent] { 

    private var prevEvent: XMLEvent = _ 
    private var addFieldName = false 
    private var fieldName: String = _ 
    private var addFieldSep = false
    private var addNil = false
    private var addEmpty = false  // Used for Option processing
 
    def hasNext = 
      (addFieldName || addFieldSep || addEmpty || prevEvent != null || 
        iter.hasNext) 

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

      } else if (addEmpty) {
        addEmpty = false
        EmptyValue

      } else if (!iter.hasNext && prevEvent != null) {
        if (prevEvent.isInstanceOf[EvElemEnd] && 
            prevEvent.asInstanceOf[EvElemEnd].label == settings.rootTag) {
          prevEvent = null
          Whitespace
        } else {
          prevEvent = null
          ObjectEnd     // final close
        }
      
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

            if (tag == settings.rootTag) {
              Whitespace
            } else if (tag == settings.arrayItemTag) {
              if (isNil(attrs.copy(attrs))) addNil = true
              if (!addNil && isNextEndTag()) addEmpty = true

              if (isPrevArrayItem() || isPrevText()) Whitespace // same array 
              else ArrayStart
            } else {
              if (isNil(attrs.copy(attrs))) {
                addFieldSep = true  // add simulated field separator
                addNil = true       // field is null
              }
              if (!addNil && isNextEndTag()) addEmpty = true

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
            if (tag == settings.rootTag) {
              Whitespace
            } else if (tag == settings.arrayItemTag) {
              // If next is <item> then continuation of array
              if (isNextArrayItem() || isNextText()) Whitespace
              else ArrayEnd
            } else {
              if (isNextField() || isNextText()) Whitespace // closing field tag
              else ObjectEnd
            }

          case e: EvText =>
            readText(e) match {
              case Right(r) =>
                if (iter.hasNext && iter.head.isInstanceOf[EvElemStart])
                  Whitespace // this is just whitespace between elements
                else chooseBestEvent(r)
              case Left(l) => ParseError(l)
            }
          
          case e: EvEntityRef => 
            readText(e) match {
              case Right(r) => 
                if (iter.hasNext && iter.head.isInstanceOf[EvElemStart])
                  Whitespace // this is just whitespace between elements
                else chooseBestEvent(r)
              case Left(l) => ParseError(l)
            }

         
          case EvComment(_) => 
            Whitespace 

          case EvProcInstr(_,_) => 
            Whitespace
        }
      
        prevEvent = nextEvent
        if (!iter.hasNext) xmlReader.stop() // cleanup
        result
      }
    }

    private def chooseBestEvent(input: String): ParseEvent = {
      val text = decodeXml(input) match {
        case Right(r) => r
        case Left(l) => return ParseError("could not decode: " + l)
      }

      // Let TextParser do the work in it's parseString method..
      StringValue(text)
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

    private def isPrevText(): Boolean =
      (prevEvent != null && (prevEvent.isInstanceOf[EvText] || 
        prevEvent.isInstanceOf[EvEntityRef]))

    private def isNextEndTag(): Boolean =
      (iter.hasNext && iter.head.isInstanceOf[EvElemEnd])

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
      
      case EmptyValue =>
        "<" + fieldNames.head + " xsi:nil=\"true\"/>" 

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
        // Ending ObjectNameValuePairSeparator
        if (!fieldNames.isEmpty) {
          fieldNames = fieldNames.tail
          nextIsFieldName = true
        }
        // End of Object
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
