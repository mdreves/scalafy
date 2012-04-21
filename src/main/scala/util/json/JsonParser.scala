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
package scalafy.util.json

import scala.collection.immutable.WrappedString

import scalafy.collection.mutable.ChunkedIterator
import scalafy.collection.mutable.DefaultChunkedIterator
import scalafy.collection.mutable.StringChunkedIterator
import scalafy.types.meta._
import scalafy.types.uniform._
import scalafy.util._
import scalafy.util.casing._
import scalafy.util.parser._
import scalafy.util.parser.TextParser._

/** JSON parser */
object JsonParser {
  private val errorPrefix = "JSON parse error :: "

  /** Parser for fromJson package function
    *
    * @return either String error msg or data of given type
    */
  def fromJson[A : Manifest](
    json: Iterable[Char], settings: JsonSettings
  ): Either[String, A] = {

    val chunkedIter = 
      if (json.isInstanceOf[String])
        StringChunkedIterator(json.asInstanceOf[String], '\\')
      else if (json.isInstanceOf[WrappedString]) 
        StringChunkedIterator(json.asInstanceOf[WrappedString].self, '\\')
      else DefaultChunkedIterator(json.iterator.buffered, '\\')

    val parserIter = createParserIterator(chunkedIter) 

    TextParser.fromText(parserIter, settings) match {
      case Left(l) =>
        Left(errorPrefix + l + " (chars read: " + (chunkedIter.readCount) + ")")
      case Right(r) => Right(r)
    }
  }

  /** fromJson returning Option */
  def fromJsonOption[A : Manifest](
    json: Iterable[Char], settings: JsonSettings
  ): Option[A] = fromJson(json, settings) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }

  /** Parser for toJson package function */
  def toJson(
    value: Any, 
    settings: JsonSettings 
  ): String = {
    TextParser.toText(value, parserConverter, settings)  
  }


  // Helpers

  protected[json] def createParserIterator[A : Manifest](
    iter: ChunkedIterator[String, Char]
  ): Iterator[ParseEvent] = new Iterator[ParseEvent] {

    // iterable is an iterable version of our own chunked iterator. This allows
    // us to pass our one single iterator for the input stream of chars in
    // all places where an iterable is requested
    val iterable = iter.toIterable

    var parsingString = false
    var parsingArray = false 

    def hasNext = iter.hasNextChunk

    def next(): ParseEvent = iter.head match {

      // cleanup from string parsing
      case c if (parsingString) =>
        if (c != '"') 
          return ParseError("invalid string: missing closing '\"'")

        iter.next()
        iter.nextStop = 0
        parsingString = false
        Whitespace
         
      // end of obj
      case '}' => 
        iter.next()
        ObjectEnd 

      // end of list
      case ']' => 
        iter.next()
        parsingArray = false
        ArrayEnd 

      case ':' => iter.next()
        ObjectNameValueSeparator
      
      case ',' => 
        iter.next()
        if (parsingArray) ArrayItemSeparator
        else ObjectNameValuePairSeparator

      // whitespace around structure characters or separators (ignore)
      case c if (
        c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ','
      ) =>
        iter.next()
        Whitespace

      // String value
      case '"' =>
        iter.next()
        iter.nextStop = '"'
        parsingString = true
        StringValue(iterable)

      // Number value
      case c if (c == '-' || c.isDigit) => NumberValue(iterable)

      // Boolean value
      case c if (c == 't' || c == 'f') => BooleanValue(iterable)

      // null value
      case 'n' => NullValue(iterable) 

      // start of object
      case '{' => 
        iter.next()
        ObjectStart

      // start of list
      case '[' => 
        iter.next()
        parsingArray = true
        ArrayStart

      case x => ParseError("unexpected character: " + x) 
    }
  }

  protected[json] def parserConverter(e: ParseEvent): String = e match {
    case StringValue(value) => "\"" + encodeStringData(value) + "\""
    case NumberValue(value) => value.toString
    case BooleanValue(value) => value.toString
    case NullValue(value) => value.toString
    case ArrayStart => "["
    case ArrayItemSeparator => ","
    case ArrayEnd => "]"
    case ObjectStart => "{"
    case ObjectNameValueSeparator => ":" 
    case ObjectNameValuePairSeparator => ","
    case ObjectEnd => "}"
    case e => throw new Error("Unknown parse event: " + e) 
  }

  protected[json] def encodeStringData(s: Iterable[Char]): String = {
    val buf = new StringBuilder
    for (c <- s) {
      if (c == '\\') buf.append("\\\\")
      else if (c == '/') buf.append("\\/")
      else if (c == '"') buf.append("\\\"")
      else buf.append(c)
    }
    buf.toString 
  }
}
