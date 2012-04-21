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
package scalafy.util.parser

import java.io.Writer
import java.text.DateFormat
import java.util.Date
import java.util.TimeZone

import scalafy.collection.mutable.ChunkedIterator
import scalafy.collection.uniform._
import scalafy.types.meta._
import scalafy.util._
import scalafy.util.casing._

sealed trait ParseEvent
case class StringValue(value: Iterable[Char]) extends ParseEvent
case class NumberValue(value: Iterable[Char]) extends ParseEvent
case class BooleanValue(value: Iterable[Char]) extends ParseEvent
case class NullValue(value: Iterable[Char]) extends ParseEvent
case object EmptyValue extends ParseEvent
case object ArrayStart extends ParseEvent
case object ArrayItemSeparator extends ParseEvent
case object ArrayEnd extends ParseEvent
case object ObjectStart extends ParseEvent
case object ObjectNameValueSeparator extends ParseEvent // between name/value
case object ObjectNameValuePairSeparator extends ParseEvent
case object ObjectEnd extends ParseEvent
case object Whitespace extends ParseEvent
case class ParseError(msg: Iterable[Char]) extends ParseEvent

/** Generic text based pull parser
  *
  * Note: The terminology used is based on JSON (e.g. Array instead of Seq)
  */
object TextParser {

  /** Converts text data into the requested type. 
    *
    * This method handles data at the level of primitives values, lists, and
    * map/object name/value fields. The passed in iterator must return the
    * appropriate event type when requested via next. This parser will validate
    * that the expected value conforms to the overall structure, but it will
    * not perform any boundary parsing (e.g. what constitutes the start/end of
    * an object/list or primitive value is up to the iterator). The parser will
    * however convert between primitives as needed to conform to the type
    * requested. This means that although a StringValue, NumberValue, etc
    * primitive parse event may be returned, the parser will convert as
    * necessary to meet its requested requirements. 
    *
    * @param iter iterator used to pull parse events
    * @return either String error msg or data of given type
    */
  def fromText[A : Manifest](
    iter: Iterator[ParseEvent], settings: ParserSettings
  ): Either[String, A] = {

    // Special handling for Streams and Itreator/Iterable at start
    if (manifest[A].erasure == classOf[Stream[_]] ||
        manifest[A].erasure == classOf[Iterable[_]] ||
        manifest[A].erasure == classOf[Iterator[_]]) {
      while (iter.hasNext) iter.next match {
        case ParseError(msg) => return Left(msg.toString)
        case Whitespace => {}  // Ignore
        case ArrayStart => 
          if (manifest[A].erasure == classOf[Stream[_]]) {
            return new StreamData(iter, manifest[A], settings).getObj()
          } else {
            return new IterableData(iter, manifest[A], settings).getObj()
          }
        case _ => return Left("unexpected data while parsing: " + manifest[A])
      }
    }

    // If uniform primitives requested, then convert to normal primitives
    // to save us from covering all the different primitive type forms
    val m = 
      if (classOf[UniformData[_]].isAssignableFrom(manifest[A].erasure) &&
          !(manifest[A].erasure == classOf[UniformMap[_]] || 
            manifest[A].erasure == classOf[UniformList[_]])) {
        toPrimitiveManifestFromUniform(manifest[A])
      } else manifest[A]

    parseNext(iter, settings)(m) match {
      case Right(r) => r match {
        case Some(v) =>
          // Unwrap objects stored with their manifests 
          if (v.isInstanceOf[StructuredData[_]]) {
             v.asInstanceOf[StructuredData[_]].getObj() match {
               case Right(v) => Right(v.asInstanceOf[A])
               case Left(l) => Left(l)
             } 
          } 
          // m doesn't match if a UniformPrimitives was requested, now do the
          // conversion to the uniform type
          else if (m != manifest[A]) v match {
            case x: String => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Symbol => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Int => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Short => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Long => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Float => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Double => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Boolean => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Char => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Byte => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: BigInt => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: BigDecimal => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: Date => Right(toUniformPrimitive(x).asInstanceOf[A])
            case x: TimeZone => Right(toUniformPrimitive(x).asInstanceOf[A])
            case _ => Left("primitive expected: " + v)
          } else {
            Right(v.asInstanceOf[A])
          }
        case None => Left("empty input")
      }
      case Left(l) => Left(l)
    }
  }

  /** Converts given type into string form.
    *
    * This method handles conversion of primitives values, lists, and
    * map/object name/value fields to strings. The the basic string conversion
    * will be done by the method, but the passed in converter is responsible
    * for doing any language specific string encodings for data based on
    * the parse events it is given. It should also return the appropriate
    * string encodings for the ArrayStart/ArrayEnd/etc events.
    *
    * @param writer writer to write output to
    * @param value value to encode
    * @param converter converter between parse events and strings 
    * @return encoded string 
    */
  def toText(
    writer: Writer, 
    value: Any, 
    converter: (ParseEvent) => String, 
    settings: ParserSettings
  ): Unit = {
    val prettyPrint = settings.prettyPrintSettings.enabled
    val indent = settings.prettyPrintSettings.indent

    val arraySepLen = converter(ArrayItemSeparator).length
    val objSepLen = converter(ObjectNameValuePairSeparator).length
    
    // Prints output to writer
    def printToWriter(writer: Writer, v: Any, offset: Int) {
      // Null
      if (v == null) {
        append(writer, converter(NullValue("null")), 0)
        return
      }

      // Option handling
      val value = 
        if (v.isInstanceOf[Option[_]]) {
          if (v == None) {
            append(writer, converter(EmptyValue), 0)
            return
          }
          v.asInstanceOf[Option[_]].get
        } else v

      // String
      if (value.isInstanceOf[String] || value.isInstanceOf[Char]) {
        append(writer, converter(StringValue(value.toString)), 0)
      }
      else if (value.isInstanceOf[Symbol]) {
        append(writer, 
          converter(StringValue(value.asInstanceOf[Symbol].name)), 0)
      }
      
      // Number 
      else if (value.isInstanceOf[Int] || value.isInstanceOf[Short] ||
          value.isInstanceOf[Long] || value.isInstanceOf[Float] ||
          value.isInstanceOf[Double] || value.isInstanceOf[Byte] ||
          value.isInstanceOf[BigInt] || value.isInstanceOf[BigDecimal]) {
        append(writer, converter(NumberValue(value.toString)), 0)
      } 
     
      // Boolean
      else if (value.isInstanceOf[Boolean]) {
        append(writer, converter(BooleanValue(value.toString)), 0)
      }

      // Date 
      else if (value.isInstanceOf[Date]) {
        append(writer, converter(StringValue(
          settings.dateFormatter.format(value.asInstanceOf[Date]))), 0)
      }

      // TimeZone 
      else if (value.isInstanceOf[TimeZone]) {
        append(writer, 
          converter(StringValue(value.asInstanceOf[TimeZone].getID)), 0)
      }

      // Map (check Map before Seq so Iterable get matched)
      else if (isMapType(value.getClass)) {
        append(writer, converter(ObjectStart), 0)
        if (prettyPrint) append(writer, "\n", 0)

        val iter = value.asInstanceOf[Iterable[_]].iterator

        for ((k,v) <- iter) {
          val convertedName = 
            convertName(k) + converter(ObjectNameValueSeparator)

          append(writer, convertedName, offset + indent)
          if (prettyPrint && convertedName != "") append(writer, " ", 0)

          printToWriter(writer, v, offset + indent)

          if (iter.hasNext) {
            append(writer, converter(ObjectNameValuePairSeparator), 0)
            if (prettyPrint) append(writer, "\n", 0)
          }
        }

        if (prettyPrint) append(writer, "\n", 0)
        append(writer, converter(ObjectEnd), offset)
      }

    
      // Seq
      else if (classOf[Iterable[_]].isAssignableFrom(value.getClass) ||
          classOf[Iterator[_]].isAssignableFrom(value.getClass)) {
        append(writer, converter(ArrayStart), 0)

        val iter = 
          if (classOf[Iterable[_]].isAssignableFrom(value.getClass))
            value.asInstanceOf[Iterable[_]].iterator
          else value.asInstanceOf[Iterator[_]]

        for (x <- iter) { 
          printToWriter(writer, x, 0)

          if (iter.hasNext) {
            append(writer, converter(ArrayItemSeparator), 0)
            if (prettyPrint) append(writer, " ", 0)
          }
        }

        append(writer, converter(ArrayEnd), 0)
      } 

      // Tuple 
      else if (isTupleType(value.getClass)) {
        append(writer, converter(ArrayStart), 0)

        val iter = value.asInstanceOf[Product].productIterator

        for (x <- iter) { 
          printToWriter(writer, x, 0)

          if (iter.hasNext) {
            append(writer, converter(ArrayItemSeparator), 0)
            if (prettyPrint) append(writer, " ", 0)
          }
        }

        append(writer, converter(ArrayEnd), 0)
      } 

      // Singleton
      else if (v.getClass.getName.endsWith("$")) {
        append(writer, converter(StringValue(v.toString)), 0)
      }

      // Enumeration 
      else if (classOf[Enumeration$Value].isAssignableFrom(v.getClass)) {
        append(writer, converter(StringValue(v.toString)), 0)
      }

      // Object (use reflection)
      else {
        append(writer, converter(ObjectStart), 0)
        if (prettyPrint) append(writer, "\n", 0)

        // Append any Opaque data
        var opaque: Map[Symbol,Any] = 
          if (value.isInstanceOf[AnyRef]) 
            OpaqueData.get(value.asInstanceOf[AnyRef]) match {
            case Some(v) => v
            case None => null 
          } else null

        var addSeparator = false

        value.getClass.getDeclaredFields.map { field =>
          // Find method with same name
          try {
            val fieldValue = value.getClass.getMethod(
                field.getName).invoke(value)

            if (addSeparator) {
              append(writer, converter(ObjectNameValuePairSeparator), 0)
              if (prettyPrint) append(writer, "\n", 0)
            }
      
            val convertedName = convertName(field.getName) +
              converter(ObjectNameValueSeparator)

            append(writer, convertedName, offset + indent)
            if (prettyPrint && convertedName != "") append(writer, " ", 0)

            printToWriter(writer, fieldValue, offset + indent)

            addSeparator = true

            if (opaque != null) opaque = opaque - Symbol(field.getName) 
          } catch {
            case e: NoSuchMethodException => {} 
          }
        }

        // Add opaque data
        if (settings.opaqueDataSettings.enabled && opaque != null) {
          for ((k, v) <- opaque) {
            if (addSeparator) {
              append(writer, converter(ObjectNameValuePairSeparator), 0)
              if (prettyPrint) append(writer, "\n", 0)
            }

            val convertedName = 
              convertName(k) + converter(ObjectNameValueSeparator)

            append(writer, convertedName, offset + indent)
            if (prettyPrint && convertedName != "") append(writer, " ", 0)

            printToWriter(writer, v, offset + indent)

            addSeparator = true
          }
        }

        if (prettyPrint) append(writer, "\n", 0)
        append(writer, converter(ObjectEnd), offset)
      }
    }

    def convertName(name: Any): String = {
      val normalized = 
        if (name.isInstanceOf[Symbol]) name.asInstanceOf[Symbol].name
        else name.toString

      val withCasing = 
        if (settings.fieldCasing != IgnoreCasing)
          Casing.toCase(normalized, settings.fieldCasing) 
        else normalized 

      converter(StringValue(withCasing))
    }

    // Adds spaces
    def append(writer: Writer, s: String, offset: Int) =  {
      if (prettyPrint) for (i <- 0 until offset) writer.write(" ")
      writer.write(s)
    }

    printToWriter(writer, value, 0)
  } 


  // Helpers

  /** parseNext for use with array/primitive parsing */
  protected[parser] def parseNext[A : Manifest](
    iter: Iterator[ParseEvent], settings: ParserSettings
  ): Either[String, Option[A]] = {
    parseNext(iter, null, null, false, settings)
  }

  /** Parses next item 
   *
   * This method can be used to parse the next value of a array, a primitive
   * type or an object. If an object is being parsed, all the objects
   * fields and values will be read (including any embedded objects/lists)
   *
   * Two special parameters are provided for when data is being parsed as an
   * object. The first is the wrapper object that the objects fields are to be
   * stored in (ObjectData). This is passed so that we can question it about
   * what types it is expecting for different fields and parse appropriately.
   * The second special parameter is a partial function that will be called for
   * each name/value that is parsed. The caller must keep track of these
   * names/values itself in order to construct the object later (they will not
   * be returned from this method).  The partial function should return String
   * if the caller rejects the name/value given or None if the name/value was
   * accepted. Both the name and value are passed as Any types giving the
   * object parsing code the flexibility to use any name/value types.
   *
   * These two special methods are not needed for Array based parsing or
   * primitive parsing.
   *
   * The allowEmpty flag is used for Option parsing only.
   *
   * @return Right(value), Right(None) - no more data, or Left(error message)
   */
  protected[parser] def parseNext[A : Manifest](
    iter: Iterator[ParseEvent],
    obj: ObjectData[_,_], // container parsed name/values will be stored in
    pf: PartialFunction[Tuple2[Any,Any],Option[String]], // for each name/value
    allowEmpty: Boolean,
    settings: ParserSettings
  ): Either[String, Option[A]] = {

    // Special parsing for Options
    if (manifest[A].erasure == classOf[Option[_]]) {
      return parseOption(iter, settings)(manifest[A])
    }

    var itemCount = 1        // cur num items we need to parse 
    var name: Any = null
    // Breaks out after required item is parsed or error found
    while (iter.hasNext && itemCount > 0) iter.next match {
      case ParseError(msg) => return Left(msg.toString) 
      
      case Whitespace => {}  // Ignore
      case ObjectNameValuePairSeparator => {}  // Ignore
      case ArrayItemSeparator => {}  // Ignore

      // end of obj
      case ObjectEnd if (obj != null) => itemCount -= 1

      // end of array 
      case ArrayEnd if (obj == null) => itemCount -= 1

      // obj field name
      case StringValue(fieldIter) if (obj != null && name == null) =>
        // Convert to proper field casing
        settings.fieldCasing match {
          case fieldCasing: Casing if (
            fieldCasing != IgnoreCasing &&  
              (obj.getNameManifest == manifest[String] || 
               obj.getNameManifest == manifest[Symbol])
          ) => {
            val parsedValue = Casing.toCase(
                fieldIter, fieldCasing, null, Nil)
            
            // Need to do our own conversion since we parsed ourselves
            if (obj.getNameManifest == manifest[Symbol]) 
              name = Symbol(parsedValue)
            else name = parsedValue
          }

          case _ => parseString(
              fieldIter.iterator.buffered,settings)(obj.getNameManifest) match {
            case Right(value) => name = value
            case Left(l) => return Left("invalid field name") 
          }
        }
      
        // Check for field separator (skipping any whitespace before)
        var sep = if (!iter.hasNext) Whitespace else iter.next 
        while (iter.hasNext && sep == Whitespace) { sep = iter.next }
       
        if (!iter.hasNext || sep != ObjectNameValueSeparator)
          return Left("invalid field: missing field separator after " + name) 


      // String value
      case StringValue(iterable) =>
        val tmp = if (obj == null) manifest[A] else obj.getItemType(name)
        if (tmp == null) {
          return Left("unknown type for field " + name + 
            "': typeHintSettings are required")
        }
        val isOption = (tmp.erasure == classOf[Option[_]])
        val itemType = if (isOption) tmp.typeArguments(0) else tmp
        
        parseString(iterable.iterator.buffered, settings)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(v) => {
            val value = if (isOption) Some(v) else v

            if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

            pf((name, value)) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }

      // None value 
      case EmptyValue =>
        if (allowEmpty) return Right(Some(None.asInstanceOf[A]))
        else if (obj == null) return Left("list is missing value")
        else {
          if (name != null) {
            val itemType = obj.getItemType(name)
            if (itemType == null) {
              return Left("unknown type for field " + name + 
                "': typeHintSettings are required")
            }
            if (itemType.erasure == classOf[Option[_]]) {
              pf((name, None)) match {
                case Some(e) => return Left(e)
                case None => name = null // success, reset
              }
            }
            else return Left("missing value after: " + name)
          } else return Left("object is missing value")
        }
    
      // if object, name must be set
      case _ if (obj != null && name == null) =>
        return Left("invalid field: missing name")

      // Number value
      case NumberValue(iterable) =>
        val tmp = if (obj == null) manifest[A] else obj.getItemType(name)
        if (tmp == null) {
          return Left("unknown type for field " + name + 
            "': typeHintSettings are required")
        }
        val isOption = (tmp.erasure == classOf[Option[_]])
        val itemType = if (isOption) tmp.typeArguments(0) else tmp
 
        parseNumber(iterable.iterator.buffered, settings)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(v) => {
            val value = if (isOption) Some(v) else v

            if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

            pf((name, value)) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }

      // Boolean value
      case BooleanValue(iterable) =>
        val tmp = if (obj == null) manifest[A] else obj.getItemType(name)
        if (tmp == null) {
          return Left("unknown type for field " + name + 
            "': typeHintSettings are required")
        }
        val isOption = (tmp.erasure == classOf[Option[_]])
        val itemType = if (isOption) tmp.typeArguments(0) else tmp
         
        parseBoolean(iterable.iterator.buffered)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(v) => {
            val value = if (isOption) Some(v) else v

            if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

            pf(name, value) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }

      // Null value
      case NullValue(iterable) =>
        val tmp = if (obj == null) manifest[A] else obj.getItemType(name)
        if (tmp == null) {
          return Left("unknown type for field " + name + 
            "': typeHintSettings are required")
        }
        val isOption = (tmp.erasure == classOf[Option[_]])
        // Use Any manifest if Option so we can parse null to None for all types
        val itemType = if (isOption || allowEmpty) manifest[Any] else tmp

        parseNull(iterable.iterator.buffered)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(v) => {
            val value = if (isOption) None else v
            
            if (obj == null) 
              return Right(Some(value).asInstanceOf[Option[A]]) // done

            pf((name, value)) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }
 
      // start of object
      case ObjectStart => 
        val itemType = if (obj == null) manifest[A] else obj.getItemType(name) 
        if (itemType == null) {
          return Left("unknown type for field " + name + 
            "': typeHintSettings are required")
        }

        parseObject(iter, settings)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(value) => {
            if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

            pf((name, value)) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }

      // start of array
      case ArrayStart =>
        val itemType = if (obj == null) manifest[A] else obj.getItemType(name) 
        if (itemType == null) {
          return Left("unknown type for field " + name + 
            "': typeHintSettings are required")
        }

        parseList(iter, settings)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(value) => {
            if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

            pf((name, value)) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }

      case x => return Left("unexpected data: " + x) 
    }

    if (itemCount != 0 && 
        (isSeqType(manifest[A]) || isMapType(manifest[A]) ||
         manifest[A].erasure == classOf[UniformList[_]] ||  
         manifest[A].erasure == classOf[UniformMap[_]])) {
      return Left("missing end of object/list")
    }
 
    // An itemCount of 0 means we are closing an array or object so
    // we read the ArrayEnd or ObjectEnd instead of our item, that's ok
    // otherwise if we are allowing empty we should have gotten EmptyValue
    // or a value
    if (allowEmpty && itemCount != 0) {
      Left("unexpected end of input while parsing: " + manifest[A])
    }
    else Right(None)   // End of data
  }

  /** Parses string (can be parsed as any primitive) */
  protected[parser] def parseString[A : Manifest](
    iter: BufferedIterator[Char],
    settings: ParserSettings
  ): Either[String, A] = {

    // Helper to read string data
    def readString(): String = {
      var s = new StringBuilder
      while (iter.hasNext) s + iter.next 
      s.toString
    }

    val clazz = manifest[A].erasure

    // Put most likley checks first
    if (clazz == classOf[String] || clazz == classOf[UniformString]) {

      Right(readString.asInstanceOf[A])

    } else if (clazz == classOf[Symbol] || clazz == classOf[UniformSymbol]) { 

      Right(Symbol(readString).asInstanceOf[A])

    } else if (clazz == classOf[Char] || clazz == classOf[UniformChar]) { 

      val s = readString
      if (s.length == 1) Right(s(0).asInstanceOf[A])
      else Left("type mismatch: expecting Char not String")

    } else if (clazz == classOf[Date] || clazz == classOf[UniformDate]) { 

      try {
        val dateStr = readString.toUpperCase()
        Right(settings.dateFormatter.parse(dateStr).asInstanceOf[A])
      } catch {
        case e: Exception =>
          Left("invalid " + manifest[A] + ": " + e.getMessage())
      }

    } else if (clazz == classOf[TimeZone] || clazz == classOf[UniformTimeZone]){

        val timeZoneStr = readString
        val timeZone = TimeZone.getTimeZone(timeZoneStr.toUpperCase())
        if (timeZoneStr != "GMT" && timeZone.getID == "GMT") {
          Left("invalid timezone: " + timeZoneStr)
        } else {
          Right(timeZone.asInstanceOf[A])
        }

    } else if (clazz == classOf[Int] ||
        clazz == classOf[Short] ||
        clazz == classOf[Long] || 
        clazz == classOf[Float] ||
        clazz == classOf[Double] || 
        clazz == classOf[Byte] ||
        clazz == classOf[BigInt] ||
        clazz == classOf[BigDecimal] ||
        clazz == classOf[UniformInt] || 
        clazz == classOf[UniformShort] || 
        clazz == classOf[UniformLong] || 
        clazz == classOf[UniformFloat] || 
        clazz == classOf[UniformDouble] || 
        clazz == classOf[UniformByte] ||
        clazz == classOf[UniformBigInt] ||
        clazz == classOf[UniformBigDecimal]) {

      // If numeric type requested, pass off to parseNumber
      parseNumber(iter, settings)(manifest[A]) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left(l) // error
      }

    } else if (clazz == classOf[Boolean] || clazz == classOf[UniformBoolean]) { 

      // If boolean type requested, pass off to parseBoolean
      parseBoolean(iter)(manifest[A]) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left(l) // error
      }

    } else if (clazz == classOf[Any] ||
        clazz == classOf[Nothing] ||
        clazz == classOf[UniformData[_]] ||
        clazz == classOf[UniformPrimitive[_]]) {

      val str = readString

      // try date
      if (str.length == 20) { 
        try {
          val dateStr = str.toUpperCase()
          return Right(settings.dateFormatter.parse(dateStr).asInstanceOf[A])
        } catch {
          case e: Exception => {} // try something else
        }
      }

      // try Number
      if (str.length > 0 && (str(0) == '-' || str(0).isDigit)) {
        parseNumber(str.iterator.buffered, settings)(manifest[A]) match {
          case Right(r) => return Right(r.asInstanceOf[A])
          case _ => {} // try something else 
        }
      }

      // try Boolean
      if (str.equalsIgnoreCase("true")) {
        return Right(true.asInstanceOf[A])
      }
      if (str.equalsIgnoreCase("false")) {
        return Right(false.asInstanceOf[A])
      }

      // try null
      if (str.equalsIgnoreCase("null")) {
        return Right(null).asInstanceOf[Either[String,A]]
      }

      // just a string
      Right(str.asInstanceOf[A])

    }

    // Enumerations 
    else if (classOf[Enumeration$Value].isAssignableFrom(clazz)) {
      val name = scalafy.util.casing.Casing.toUpperCamelCase(readString)
      // Check if Int id
      try {
        val enumOpt = getEnumValueById(name.toInt, settings)
        if (!enumOpt.isEmpty) return Right(enumOpt.get.asInstanceOf[A])
      } catch {
        case _ => {}
      }

      getEnumValueByName(name, settings) match {
        case Some(v) => Right(v.asInstanceOf[A])
        case None => 
          Left("unknown enumeration value '" + name + 
            "': typeHintSettings may be required")
      }
    } 
   
    else {
      
      // Try singletons 
      try {
        val obj = 
          if (settings.classLoader != null) { 
            settings.classLoader.loadClass(
              manifest[A].erasure.getName).newInstance
          } else manifest[A].erasure.newInstance

        val name = scalafy.util.casing.Casing.toUpperCamelCase(readString)
        if (obj.getClass.getName.endsWith(name) ||
            obj.getClass.getName.endsWith(name + "$")) {
          return Right(obj.asInstanceOf[A])
        }
      } catch {
        case e => {}
      }

      Left("type mismatch: expecting " + manifest[A] + " not String")
    }
  }

  /** Parses Number */
  protected[parser] def parseNumber[A : Manifest](
    iter: BufferedIterator[Char],
    settings: ParserSettings
  ): Either[String, A] = {

    var s = new StringBuilder()

    // Skip over minus sign (NOTE: parser does not allow starting with +)
    if (iter.hasNext && iter.head == '-') s + iter.next

    if (!iter.hasNext) return Left(
      "unexpected end of input while parsing: " + manifest[A])

    val isDoubleChar = (c: Char) =>
      c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+'

    var next = iter.head
    var isDouble = false  // var used in convertToNumber
    while (iter.hasNext && (next.isDigit || isDoubleChar(next))) {
      if (isDoubleChar(next)) isDouble = true
      s + iter.next()
      if (iter.hasNext) next = iter.head
    }
   
    // Helper to convert string to number
    def convertToNumber(clazz: Class[_], s: String): Either[String, A] = try {
      if (clazz == classOf[Short]) 
        Right(s.toShort.asInstanceOf[A])
      else if (clazz == classOf[Int]) 
        Right(s.toInt.asInstanceOf[A])
      else if (clazz == classOf[Long]) 
        Right(s.toLong.asInstanceOf[A])
      else if (clazz == classOf[Float]) 
        Right(s.toFloat.asInstanceOf[A])
      else if (clazz == classOf[Double]) 
        Right(s.toDouble.asInstanceOf[A])
      else if (clazz == classOf[Byte]) 
        Right(s.toByte.asInstanceOf[A])
      else if (clazz == classOf[BigInt]) 
        Right(BigInt(s).asInstanceOf[A])
      else if (clazz == classOf[BigDecimal]) 
        Right(BigDecimal(s).asInstanceOf[A])
      else if (clazz == classOf[Any] ||
          clazz == classOf[Nothing] ||
          clazz == classOf[UniformData[_]] ||
          clazz == classOf[UniformPrimitive[_]]) {

        // Any passed, need to find best type ourselves
        if (isDouble) convertToNumber(classOf[Double], s) match {
          case Right(d) => Right(d.asInstanceOf[A])
          case Left(l) => Left(l)
        }
        else convertToNumber(classOf[Int], s) match {
          case Right(i) => Right(i.asInstanceOf[A])
          case _ => convertToNumber(classOf[Long], s) match { // try long
            case Right(l) => Right(l.asInstanceOf[A])
            case _ => convertToNumber(classOf[BigInt], s) match { // try BigInt
              case Right(bi) => Right(bi.asInstanceOf[A])
              case Left(l) => try {  // try date
                Right(settings.dateFormatter.parse(s).asInstanceOf[A])
              } catch {
                case e: Exception => Left(l)
              }
            }
          }
        }
      }
      else Left("type mismatch: expecting " + manifest[A] + " not Number")
    } catch {
      case e: Exception => 
        Left("invalid " + manifest[A] + ": " + e.getMessage())
    }

    // Special case for enumerations that are numbers
    if (classOf[Enumeration$Value].isAssignableFrom(manifest[A].erasure)) {

      val name = scalafy.util.casing.Casing.toUpperCamelCase(s.toString)
      // Check if Int id
      try {
        val enumOpt = getEnumValueById(name.toInt, settings)
        if (!enumOpt.isEmpty) return Right(enumOpt.get.asInstanceOf[A])
      } catch {
        case _ => {}
      }

      Left("unknown enumeration id '" + name + 
            "': typeHintSettings may be required")
    // All other numbers
    } else {

      convertToNumber(manifest[A].erasure, s.toString) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case l => l
      }
    }
  }

  /** Parses boolean true or false */
  protected[parser] def parseBoolean[A : Manifest](
    iter: BufferedIterator[Char]
  ): Either[String, A] = {

    val clazz = manifest[A].erasure

    if (!(clazz == classOf[Boolean] ||
        clazz == classOf[Any] ||
        clazz == classOf[Nothing] ||
        clazz == classOf[UniformData[_]] ||
        clazz == classOf[UniformPrimitive[_]])) {
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

    if (expectedChars.hasNext) Left("invalid Boolean") 
    else Right(isTrue.asInstanceOf[A])
  }

  /** Parses null */
  protected[parser] def parseNull[A : Manifest](
    iter: BufferedIterator[Char]
  ): Either[String, A] = {
  
    if (!scalafy.util.isNullAllowed(manifest[A].erasure)) {
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

  /** Parses List */
  protected[parser] def parseList[A : Manifest](
    iter: Iterator[ParseEvent],
    settings: ParserSettings
  ): Either[String, ArrayData[A]] = {

    // Create seq 
    val xs = 
      if (manifest[A] == manifest[Any] || manifest[A] == manifest[Nothing]) {
        // If specific type not given, default to List
        new SeqData(manifest[List[Any]], settings)
      } else if (isTupleType(manifest[A])) {
        // Special handling for Tuples 
        new TupleData(manifest[A], settings)
      } else if (isSeqType(manifest[A]) || 
          manifest[A].erasure == classOf[Iterable[_]] ||
          manifest[A].erasure == classOf[Iterator[_]]) {
        new SeqData(manifest[A], settings)
      } else if (manifest[A].erasure == classOf[UniformList[_]] ||
          manifest[A].erasure == classOf[UniformData[_]]) {
        new UniformListData(manifest[A], settings)
      } else return Left("type mismatch: expecting List not " + manifest[A]) 

    // Now parse the list...
    var i = 0
    while (true) {
      parseNext(iter, settings)(xs.getItemType(i)) match {
        case Left(l) => return Left(l)
        case Right(r) => r match { 
          case Some(v) => xs.add(v); i += 1
          case None =>  // Done
            return Right(xs.asInstanceOf[ArrayData[A]])
        }
      }
    }
    // Can't get here...
    Left("Not possible")
  }

  /** Parses Object */
  protected[parser] def parseObject[A : Manifest](
    iter: Iterator[ParseEvent],
    settings: ParserSettings
  ): Either[String, ObjectData[A,_]] = {
    // Create object
    val xm = 
      if (manifest[A] == manifest[Any] || manifest[A] == manifest[Nothing]) {
        // If specific type not given, default to Map 
        new MapData(manifest[Map[Symbol, Any]], manifest[Symbol], settings)
      } else if (isMapType(manifest[A])) {
        val keyType = manifest[A].typeArguments(0).erasure
        // Type must be a basic type
        if (!(keyType == classOf[String] || keyType == classOf[Symbol] ||
            !scalafy.util.isNullAllowed(keyType))) {
          return Left(
            "field types of " + manifest[A].typeArguments(0) + " not supported")
        } else new MapData(manifest[A], manifest[A].typeArguments(0), settings)
      } else if (manifest[A].erasure == classOf[UniformMap[_]] ||
          manifest[A].erasure == classOf[UniformData[_]]) {
        new UniformMapData(manifest[A], settings)
      } else new ReflectionData(manifest[A], settings)

    // Now parse the object...
    parseNext(iter, xm, {
      case (name, value) => xm.add(name, value) match {
        case Some(v) => Some(v)
        case None => None // success
      }
      case o => Some("unexpected data: " + o) 
    }, false, settings) match {
      case Left(e) => Left(e) // error
      case _ => Right(xm.asInstanceOf[ObjectData[A,_]])  // success
    }
  }

  /** Parses Option
    * 
    * In this case we are returning an Option for the OptionData because
    * it is possible that we don't read anything (end of List/Array markers)
    * and we don't want to return those values as Option None
    */
  protected[parser] def parseOption[A : Manifest](
    iter: Iterator[ParseEvent],
    settings: ParserSettings
  ): Either[String, Option[A]] = {

    // Create object
    val option = new OptionData(manifest[A], settings) 

    // Now parse the data to go into the option...
    parseNext(iter, null, null, true, settings)(option.getItemType()) match {
      case Left(l) => Left(l)
      case Right(r) => r match { 
        case Some(v) => 
          if (v != None && v != null) option.add(v) // None is default
          option.getObj() match {
            case Right(r) => Right(Some(r.asInstanceOf[A]))
            case Left(l) => Left(l)
          }
        case None => Right(None)
      }
    }
  }

  protected[parser] def getEnumValueByName(
    name: String, settings: ParserSettings
  ): Option[Enumeration#Value] = {
    for {
      e <- settings.typeHintSettings.enums
      v <- e.values
      if (v.toString == name)
    } {
      return Some(v)
    }

    None
  }

  protected[parser] def getEnumValueById(
    id: Int, settings: ParserSettings
  ): Option[Enumeration#Value] = {
    for {
      e <- settings.typeHintSettings.enums
      v <- e.values
      if (v.id == id)
    } {
      return Some(v)
    }

    None
  }

}
