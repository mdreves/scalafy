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
    * @param value value to encode
    * @param converter converter between parse events and strings 
    * @return encoded string 
    */
  def toText(
    value: Any, converter: (ParseEvent) => String, settings: ParserSettings
  ): String = {
    val prettyPrint = settings.prettyPrintSettings.enabled
    val indent = settings.prettyPrintSettings.indent

    val arraySepLen = converter(ArrayItemSeparator).length
    val objSepLen = converter(ObjectNameValuePairSeparator).length
    
    // Prints output to buffer
    def printToBuf(buf: StringBuilder, value: Any, offset: Int) {
      // Null
      if (value == null) append(buf, converter(NullValue("null")), 0)

      // String
      else if (value.isInstanceOf[String] || value.isInstanceOf[Char]) {
        append(buf, converter(StringValue(value.toString)), 0)
      }
      else if (value.isInstanceOf[Symbol]) {
        append(buf, converter(StringValue(value.asInstanceOf[Symbol].name)), 0)
      }
      
      // Number 
      else if (value.isInstanceOf[Int] || value.isInstanceOf[Short] ||
          value.isInstanceOf[Long] || value.isInstanceOf[Float] ||
          value.isInstanceOf[Double] || value.isInstanceOf[Byte] ||
          value.isInstanceOf[Boolean]) {
        append(buf, converter(NumberValue(value.toString)), 0)
      } 
     
      // Boolean
      else if (value.isInstanceOf[Boolean]) {
        append(buf, converter(BooleanValue(value.toString)), 0)
      }

      // Map (check Map before Seq so Iterable get matched)
      else if (isMapType(value.getClass)) {
        append(buf, converter(ObjectStart), 0)
        if (prettyPrint) append(buf, "\n", 0)

        for ((k,v) <- value.asInstanceOf[Iterable[_]]) {
          val convertedName = 
            convertName(k) + converter(ObjectNameValueSeparator)

          append(buf, convertedName, offset + indent)
          if (prettyPrint && convertedName != "") append(buf, " ", 0)

          printToBuf(buf, v, offset + indent)
          append(buf, converter(ObjectNameValuePairSeparator), 0)
          if (prettyPrint) append(buf, "\n", 0)
        }

        if (prettyPrint) buf.length -= (objSepLen + 1) 
        else buf.length -= objSepLen 
        if (prettyPrint) append(buf, "\n", 0)
        append(buf, converter(ObjectEnd), offset)
      }

    
      // Seq
      else if (classOf[Iterable[_]].isAssignableFrom(value.getClass) ||
          classOf[Iterator[_]].isAssignableFrom(value.getClass)) {
        append(buf, converter(ArrayStart), 0)

        val iter = 
          if (classOf[Iterable[_]].isAssignableFrom(value.getClass))
            value.asInstanceOf[Iterable[_]]
          else value.asInstanceOf[Iterator[_]]

        for (x <- iter) { 
          printToBuf(buf, x, 0) 
          append(buf, converter(ArrayItemSeparator), 0)
          if (prettyPrint) append(buf, " ", 0)
        }

        if (prettyPrint) buf.length -= (objSepLen + 1) 
        else buf.length -= arraySepLen 
        append(buf, converter(ArrayEnd), 0)
      } 

      // Tuple 
      else if (isTupleType(value.getClass)) {
        append(buf, converter(ArrayStart), 0)

        for (x <- value.asInstanceOf[Product].productIterator) { 
          printToBuf(buf, x, 0) 
          append(buf, converter(ArrayItemSeparator), 0)
          if (prettyPrint) append(buf, " ", 0)
        }

        if (prettyPrint) buf.length -= (arraySepLen + 1)
        else buf.length -= arraySepLen 
        append(buf, converter(ArrayEnd), 0)
      } 


      // Object (use reflection)
      else {
        append(buf, converter(ObjectStart), 0)
        if (prettyPrint) append(buf, "\n", 0)

        // Append any Opaque data
        var opaque: Map[Symbol,Any] = 
          if (value.isInstanceOf[AnyRef]) 
            OpaqueData.get(value.asInstanceOf[AnyRef]) match {
            case Some(v) => v
            case None => null 
          } else null

        value.getClass.getDeclaredFields.map { field =>
          // Find method with same name
          try {
            val fieldValue = value.getClass.getMethod(
                field.getName).invoke(value)
      
            val convertedName = convertName(field.getName) +
              converter(ObjectNameValueSeparator)

            append(buf, convertedName, offset + indent)
            if (prettyPrint && convertedName != "") append(buf, " ", 0)

            printToBuf(buf, fieldValue, offset + indent)
            append(buf, converter(ObjectNameValuePairSeparator), 0)
            if (prettyPrint) append(buf, "\n", 0)

            if (opaque != null) opaque = opaque - Symbol(field.getName) 
          } catch {
            case e: NoSuchMethodException => {} 
          }
        }

        // Add opaque data
        if (settings.opaqueDataSettings.enabled && opaque != null) {
          for ((k, v) <- opaque) {
            val convertedName = 
              convertName(k) + converter(ObjectNameValueSeparator)

            append(buf, convertedName, offset + indent)
            if (prettyPrint && convertedName != "") append(buf, " ", 0)

            printToBuf(buf, v, offset + indent)
            append(buf, converter(ObjectNameValuePairSeparator), 0)
            if (prettyPrint) append(buf, "\n", 0)
          }
        }

        if (prettyPrint) buf.length -= (objSepLen + 1)
        else buf.length -= objSepLen 
        if (prettyPrint) append(buf, "\n", 0)
        append(buf, converter(ObjectEnd), offset)
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
    def append(buf: StringBuilder, s: String, offset: Int) =  {
      if (prettyPrint) for (i <- 0 until offset) buf.append(" ")
      buf.append(s)
    }

    val buf = new StringBuilder()
    printToBuf(buf, value, 0)
    buf.toString
  } 


  // Helpers

  /** parseNext for use with array/primitive parsing */
  protected[parser] def parseNext[A : Manifest](
    iter: Iterator[ParseEvent], settings: ParserSettings
  ): Either[String, Option[A]] = {
    parseNext(iter, null, null, settings)
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
   * @return Right(value), Right(None) - no more data, or Left(error message)
   */
  protected[parser] def parseNext[A : Manifest](
    iter: Iterator[ParseEvent],
    obj: ObjectData[_,_], // container parsed name/values will be stored in
    pf: PartialFunction[Tuple2[Any,Any],Option[String]], // for each name/value
    settings: ParserSettings
  ): Either[String, Option[A]] = {

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
              fieldIter.iterator.buffered)(obj.getNameManifest) match {
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
        val itemType = if (obj == null) manifest[A] else obj.getItemType(name)
        parseString(iterable.iterator.buffered)(itemType) match {
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

      // Number value
      case NumberValue(iterable) =>
        val itemType = if (obj == null) manifest[A] else obj.getItemType(name)
 
        parseNumber(iterable.iterator.buffered)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(value) =>  {
            if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

            pf((name, value)) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }

      // Boolean value
      case BooleanValue(iterable) =>
        val itemType = if (obj == null) manifest[A] else obj.getItemType(name)
 
        parseBoolean(iterable.iterator.buffered)(itemType) match {
          case Left(l) => return Left(l) 
          case Right(value) => {
            if (obj == null) return Right(Some(value.asInstanceOf[A])) // done

            pf(name, value) match {
              case Some(e) => return Left(e)
              case None => name = null  // success, reset
            }
          }
        }

      // Null value
      case NullValue(iterable) =>
        val itemType = if (obj == null) manifest[A] else obj.getItemType(name)

        parseNull(iterable.iterator.buffered)(itemType) match {
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
      case ObjectStart => 
        val itemType = if (obj == null) manifest[A] else obj.getItemType(name) 

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
         manifest[A] == manifest[UniformList[_]] ||  
         manifest[A] == manifest[UniformMap[_]])) {
      return Left("missing end of object/list")
    }
    
    Right(None)   // End of data
  }

  /** Parses string (can be parsed as any primitive) */
  protected[parser] def parseString[A : Manifest](
    iter: BufferedIterator[Char]
  ): Either[String, A] = {
      if (!(manifest[A] >:> manifest[String] || 
          manifest[A] >:> manifest[Symbol] ||
          classOf[UniformData[_]].isAssignableFrom(manifest[A].erasure) ||
          manifest[A] <:< manifest[AnyVal])) {
      return Left("type mismatch: expecting " + manifest[A] + " not String")
    }

    if (manifest[A] == manifest[Int] ||
        manifest[A] == manifest[Short] ||
        manifest[A] == manifest[Long] || 
        manifest[A] == manifest[Float] ||
        manifest[A] == manifest[Double] || 
        manifest[A] == manifest[Byte] ||
        manifest[A].erasure == classOf[UniformInt] || 
        manifest[A].erasure == classOf[UniformShort] || 
        manifest[A].erasure == classOf[UniformLong] || 
        manifest[A].erasure == classOf[UniformFloat] || 
        manifest[A].erasure == classOf[UniformDouble] || 
        manifest[A].erasure == classOf[UniformByte]) {

      // If numeric type requested, pass off to parseNumber
      parseNumber(iter)(manifest[A]) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left(l) // error
      }

    } else if (manifest[A] == manifest[Boolean] ||
        manifest[A].erasure == classOf[UniformBoolean]) { 

      // If boolean type requested, pass off to parseBoolean
      parseBoolean(iter)(manifest[A]) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left(l) // error
      }

    } else {

      var s = new StringBuilder
      while (iter.hasNext) s + iter.next 

      if (manifest[A] == manifest[Char] ||
          manifest[A].erasure == classOf[UniformChar]) { 
        if (s.length == 1) Right(s.toString()(0).asInstanceOf[A])
        else Left("type mismatch: expecting Char not String")
      } else if (manifest[A] == manifest[Symbol] ||
          manifest[A].erasure == classOf[UniformSymbol]) { 
        Right(Symbol(s.toString).asInstanceOf[A])
      } else Right(s.toString().asInstanceOf[A])
    }
  }

  /** Parses Number */
  protected[parser] def parseNumber[A : Manifest](
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
      else if (classOf[UniformData[_]].isAssignableFrom(manifest[A].erasure))
        "Number" 
      else if (manifest[A].erasure == classOf[UniformInt]) "Int" 
      else if (manifest[A].erasure == classOf[UniformShort]) "Short" 
      else if (manifest[A].erasure == classOf[UniformLong]) "Long" 
      else if (manifest[A].erasure == classOf[UniformFloat]) "Float" 
      else if (manifest[A].erasure == classOf[UniformDouble]) "Double" 
      else if (manifest[A].erasure == classOf[UniformByte]) "Byte" 
      else return Left(
        "type mismatch expecting " + manifest[A] + " not number")

    if (!iter.hasNext || !(iter.head == '-' || iter.head.isDigit)) 
      return Left("invalid " + typeName)

    var s = new StringBuilder()

    // Skip over minus sign (NOTE: parser does not allow starting with +)
    if (iter.head == '-') {
      s + iter.head
      if (iter.hasNext) iter.next() 
      else return Left("invalid " + typeName) 
    }

    val isDoubleChar = (c: Char) =>
      c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+'

    var next = iter.head
    var isDouble = false
    while (iter.hasNext && (next.isDigit || isDoubleChar(next))) {
      if (isDoubleChar(next)) isDouble = true
      s + iter.next()
      if (iter.hasNext) next = iter.head
    }

    // Helper to convert string to number
    def convertToNumber[A : Manifest](s: String): Either[String, A] = try {
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

    // Handle case where Any was passed and we must find best match ourselves
    if (manifest[A] == manifest[Any] || manifest[A] == manifest[Nothing] ||
        manifest[A].erasure == classOf[UniformData[_]] ||
        manifest[A].erasure == classOf[UniformPrimitive[_]]) {

      if (isDouble) convertToNumber[Double](s.toString) match {

        case Right(d) => Right(d.asInstanceOf[A])
        case Left(l) => Left("invalid Double, " + l)

      } else convertToNumber[Int](s.toString) match {

        case Right(i) => Right(i.asInstanceOf[A])
        case Left(l) => convertToNumber[Long](s.toString) match { // try long
          case Right(l) => Right(l.asInstanceOf[A])
          case Left(l) => Left("invalid Int, " + l)
        }

      }

    } else convertToNumber[A](s.toString) match {

      case Right(r) => Right(r.asInstanceOf[A])
      case Left(l) => Left("invalid " + typeName + ", " + l)

    }
  }

  /** Parses boolean true or false */
  protected[parser] def parseBoolean[A : Manifest](
    iter: BufferedIterator[Char]
  ): Either[String, Boolean] = {
    if (!(manifest[A] >:> manifest[Boolean] ||
        manifest[A].erasure == classOf[UniformBoolean] ||
        manifest[A].erasure == classOf[UniformData[_]] ||
        manifest[A].erasure == classOf[UniformPrimitive[_]])) {
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

  /** Parses null */
  protected[parser] def parseNull[A : Manifest](
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

  /** Parses List */
  protected[parser] def parseList[A : Manifest](
    iter: Iterator[ParseEvent],
    settings: ParserSettings
  ): Either[String, ArrayData[A]] = {
    // Special handling for Iterable and Iterator
    if (manifest[A] <:< manifest[Iterable[_]] || 
        manifest[A] <:< manifest[Iterator[_]]) {
      Right(new IterableData(iter, manifest[A], settings)) 
    } 
    
    // Special handling for Streams 
    else if (manifest[A] <:< manifest[Stream[_]]) {
      Right(new StreamData(iter, manifest[A], settings)) 
    } 
     
    // Any other seq type... 
    else {
      // Create seq 
      val xs = 
        if (manifest[A] == manifest[Any] || manifest[A] == manifest[Nothing]) {
          // If specific type not given, default to List
          new SeqData(manifest[List[Any]], settings)
        } else if (isTupleType(manifest[A])) {
          // Special handling for Tuples 
          new TupleData(manifest[A], settings)
        } else if (isSeqType(manifest[A])) {
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
        val keyType = manifest[A].typeArguments(0)
        // Type must be a basic type
        if (!(keyType == manifest[String] || keyType == manifest[Symbol] ||
            keyType <:< manifest[AnyVal])) {
          return Left(
            "field types of " + manifest[A].typeArguments(0) + " not supported")
        } else new MapData(manifest[A], keyType, settings)
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
    }, settings) match {
      case Left(e) => Left(e) // error
      case _ => Right(xm.asInstanceOf[ObjectData[A,_]])  // success
    }
  }
}
