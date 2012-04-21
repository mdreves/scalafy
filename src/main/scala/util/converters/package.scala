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

import scalafy.types.meta._

/** Utils for converting between types.
  *
  * Converters vs Uniform types vs Records:
  * <li> Converters should be used when dealing with multiple objects at a time
  * and you need to work with them in various forms (Map, List, object graph) 
  * at different times. Converters impose no constraints other than the types
  * must be convertable (either strictly or on a best-effort basis)
  * <li> If dealing with multiple objects/keys/values at a time and you wish 
  * to maintain a pure data form, use Uniform types instead. Uniform types
  * constrain you to only using primitive data forms, but you can add any
  * number of keys and convert to any compatible object when you are done.
  * <li> If you are only dealing with keys/values for one particular object
  * at a time and it is possible/desireable to tag these as records, then use
  * Records. Records will constrain you to only using keys that belong to that
  * particular object (else throws exception)
  * 
  * Note, by default conversions are done as all or nothing. If you wish to
  * do conversion on a best-effort basis then either directly pass the 
  * bestEffort flag or set the implicit ConversionSettings prior to making calls
  *
  * The following is a summary of features:
  * {{{
  * // Conversion between primitives
  * to[Int]("1")                               // Some(1)
  * to[Int]("foo")                             // None 
  * to[Char]("f")                              // Some('f')
  * to[Char]("foo")                            // None 
  * to[Boolean](0)                             // Some(false) 
  * to[Boolean](1.0)                           // Some(true) 
  * to[Short](32767)                           // Some(32767) 
  * to[Short](32768)                           // None 
  *
  * // Conversion between Seqs of primitives
  * to[List[Int]](List("1", "2"))              // Some(List(1, 2))
  * to[List[Boolean]](List("true", "false"))   // Some(List(true, false))
  * to[Vector[String]](List('foo, 'bar))       // Some(Vector("foo", "bar"))
  *
  * // Conversion between Maps of primitives
  * to[Map[Char, Int]](Map("f" -> "1")         // Some(Map('f' -> 1)) 
  * to[Map[String, Boolean]](Map('test -> 'T') // Some(Map("test" -> true))
  * // Any leaves type as is
  * to[Map[String, Any]](Map('test -> 1)       // Some(Map("test" -> 1))
  *
  * // Conversion to/from tuples
  * to[Tuple2[String,Int]]('test -> "1")       // Some("test" -> 1)
  * to[Tuple2[String,Int]](List('test, "1"))   // Some("test" -> 1)
  * to[List[Tuple2[Int,Int]]](List(List('test, "1"))) // Some(List("test" -> 1))
  * to[List[Int]](1 -> 2)                      // Some(List(1,2))
  *
  * // Conversion from objects to Maps (using reflection)
  * class Foo(val s: String, val i: Int) {
  *   def this(s: String) = this(s, 1)  // NOTE: supports multiple constructors
  *   def this(i: Int) = this("yy", i)
  * }
  * to[Map[Symbol, Any]](new Foo("xx", 2))     // Some(Map('s -> "xx", 'i -> 2))
  * to[Foo](Map('s -> "xx", 'i -> 2))          // Some(Foo("xx", 2))
  * to[Map[Symbol, Any]](new Foo("xx"))        // Some(Map('s -> "xx", 'i -> 1))
  * to[Foo](Map('s -> "xx"))                   // Some(Foo("xx", 1))
  * to[Map[Symbol, Any]](new Foo(5))           // Some(Map('s -> "yy", 'i -> 5))
  * to[Foo](Map('i -> 5))                      // Some(Foo("yy", 5))
  *
  * // Conversion from objects to objects (using reflection)
  * case class Bar(s: String)
  * to[Foo](Bar("bar"))                        // Some(Foo("bar", 1)) 
  *
  * // Conversion with Embedded Lists/Maps/Tuples 
  * to[List[Map[Int,Int]]](List(Map("2" -> "1")) // Some(List(Map(2 -> 1)))
  * to[List[Foo]](List(Map("i" -> "1"))          // Some(List(Foo("yy",1)))
  * to[Map[Symbol,Foo]](Map('t -> Map("i" -> 1)))// Some(Map('t -> Foo("yy",1)))
  * to[Tuple2[Foo,Foo]](Map('i -> 8),Map("i" -> 1))// Some(Foo(...) -> Foo(...))
  *
  * // Filtering by field value types (bestEffort = true)
  * to[Map[Symbol, String]](new Foo("xx", 2))(true) // Some(Map('s -> "xx"))
  *
  * // Configuring default settings
  * implicit val conversionSettings = ConversionSettings(
  *   true                        // best effort conversion (default is false)
  *   OpaqueDataSettings(false)   // store opaque data (default is false)
  * )
  * }}}
  */
package object converters {

  ///////////////////////////////////////////////////////////////////////////
  // Settings 
  ///////////////////////////////////////////////////////////////////////////

  /** Conversion settings of use with implicit params */
  case class ConversionSettings(
    bestEffort: Boolean, 
    opaqueDataSettings: OpaqueDataSettings)

  implicit val conversionSettings = ConversionSettings(
    false, 
    OpaqueDataSettings(false)
  )


  ///////////////////////////////////////////////////////////////////////////
  // Converters 
  ///////////////////////////////////////////////////////////////////////////

  /** Converts between types
    *
    * Conversion can be from primitive to primitive, TupleN to TupleN,
    * Seq to Seq, Map to Map, Map to Object, Object to Object, Object to Map.
    *
    * Conversions can be between different primitive types and/or between
    * different Seq/Map/Object types (immutable and mutable). 
    *
    * See package documentation for examples.
    * 
    * @return converted type or None if conversion not possible
    */
  def to[A](from: Any)(
    implicit m: Manifest[A], settings : ConversionSettings
  ): Option[A] = toType(from, m, settings) match { 
    case Right(r) => Some(r.asInstanceOf[A])
    case Left(l) => None
  }

  /** to with explicit bestEffort flag */
  def to[A : Manifest](from: Any, bestEffort: Boolean): Option[A] =
    to[A](from)(manifest[A], 
      ConversionSettings(bestEffort, conversionSettings.opaqueDataSettings))

  /** Makes a shallow copy of an object using reflection */
  def shallowCopy[A](src: A): Option[A] = toObj(
      src, src.getClass, conversionSettings) match {
    case Right(r) => Some(r._1.asInstanceOf[A])
    case Left(l) => None
  }


  ///////////////////////////////////////////////////////////////////////////
  // Helpers 
  ///////////////////////////////////////////////////////////////////////////

  private[scalafy] def toType(
    x: Any, m: Manifest[_], settings: ConversionSettings
  ): Either[String,Any] = {
    if (m == manifest[Any])
      Right(x) // no conversion
    else if (scalafy.util.isPrimitiveType(m))
      toPrimitive(x, m)
    else if (scalafy.util.isMapType(m))
      toMap(x, m, settings)
    else if (scalafy.util.isSeqType(m))
      toSeq(x, m, settings)
    else if (scalafy.util.isTupleType(m))
      toTuple(x, m, settings)
    else toObj(x, m, settings) match {
      case Right(r) => Right(r._1) // Ignore unused data
      case Left(l) => Left(l)
    }
  }

  /** Converts to a primtive type from to another primitive type 
    * 
    * @return primitive of given type or String error if conversion not possible
    */
  private[scalafy] def toPrimitive(
    x: Any, m: Manifest[_]
  ): Either[String,Any] = {
    toPrimitive(x, m.erasure)
  }

  private[scalafy] def toPrimitive(x: Any, to: Class[_]): Either[String,Any] = {
    val clazz = toClass(x)
    if (clazz == to) return Right(x)

    // Helper functions for formatting errors
    def formatError(msg: String = null): Either[String, Any] = {
      if (msg == null) Left(x + " is not convertable to " + to)
      else Left(x + " is not convertable to " + to + ": " + msg)
    }

    if (to == classOf[String]) {
      // String
      if (clazz == classOf[Symbol]) Right(x.asInstanceOf[Symbol].name)
      else Right(x.toString)
    } else if (to == classOf[Symbol]) {
      // Symbol
      Right(Symbol(x.toString))
    } else if (to == classOf[Char]) {
      // Char
      val s = 
        if (clazz == classOf[Symbol]) x.asInstanceOf[Symbol].name
        else x.toString
     
      if (s.length == 1) Right(s(0))
      else if (s == "true") Right('T')
      else if (s == "false") Right('F')
      else formatError()
    } else if (to == classOf[Boolean]) {
      // Boolean
      val s = 
        if (clazz == classOf[Symbol]) x.asInstanceOf[Symbol].name.toLowerCase
        else x.toString.toLowerCase
      
      if (s == "true" || s == "t" || s == "1" || s == "1.0") Right(true)
      else if (s == "false" || s == "f" || s == "0" || s == "0.0") Right(false)
      else formatError() 
    } else try { 
      // Short
      if (to == classOf[Short]) {
        if (clazz == classOf[Int]) {
          val i = x.asInstanceOf[Int]
          if (i > Short.MinValue && i < Short.MaxValue) Right(i.toShort)
          else formatError()
        } else if (clazz == classOf[Long]) {
          val l = x.asInstanceOf[Long]
          if (l > Short.MinValue && l < Short.MaxValue) Right(l.toShort)
          else formatError()
        } else if (clazz == classOf[Byte]) {
          val b = x.asInstanceOf[Byte]
          if (b > Short.MinValue && b < Short.MaxValue) Right(b.toShort)
          else formatError()
        } else if (clazz == classOf[Float]) {
          Right(x.asInstanceOf[Float].toShort)
        } else if (clazz == classOf[Double]) {
          Right(x.asInstanceOf[Double].toShort)
        } else if (clazz == classOf[String]) {
          Right(x.asInstanceOf[String].toShort)
        } else if (clazz == classOf[Symbol]) {
          Right(x.asInstanceOf[Symbol].name.toShort)
        } else if (clazz == classOf[Char]) {
          Right(x.asInstanceOf[Char].toShort)
        } else if (x == true) {
          Right(1.toShort)
        } else if (x == false) {
          Right(0.toShort)
        } else formatError()
      }

      // Int
      else if (to == classOf[Int]) {
        if (clazz == classOf[Short]) {
          val s = x.asInstanceOf[Short]
          if (s > Int.MinValue && s < Int.MaxValue) Right(s.toInt)
          else formatError()
        } else if (clazz == classOf[Long]) {
          val l = x.asInstanceOf[Long]
          if (l > Int.MinValue && l < Int.MaxValue) Right(l.toInt)
          else formatError()
        } else if (clazz == classOf[Byte]) {
          val b = x.asInstanceOf[Byte]
          if (b > Int.MinValue && b < Int.MaxValue) Right(b.toInt)
          else formatError()
        } else if (clazz == classOf[Float]) {
          Right(x.asInstanceOf[Float].toInt)
        } else if (clazz == classOf[Double]) {
          Right(x.asInstanceOf[Double].toInt)
        } else if (clazz == classOf[String]) {
          Right(x.asInstanceOf[String].toInt)
        } else if (clazz == classOf[Symbol]) {
          Right(x.asInstanceOf[Symbol].name.toInt)
        } else if (clazz == classOf[Char]) {
          Right(x.asInstanceOf[Char].toInt)
        } else if (x == true) {
          Right(1)
        } else if (x == false) {
          Right(0)
        } else formatError()
      }

      // Long
      else if (to == classOf[Long]) {
        if (clazz == classOf[Short]) {
          val s = x.asInstanceOf[Short]
          if (s > Long.MinValue && s < Long.MaxValue) Right(s.toLong)
          else formatError()
        } else if (clazz == classOf[Int]) {
          val i = x.asInstanceOf[Int]
          if (i > Long.MinValue && i < Long.MaxValue) Right(i.toLong)
          else formatError()
        } else if (clazz == classOf[Byte]) {
          val b = x.asInstanceOf[Byte]
          if (b > Long.MinValue && b < Long.MaxValue) Right(b.toLong)
          else formatError()
        } else if (clazz == classOf[Float]) {
          Right(x.asInstanceOf[Float].toLong)
        } else if (clazz == classOf[Double]) {
          Right(x.asInstanceOf[Double].toLong)
        } else if (clazz == classOf[String]) {
          Right(x.asInstanceOf[String].toLong)
        } else if (clazz == classOf[Symbol]) {
          Right(x.asInstanceOf[Symbol].name.toLong)
        } else if (clazz == classOf[Char]) {
          Right(x.asInstanceOf[Char].toLong)
        } else if (x == true) {
          Right(1L)
        } else if (x == false) {
          Right(0L)
        } else formatError()
      }

      // Byte
      else if (to == classOf[Byte]) {
        if (clazz == classOf[Short]) {
          val s = x.asInstanceOf[Short]
          if (s > Byte.MinValue && s < Byte.MaxValue) Right(s.toByte)
          else formatError()
        } else if (clazz == classOf[Int]) {
          val i = x.asInstanceOf[Int]
          if (i > Byte.MinValue && i < Byte.MaxValue) Right(i.toByte)
          else formatError()
        } else if (clazz == classOf[Long]) {
          val l = x.asInstanceOf[Long]
          if (l > Byte.MinValue && l < Byte.MaxValue) Right(l.toByte)
          else formatError()
        } else if (clazz == classOf[Float]) {
          Right(x.asInstanceOf[Float].toByte)
        } else if (clazz == classOf[Double]) {
          Right(x.asInstanceOf[Double].toByte)
        } else if (clazz == classOf[String]) {
          Right(x.asInstanceOf[String].toByte)
        } else if (clazz == classOf[Symbol]) {
          Right(x.asInstanceOf[Symbol].name.toByte)
        } else if (clazz == classOf[Char]) {
          Right(x.asInstanceOf[Char].toByte)
        } else if (x == true) {
          Right(1.toByte)
        } else if (x == false) {
          Right(0.toByte)
        } else formatError()
      }

      // Float 
      else if (to == classOf[Float]) {
        if (clazz == classOf[Short]) {
          Right(x.asInstanceOf[Short].toFloat)
        } else if (clazz == classOf[Int]) {
          Right(x.asInstanceOf[Int].toFloat)
        } else if (clazz == classOf[Long]) {
          Right(x.asInstanceOf[Long].toFloat)
        } else if (clazz == classOf[Byte]) {
          Right(x.asInstanceOf[Byte].toFloat)
        } else if (clazz == classOf[Double]) {
          Right(x.asInstanceOf[Double].toFloat)
        } else if (clazz == classOf[String]) {
          Right(x.asInstanceOf[String].toFloat)
        } else if (clazz == classOf[Symbol]) {
          Right(x.asInstanceOf[Symbol].name.toFloat)
        } else if (clazz == classOf[Char]) {
          Right(x.asInstanceOf[Char].toFloat)
        } else if (x == true) {
          Right(1.0f)
        } else if (x == false) {
          Right(0.0f)
        } else formatError()
      }

      // Double 
      else if (to == classOf[Double]) {
        if (clazz == classOf[Short]) {
          Right(x.asInstanceOf[Short].toDouble)
        } else if (clazz == classOf[Int]) {
          Right(x.asInstanceOf[Int].toDouble)
        } else if (clazz == classOf[Long]) {
          Right(x.asInstanceOf[Long].toDouble)
        } else if (clazz == classOf[Byte]) {
          Right(x.asInstanceOf[Byte].toDouble)
        } else if (clazz == classOf[Float]) {
          Right(x.asInstanceOf[Float].toDouble)
        } else if (clazz == classOf[String]) {
          Right(x.asInstanceOf[String].toDouble)
        } else if (clazz == classOf[Symbol]) {
          Right(x.asInstanceOf[Symbol].name.toDouble)
        } else if (clazz == classOf[Char]) {
          Right(x.asInstanceOf[Char].toDouble)
        } else if (x == true) {
          Right(1.0)
        } else if (x == false) {
          Right(0.0)
        } else formatError()
      }

      else formatError()
    } catch {
      case e: NumberFormatException => formatError(e.getMessage()) 
    }
  }

  /** Converts to a tuple from another tuple */
  private[scalafy] def toTuple(
    from: Any, m: Manifest[_], settings : ConversionSettings
  ): Either[String, Any] = {
    if (m.typeArguments.size < 2 && m != manifest[Any]) 
      return Left("cannot convert from " + from.getClass + " to " + m)

    var xs = collection.mutable.ListBuffer[Any]()
   
    val iterator =
      if (isSeqType(from.getClass)) 
        from.asInstanceOf[Iterable[_]].iterator
      else from.asInstanceOf[Product].productIterator

    var offset = 0
    for (x <- iterator) {
      val xManifest = if (m == manifest[Any]) m else m.typeArguments(offset)
      toType(x, xManifest, settings) match {
        case Left(l) => return Left(l)
        case Right(xNew) => xs += xNew
      }

      offset += 1
    }

    if (xs.size == m.typeArguments.size || m == manifest[Any]) {
      createTuple(xs : _*)
    } else if (m.typeArguments.size < xs.size && settings.bestEffort) {
      createTuple(xs.take(m.typeArguments.size) : _*)
    } else { 
      Left("cannot convert from " + from.getClass + " to " + m)
    }
  }

  /** Converts to a Seq from a Seq */
  private[scalafy] def toSeq(
    from: Any, m: Manifest[_], settings : ConversionSettings
  ): Either[String, Any] = {
    if (m.typeArguments.size != 1) 
      return Left("cannot convert from " + from.getClass + " to " + m)

    // Special case for List, since we need to reverse the list, we will
    // use a ListBuffer and convert it to List later
    val clazz = 
      if (m.erasure == classOf[List[_]] ||  
          m.erasure == classOf[::[_]] || 
          m.erasure == classOf[Seq[_]] || 
          m.erasure == classOf[collection.immutable.LinearSeq[_]]) {
        classOf[collection.mutable.ListBuffer[_]]
      } else m.erasure 
 
    var xs = 
      createSeq(clazz)(m.typeArguments(0)) match {
        case Right(r) => r
        case Left(l) => return Left(l)
      }

    val iter = 
      if (from.isInstanceOf[Iterable[_]])
        from.asInstanceOf[Iterable[_]].iterator
      else if (from.isInstanceOf[Product])
        from.asInstanceOf[Product].productIterator
      else return Left("cannot convert from " + from.getClass + " to " + m)

    for (x <- iter) {
      // Convert key
      toType(x, m.typeArguments(0), settings) match {
        case Left(l) =>
          if (!settings.bestEffort) return Left(l)
          // do nothing (e.g. ignore...)
        
        case Right(xNew) => updateSeq(xs, xNew) match {
          case Right(r) => xs = r // success
          case Left(l) =>
            if (!settings.bestEffort) return Left(l)
            // do nothing (e.g. ignore...)
        }
      }
    }

    if (m.erasure == classOf[List[_]] || m.erasure == classOf[::[_]]) {
      Right(xs.asInstanceOf[collection.mutable.ListBuffer[_]].toList)
    } else if (m.erasure == classOf[Seq[_]]) {
      Right(xs.asInstanceOf[collection.mutable.ListBuffer[_]].toSeq)
    } else if (m.erasure == classOf[collection.immutable.LinearSeq[_]]) {
      Right(xs.asInstanceOf[collection.mutable.ListBuffer[_]].toSeq)
    } else {
      Right(xs)
    }
  }

  /** Converts to a Map from a Map or an object (using reflection) */
  private[scalafy] def toMap(
    from: Any, m: Manifest[_], settings : ConversionSettings
  ): Either[String, Any] = {
    if (m.typeArguments.size != 2) 
      return Left("cannot convert from " + from.getClass + " to " + m)

    // Helper to crate symbol
    def getSymbol(k: Any): Symbol =  
      if (k.isInstanceOf[Symbol]) k.asInstanceOf[Symbol] else Symbol(k.toString)

    var xm = 
      createMap(m.typeArguments(0), 
          m.typeArguments(1), m.erasure) match {
        case Right(r) => r
        case Left(l) => return Left(l)
      }

    var isMap = false
    
    // Track any opaque data that might have been used
    var opaque: Map[Symbol,Any] = 
      if (from.isInstanceOf[AnyRef])
        OpaqueData.get(from.asInstanceOf[AnyRef]) match {
        case Some(v) => v
        case None => null 
      } else null

    // If from is a Map type...
    if (from.isInstanceOf[Iterable[_]]) {
      isMap = true

      // At compile time, don't know if Tuple2 so must catch ClassCastException
      try {

        for ((k,v) <- from.asInstanceOf[Iterable[Tuple2[_,_]]].iterator) {
          // Convert key
          toType(k, m.typeArguments(0), settings) match {
            case Left(l) =>
              if (!settings.bestEffort) return Left(l)
              // do nothing (e.g. ignore...)
            
            // Convert value
            case Right(kNew) => toType(v, m.typeArguments(1), settings) match {
              case Left(l) =>
                if (!settings.bestEffort) return Left(l)
                // do nothing (e.g. ignore...)

              case Right(vNew) => updateMap(xm, kNew, vNew) match {
                case Right(r) =>
                  if (opaque != null) opaque -= getSymbol(k) 
                  xm = r  // success
                case Left(l) =>
                  if (!settings.bestEffort) return Left(l)
                  // do nothing (e.g. ignore...)
              }
            }
          }
        }

      } catch {
        case _: ClassCastException => isMap = false // wan't map after all 
      }
    } 
   
    // from is any obj, use reflection...
    if (!isMap) {

      // Can only convert to key values that are Symbols or Strings
      if (!(m.typeArguments(0) == manifest[Symbol] || 
          m.typeArguments(0) == manifest[String])) 
        return Left("cannot convert from " + from.getClass + " to " + m)

      val clazz = from.getClass

      // Create Map using reflection
      clazz.getDeclaredFields.map { field =>

        // Find method with same name
        try {
          // Scala bug, private trait members are returned as public...
          // We will allow use of __xxx__ to flag them
          if (!field.getName.contains("__")) {
            val key = 
              if (m.typeArguments(0) == manifest[Symbol]) Symbol(field.getName)
              else field.getName

            val value = clazz.getMethod(field.getName).invoke(from)

            if (toClass(value) == m.typeArguments(1).erasure ||
                m.typeArguments(1) == manifest[Any]) {
              updateMap(xm, key, value) match {
                case Right(r) => 
                  if (opaque != null) opaque -= getSymbol(key)
                  xm = r
                case Left(l) => if (!settings.bestEffort) return Left(l) 
              }
            } else if (!settings.bestEffort) {
              return Left("cannot convert from " + 
                value.getClass + " to " + m.typeArguments(1))
            }
          }
        } catch {
          case e: NoSuchMethodException => {}
        }
      }
    }

    // Add opaque data
    if (settings.opaqueDataSettings.enabled && opaque != null) {
      for ((k, v) <- opaque) {
        // Convert key
        toType(k, m.typeArguments(0), settings) match {
          case Left(l) => // do nothing (e.g. ignore...)
          
          // Convert value
          case Right(kNew) => toType(v, m.typeArguments(1), settings) match {
            case Left(l) => // do nothing (e.g. ignore...)

            case Right(vNew) => updateMap(xm, kNew, vNew) match {
              case Right(r) => xm = r  // success
              case Left(l) => // do nothing (e.g. ignore...)
            }
          }
        }
      }
    }

    Right(xm) 
  }

  private[scalafy] def toObj(
    from: Any, m: Manifest[_], settings : ConversionSettings
  ): Either[String, (Any, Map[Symbol,Any])] = {
    toObj(from, m.erasure, settings)
  }

  /** Converts to an object from a Map or another object using reflection
    *
    * @from a Map (immutable or mutable) with field names as Symbols or Strings
    *   or an object
    * @return left (error) right (value and any unused data)
    */
  private[scalafy] def toObj(
    from: Any, clazz: Class[_], settings : ConversionSettings
  ): Either[String, (Any, Map[Symbol,Any])] = {

    var unused = Map[Symbol,Any]()  // unused data (if map passed)

    // Converting to ourselves...
    if (from.getClass == clazz) return Right((from, unused))

    val xm = 
      if (from.isInstanceOf[collection.Map[_,_]]) 
        from.asInstanceOf[collection.Map[Any,Any]]
      else { 
        // convert from object to map so we can do conversion field by field
        toMap(from, manifest[Map[Symbol, Any]], settings) match {
          case Right(r) => r.asInstanceOf[collection.Map[Any,Any]]
          case Left(l) => return Left(l)
        }
      }

    var symUsed = false  // tracks whether symbols or strings used for fields

    // Keep track of unused fields
    var unusedFields = collection.mutable.Set.empty ++ xm.keySet 
    
    // Get field name/value params
    var fieldNames = Vector[String]()  // field names
    var fieldValues = Vector[Any]()    // field values (after conversion) 

    var ctorTypes = Vector[Class[_]]() // based on max contig fields found
    var maxCtorParamsFound = false

    clazz.getDeclaredFields.map { field =>

      // Try to lookup by Symbol, then try String
      val sym = Symbol(field.getName)
      val key = if (symUsed || xm.contains(sym)) sym else field.getName
      if (key == sym) symUsed = true

      val uniformDataOpt = xm.get(key)

      if (uniformDataOpt.isEmpty) {
        if (ctorTypes.size > 0) maxCtorParamsFound = true
      } else {
        val uniformData = uniformDataOpt.get
        val fieldClazz = field.getType

        val value =
          if (isPrimitiveType(fieldClazz)) {
            toPrimitive(uniformData, fieldClazz)
          } else if (isMapType(fieldClazz)) {
            // don't know the types in map, so use manifest Any
            createMapManifest[Any, Any](fieldClazz) match {
              case Right(m) => toMap(uniformData, m, settings)
              case Left(l) => Left(l)
            }
          } else if (isSeqType(fieldClazz)) {
            // don't know the types in seq, so use manifest Any
            createSeqManifest[Any](fieldClazz) match {
              case Right(m) => toSeq(uniformData, m, settings)
              case Left(l) => Left(l)
            }
          } else if (isTupleType(fieldClazz)) {
            // don't know the types in tuple, so use manifest Any
            toTuple(uniformData, manifest[Any], settings)
          } else {
            // Might be embedded unused fields in this obj so handle directly
            toObj(uniformData, fieldClazz, settings) match {
              case Right(r) =>
                // although field itself used, some embedded data may not have
                if (r._2.size > 0) unused += (sym -> r._2)
                Right(r._1)
              case l => l
            }
          }

        value match {
          case Right(r) => 
            if (!maxCtorParamsFound) ctorTypes :+= field.getType
            fieldNames :+= field.getName
            fieldValues :+= r 
            unusedFields -= key

          case Left(l) =>
            if (!settings.bestEffort) return Left(l)
            // do nothing (e.g. ignore...)
        }
      }
    }

    // Find constructors with param counts <= to max ctor params found 
    val ctors = clazz.getDeclaredConstructors.filter {
      ctor => ctor.getParameterTypes.size <= ctorTypes.size 
    }
    if (ctors.isEmpty) return Left("no matching constructor for: " + clazz)

    // Find ctor matching our args
    val ctor = if (ctors.size == 1) ctors(0) else {
      val ctorOpt = ctors.find { ctor => 
        ctor.getParameterTypes.zip(ctorTypes).forall {
          pair => pair._1 == pair._2
        }
      }
      if (ctorOpt.isEmpty) { 
        return Left("no matching constructor for: " + clazz)
      } else {
        ctorOpt.get
      }
    }

    try {
      val numParams = ctor.getParameterTypes.size
      val newObj = ctor.newInstance(
          toRefArray(fieldValues.take(numParams)) : _*)
      
      // Set any fields not used during construction that were passed
      for ((name, value) <- fieldNames.zip(fieldValues).drop(numParams)) {
        // Need to find method with name_$eq
        val method = clazz.getDeclaredMethods.find {
          m => m.getName == (name + "_$eq")
        }
        if (!method.isEmpty) { 
          method.get.invoke(newObj, toRefArray(Seq(value)) : _*)
        } else {
          // re-add to unused fields, it wasn't used
          unusedFields += ( if (symUsed) Symbol(name) else name )
        }
      }

      // Update unused data map
      for (key <- unusedFields) {
        val sym = 
          if (key.isInstanceOf[Symbol]) key.asInstanceOf[Symbol] 
          else Symbol(key.toString)

        unused += (sym -> xm.get(key).get)
      }

      if (settings.opaqueDataSettings.enabled && newObj.isInstanceOf[AnyRef]) {
        OpaqueData.add(newObj.asInstanceOf[AnyRef], unused)(
          settings.opaqueDataSettings)
      }

      Right((newObj, unused))
    } catch {
      case e => Left("object construction failed: " + e.getMessage())
    }
  }

} // end package object
