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
package scalafy.collection.uniform

import java.util.Date
import java.util.TimeZone

import scalafy.types.meta._
import scalafy.util._
import scalafy.util.converters._

/** Uniform data parser */
private[uniform] object UniformDataParser {
  private val errorPrefix = "UniformData parse error :: "

  /** Parser for fromUniformData package function
    *
    * @return either String error msg or data of given type
    */
  def fromUniformData[A : Manifest](
    u: UniformData[_], settings: UniformDataSettings
  ): Either[String, A] = {
  
    if (isMapType(manifest[A])) {
      if (!u.isInstanceOf[UniformMap[_]]) {
        return Left(errorPrefix + "conversion from " + u.getClass() + 
          " to " + manifest[A] + " not possible")
      }

      val xm = u.asInstanceOf[UniformMap[_]]

      if (manifest[A] == manifest[Map[UniformMapKey, Any]]) {
        Right(xm.value.asInstanceOf[A]) // no convesion needed
      } else toMap(xm.value, manifest[A], settings.conversionSettings) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left(errorPrefix + l)
      }

    } else if (isSeqType(manifest[A])) {
      val xs = 
        if (u.isInstanceOf[UniformList[_]])
          u.asInstanceOf[UniformList[_]].value
        else if (u.isInstanceOf[UniformMap[_]])
          u.asInstanceOf[UniformMap[_]].value.toSeq
        else if (u.isInstanceOf[UniformPrimitive[_]])
          Seq(u.asInstanceOf[UniformPrimitive[_]].value)
        else
          return Left(errorPrefix + "conversion from " + u.getClass() + 
            " to " + manifest[A] + " not possible")

      if (manifest[A] == manifest[List[Any]]) {
        Right(xs.asInstanceOf[A]) // no conversion needed
      } else toSeq(xs, manifest[A], settings.conversionSettings) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left(errorPrefix + l)
      }

    // Tuples
    } else if (isTupleType(manifest[A])) {

      if (!u.isInstanceOf[UniformList[_]]) {
        return Left(errorPrefix + "conversion from " + u.getClass() + 
          " to " + manifest[A] + " not possible")
      }

      val xs = u.asInstanceOf[UniformList[_]]

      toTuple(xs.value.toSeq, manifest[A], settings.conversionSettings) match {
        case Right(r) => Right(r.asInstanceOf[A])
        case Left(l) => Left(errorPrefix + l)
      }
     
    } else if (isPrimitiveType(manifest[A])) {

      u match {
        // primitives
        case v: UniformString if (manifest[A] == manifest[String]) => 
          Right(v.value.asInstanceOf[A])
        case v: UniformSymbol if (manifest[A] == manifest[Symbol]) => 
          Right(v.value.asInstanceOf[A])
        case v: UniformShort if (manifest[A] == manifest[Short]) => 
          Right(v.value.asInstanceOf[A])
        case v: UniformInt if (manifest[A] == manifest[Int]) => 
          Right(v.value.asInstanceOf[A])
        case v: UniformLong if (manifest[A] == manifest[Long]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformFloat if (manifest[A] == manifest[Float]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformDouble if (manifest[A] == manifest[Double]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformBoolean if (manifest[A] == manifest[Boolean]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformChar if (manifest[A] == manifest[Char]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformByte if (manifest[A] == manifest[Byte]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformBigInt if (manifest[A] == manifest[BigInt]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformBigDecimal if (manifest[A] == manifest[BigDecimal]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformDate if (manifest[A] == manifest[Date]) =>
          Right(v.value.asInstanceOf[A])
        case v: UniformTimeZone if (manifest[A] == manifest[TimeZone]) =>
          Right(v.value.asInstanceOf[A])
        case _ => 
          Left(errorPrefix + "conversion from " + u.getClass() + 
            " to " + manifest[A] + "not possible")
      }

    } else {

      // Object conversion (only supported from UniformMap)

      if (!u.isInstanceOf[UniformMap[_]]) {
        return Left(errorPrefix + "conversion from " + u.getClass() + 
          " to " + manifest[A] + " not possible")
      }

      val xm = u.asInstanceOf[UniformMap[_]]
      toObj(xm.value, manifest[A], settings.conversionSettings) match {
        case Right(r) =>
          // Update opaque data cache if unused data found
          if (r._2.size > 0 && r._1.isInstanceOf[AnyRef] &&
              settings.conversionSettings.opaqueDataSettings.enabled) {
            OpaqueData.add(r._1.asInstanceOf[AnyRef], r._2)(
              settings.conversionSettings.opaqueDataSettings)
          }
          Right(r._1.asInstanceOf[A])

        case Left(l) => Left(errorPrefix + l)
      }
    }
  }

  /** Parser for toUniformData package function */
  def toUniformData(
    x: Any, settings:  UniformDataSettings
  ): UniformData[Any] = x match {
    case ud: UniformData[_] => ud.asInstanceOf[UniformData[Any]]
    case s: String => UniformString(s) 
    case s: Symbol => UniformSymbol(s) 
    case s: Short => UniformShort(s) 
    case i: Int => UniformInt(i)
    case l: Long => UniformLong(l)
    case f: Float => UniformFloat(f)
    case d: Double => UniformDouble(d)
    case b: Boolean => UniformBoolean(b)
    case c: Char => UniformChar(c)
    case b: Byte => UniformByte(b)
    case bi: BigInt => UniformBigInt(bi)
    case bd: BigDecimal => UniformBigDecimal(bd)
    case d: Date => UniformDate(d)
    case tz: TimeZone => UniformTimeZone(tz)
    case x => 
      val value = toUniformDataValue(x, settings)
     
      if (value.isInstanceOf[List[_]]) {
        val xs = value.asInstanceOf[List[Any]] 
        new UniformList[Any] { val value = xs }
      } else {
        val xm = value.asInstanceOf[Map[UniformMapKey, Any]]

        val opaque = 
          if (x.isInstanceOf[AnyRef]) OpaqueData.get(x.asInstanceOf[AnyRef])
          else None

        if (!settings.conversionSettings.opaqueDataSettings.enabled ||
            opaque.isEmpty) {
          new UniformMap[Any] { val value = xm }
        } else {
          val mapValue = toUniformMap(opaque.get)(settings) match {
            case Some(m) => m.toMap ++ xm
            case None => xm
          }
          new UniformMap[Any] { val value = mapValue }
        }
      }
  }

  /** Same as toUniformData only primitives are not wrapped */
  private[uniform] def toUniformDataValue(
    x: Any, settings: UniformDataSettings
  ): Any = x match {
    case up: UniformPrimitive[_] => up.value
    case ul: UniformList[_] => ul.value
    case um: UniformMap[_] => um.value
    case s: String => s
    case s: Symbol => s
    case s: Short => s
    case i: Int => i
    case l: Long => l
    case f: Float => f
    case d: Double => d
    case b: Boolean => b
    case c: Char => c
    case b: Byte => b
    case bi: BigInt => bi
    case bd: BigDecimal => bd
    case d: Date => d
    case tz: TimeZone => tz 
    case x =>

      // Map
      if (isMapType(x.getClass)) {

        var xm = Map[UniformMapKey, Any]()

        for ((k,v) <- x.asInstanceOf[Iterable[_]].iterator) {
          xm += (toMapKey(k) -> toUniformDataValue(v, settings))
        }

        xm

      // List
      } else if (isSeqType(x.getClass)) {
        
        var xs = scala.collection.mutable.ListBuffer[Any]()

        for (v <- x.asInstanceOf[Iterable[_]].iterator) {
          xs += toUniformDataValue(v, settings)
        }

        xs.toList

      // Tuples
      } else if (isTupleType(x.getClass)) {
        
         
        var xs = scala.collection.mutable.ListBuffer[Any]()

        for (v <- x.asInstanceOf[Product].productIterator) {
          xs += toUniformDataValue(v, settings)
        }

        xs.toList

      // Create Uniform Map via object using reflection
      } else {

        var xm = Map[UniformMapKey, Any]()
        val clazz = x.getClass

        // append any opaque data
        var opaque: Map[Symbol,Any] = 
          if (x.isInstanceOf[AnyRef]) {
            OpaqueData.get(x.asInstanceOf[AnyRef]) match {
              case Some(v) => v
              case None => null 
            }
          } else null

        clazz.getDeclaredFields.map { field =>

          // Find  method with same name
          try {
            // Scala bug, private triat members are returned as public...
            // We will use __xxx__ to flag them
            if (!field.getName.contains("__")) {
              xm += (toMapKey(field.getName) -> toUniformDataValue(
                  clazz.getMethod(field.getName).invoke(x), settings))
              if (opaque != null) opaque -= Symbol(field.getName)
            }
          } catch {
            case e: NoSuchMethodException => {}
          }
        }

        // If opaque data available, re-add it
        if (settings.conversionSettings.opaqueDataSettings.enabled && 
            opaque != null) {
          toUniformMap(opaque)(settings) match {
            case Some(m) => m.toMap ++ xm
            case None => xm
          }
        } else {
          xm
        }
      }
  }

  private[uniform] def toMapKey(key: Any): UniformMapKey = {
    if (manifest[UniformMapKey] == manifest[Symbol]) {
      if (key.getClass == classOf[Symbol]) key.asInstanceOf[UniformMapKey]
      else Symbol(key.toString).asInstanceOf[UniformMapKey]
    } else if (manifest[UniformMapKey] == manifest[String]) {
      if (key.getClass == classOf[Symbol])
        key.asInstanceOf[Symbol].name.asInstanceOf[UniformMapKey]
      else key.toString.asInstanceOf[UniformMapKey]
    } else {
      throw new Error("key of type " + 
        manifest[UniformMapKey] + " not implemented")
    }
  }

}
