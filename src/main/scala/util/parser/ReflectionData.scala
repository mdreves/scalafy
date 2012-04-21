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

import scalafy.types.meta._
import scalafy.util._

/** Wrapper for Object read from reflection */
private[parser] class ReflectionData[A](
  reflectionType: Manifest[A],
  settings: ParserSettings
) extends ObjectData[A, Symbol](reflectionType, manifest[Symbol], settings) {

  var fieldValues = 
    new Array[AnyRef](ReflectionData.getFieldCount(getClass(objType), settings))

  var unusedData = Map[Symbol,Any]()  // track unused data

  override protected def isContainerUpgradeable(): Boolean = false 
  
  override def getItemType(name: Any): Manifest[_] = {
    ReflectionData.getFieldType(
        getClass(objType), name.asInstanceOf[Symbol], settings) match {
      case Some(mf) => mf   // may be null if typeHints needed 
      case None => manifest[Any] // not a type we know, deal with error at toObj
    }
  }

  def getObj(): Either[String, A] = {
    // Fast path:
    //   The assumption is that 99% of the objects will have a single
    //   constructor matching the paramters. We will just blindly attempt
    //   to do the conversion with what we have. If it throws an  
    //   exception, then take slow path
    ReflectionData.getSingleConstructor(getClass(objType)) match {
      case Some(ctor) => try {
        val newObj = ctor.newInstance(fieldValues: _*).asInstanceOf[A]
      
        if (settings.opaqueDataSettings.enabled && unusedData.size > 0 &&
            newObj.isInstanceOf[AnyRef]) {
          OpaqueData.add(newObj.asInstanceOf[AnyRef], unusedData)(
            settings.opaqueDataSettings)
        }

        return Right(newObj)
      } catch {
        case e => {} // go to slow path
      }

      case None => {} // go to slow path
    }

    // Slow path:

    // Create list of actual fields, types, and values used 
    val fieldNames = ReflectionData.getFieldNames(getClass(objType), settings)
    var fieldsUsed = collection.mutable.ArrayBuffer[Symbol]()
    var typesUsed = collection.mutable.ArrayBuffer[Class[_]]()
    var valuesUsed = collection.mutable.ArrayBuffer[AnyRef]()
    for (i <- 0 until fieldValues.size) {
      val t = ReflectionData.getFieldType(
          getClass(objType), fieldNames(i), settings).get
      val v = fieldValues(i)
      if (v == null) {
        if (t.erasure == classOf[Option[_]]) {
          fieldsUsed += fieldNames(i)
          typesUsed += t.erasure
          valuesUsed += None
        }
      } else {
        fieldsUsed += fieldNames(i)
        typesUsed += t.erasure
        valuesUsed += v
      }
    }

    // Find constructors with param counts less than or equal to value count
    val ctors = getClass(objType).getDeclaredConstructors.filter {
      ctor => ctor.getParameterTypes.size <= valuesUsed.size 
    }

    if (ctors.isEmpty) {
      val missing = ReflectionData.getFieldNames(
          getClass(objType), settings).filter { f =>
        fieldValues(ReflectionData.getFieldOffset(
            getClass(objType), f, settings)) != null 
      }
      return Left("missing fields: " + missing.mkString(", ") + 
        " for " + objType) 
    }

    val ctor = if (ctors.size == 1) ctors(0) else {
      // Find ctor matching our args
      val ctorOpt = ctors.find { ctor => 
        ctor.getParameterTypes.zip(typesUsed) forall { pair =>
          pair._1 == pair._2
        }
      }
      if (ctorOpt.isEmpty) {
        return Left("no matching constructor for: " + objType)
      } else {
        ctorOpt.get
      }
    }

    try {
      val numParams = ctor.getParameterTypes.size

      val newObj = ctor.newInstance(valuesUsed.take(numParams) : _*)
       
      // Set any fields not used during construction that were passed
      for (i <- numParams until valuesUsed.size) {
        // Need to find method with name_$eq
        val method = getClass(objType).getDeclaredMethods.find {
          m => m.getName == (fieldsUsed(i).name + "_$eq")
        }

        if (!method.isEmpty) { 
          method.get.invoke(newObj, Seq(valuesUsed(i)) : _*)
        }
      }

      if (settings.opaqueDataSettings.enabled && unusedData.size > 0 &&
          newObj.isInstanceOf[AnyRef]) {
        OpaqueData.add(newObj.asInstanceOf[AnyRef], unusedData)(
          settings.opaqueDataSettings)
      }

      Right(newObj.asInstanceOf[A])
    } catch {
      case e => Left("object construction failed: " + e.getMessage())
    }
  }

  override protected def internalAdd(
      name: Any, item: Any): Option[String] = {

    val sym = name.asInstanceOf[Symbol]

    // Validate type 
    ReflectionData.getFieldType(getClass(objType), sym, settings) match {
      case None =>
        if (settings.opaqueDataSettings.enabled) unusedData += (sym -> item)
  
      case Some(t) =>
        if (item == null) {
          if (!scalafy.util.isNullAllowed(t.erasure)) {
            return Some("type mismatch: expecting "+ t + " not null")
          }

        } else if (!t.erasure.isAssignableFrom(toClass(item))) {
          return Some("type mismatch: expecting " + t + " not " + toClass(item))
        }

        // Add to our container
        val offset = ReflectionData.getFieldOffset(
            getClass(objType), sym, settings)
        fieldValues(offset) = toRef(item)
    }

    None
  }

  protected def getClass(mf: Manifest[_]): Class[_] = {
    if (settings.classLoader == null) mf.erasure
    else settings.classLoader.loadClass(mf.erasure.getName)
  }
}

private[parser] object ReflectionData {

  private val cache = new collection.mutable.WeakHashMap[
      Class[_], Map[Symbol, (Int, Manifest[_])]]()

  def getFieldCount(clazz: Class[_], settings: ParserSettings): Int = 
    getFieldMapping(clazz, settings).size

  def getFieldOffset(
    clazz: Class[_], field: Symbol, settings: ParserSettings
  ): Int = {
    getFieldMapping(clazz, settings).get(field) match {
      case Some(pair) => pair._1
      case None => -1
    }
  }

  def getFieldType(
    clazz: Class[_], field: Symbol, settings: ParserSettings
  ): Option[Manifest[_]] = {
    getFieldMapping(clazz, settings).get(field) match {
      case Some(pair) => Some(pair._2)
      case None => None 
    }
  }

  def getFieldNames(
    clazz: Class[_], settings: ParserSettings
  ): Array[Symbol] = {
    val mapping = getFieldMapping(clazz, settings)
    val result = new Array[Symbol](mapping.size)
    mapping.foreach { kv => result(kv._2._1) = kv._1 }
    result
  }

  /** Gets single constructor for class
    *
    * If one constructor exists that is returned, if an empty constructor
    * along with only one other consturctor exists the other constructor
    * is returned else None is returned.
    */
  def getSingleConstructor(
    clazz: Class[_]
  ): Option[java.lang.reflect.Constructor[_]] = {
    val ctors = clazz.getDeclaredConstructors
    if (ctors.size == 1) Some(ctors(0)) 
    else if (ctors.size == 2) {
      if (ctors(0).getParameterTypes.size == 0) Some(ctors(1))
      else if (ctors(1).getParameterTypes.size == 0) Some(ctors(0))
      else None
    } else None
  }

  private def getFieldMapping(
    clazz: Class[_], settings: ParserSettings
  ): Map[Symbol, (Int, Manifest[_])] = {
    cache.get(clazz) match {
      case Some(fieldMapping) => fieldMapping 
      case None => 
        var fieldMapping = Map[Symbol, (Int, Manifest[_])]()
        var offset = 0
        for (f <- clazz.getDeclaredFields) {
          val sym = Symbol(f.getName())
          toManifest(clazz, f.getType(), sym, settings) match {
            case Some(mf) =>
              fieldMapping += (sym -> (offset -> mf))
            case None => // Missing information (need typeHints)
              fieldMapping += (sym -> (offset -> null))
          }
          offset += 1
        }
        cache.put(clazz, fieldMapping)
        fieldMapping
    }
  }

  /** Creates manifest that goes with class */
  private def toManifest(
    parent: Class[_], clazz: Class[_], sym: Symbol, settings: ParserSettings
  ): Option[Manifest[_]] = {

    // Helper to create manifest given just class
    def createManifest() = {
      // If no type params, then clazz may be enough do instaniate
      // (assuming a concrete class used)
      new Manifest[ReflectionData[_]]() { 
        def erasure = clazz
      }
    }

    if (isPrimitiveType(clazz)) toPrimitiveManifest(clazz)
    else settings.typeHintSettings.classes.get(parent) match {
      case Some(map) => map.get(sym) match {
        case Some(mf) => Some(mf)
        case None => 
          if (clazz.getTypeParameters.size == 0) Some(createManifest())
          else None
      }
      case None => 
        if (clazz.getTypeParameters.size == 0) Some(createManifest())
        else None
    }
  }
}
