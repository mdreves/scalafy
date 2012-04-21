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

  val fieldTypes = ReflectionData.getFieldTypes(objType.erasure) 
  var fieldValues = collection.mutable.LinkedHashMap[Symbol,Any]()  

  var unusedData = Map[Symbol,Any]()  // track unused data

  override protected def isContainerUpgradeable(): Boolean = false 
  
  override def getItemType(name: Any): Manifest[_] = {
    val fieldType = fieldTypes.get(name.asInstanceOf[Symbol])
    if (fieldType.isEmpty) {
      // Not a type we know, return Any for now, we will catch the
      // error later when we try to convert to object
      manifest[Any]
    } else {
      toManifest(fieldType.get)
    }
  }

  def getObj(): Either[String, A] = {
    // Find constructors with param counts to our value count
    val ctors = objType.erasure.getDeclaredConstructors.filter {
      ctor => ctor.getParameterTypes.size <= fieldValues.size 
    }
    if (ctors.isEmpty) {
      Left("missing fields: " +
          fieldTypes.keySet.diff(fieldValues.keySet).mkString(", ") + 
          " for " + objType) 
    } else {
      val ctor = if (ctors.size == 1) ctors(0) else {
        // Find ctor matching our args
        val ctorOpt = ctors.find { ctor => 
          val matchingTypes = fieldValues.keys.map { 
            name => fieldTypes.get(name).get
          }
          ctor.getParameterTypes.zip(matchingTypes).forall {
            pair => pair._1 == pair._2
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

        val newObj = ctor.newInstance(
            toRefArray(fieldValues.values.take(numParams)) : _*)
         
        // Set any fields not used during construction that were passed
        for ((name, value) <- fieldValues.drop(numParams)) {
          // Need to find method with name_$eq
          val method = objType.erasure.getDeclaredMethods.find {
            m => m.getName == (name.name + "_$eq")
          }
          if (!method.isEmpty) { 
            method.get.invoke(newObj, toRefArray(Seq(value)) : _*)
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
  }

  override protected def internalAdd(
      name: Any, item: Any): Option[String] = {

    // Validate type 
    val fieldType = fieldTypes.get(name.asInstanceOf[Symbol])
    
    if (fieldType.isEmpty) {
      if (settings.opaqueDataSettings.enabled) {
        unusedData += (name.asInstanceOf[Symbol] -> item)
      }
    } else {
   
      if (item == null) {
        if (!scalafy.util.isNullAllowed(fieldType.get)) {
          return Some("type mismatch: expecting "+fieldType.get + " not null")
        }

      } else if (!fieldType.get.isAssignableFrom(toClass(item))) {
        return Some("type mismatch: expecting " + fieldType.get + 
            " not " + toClass(item))
      }

      // Add to our container
      fieldValues += (name.asInstanceOf[Symbol] -> item)
    }

    None
  }

  /** Creates manifest that goes with class */
  private def toManifest(clazz: Class[_]): Manifest[_] = {
    if (isPrimitiveType(clazz)) toPrimitiveManifest(clazz).get
    else if (isSeqType(clazz)) createSeqManifest[Any](clazz).right.get
    else if (isTupleType(clazz)) 
      createTupleManifest(manifest[Any], getTupleCount(clazz)).right.get
    else if (isMapType(clazz)) createMapManifest(clazz).right.get
    else new Manifest[ReflectionData[_]]() { 
      def erasure = clazz
    }
  }
}

private[parser] object ReflectionData {
  def getFieldTypes(clazz: java.lang.Class[_]) = {
    var fieldTypes = Map[Symbol, java.lang.Class[_]]()
    for (f <- clazz.getDeclaredFields) {
      fieldTypes += (Symbol(f.getName()) -> f.getType()) 
    }
    fieldTypes
  }
}
