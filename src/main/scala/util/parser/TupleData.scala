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

import scalafy.types.reifiable.Reifiable
import scalafy.util._

/** Wrapper for tuples */
private[parser] class TupleData[A](
  tupleType: Manifest[A], 
  settings: ParserSettings
) extends ArrayData[A](tupleType, settings) {

  // Use ListBuffer to store tuple args 
  private var obj = collection.mutable.ListBuffer[Any]()

  override def getItemType(index: Int): Manifest[_] = {
    if (index < objType.typeArguments.size) objType.typeArguments(index)
    else manifest[Any]  // we will deal with this error later
  }

  def getObj(): Either[String, A] = {
    // Now match our args with the parsed data
    if (obj.size != objType.typeArguments.size)
      return Left("wrong number of arguments for tuple: " + 
        obj.size + " != " + objType.typeArguments.size)

    val result = createTuple(obj.toSeq : _*) match {
      case Right(r) => r
      case Left(l) => return Left(l)
    }

    Reifiable(getManifest(), result)(settings.reifiableSettings)
    Right(result.asInstanceOf[A])
  }

  override protected def internalAdd(item: Any): Option[String] = {
    if (obj.size > objType.typeArguments.size - 1) return None
    
    validateType(item, objType.typeArguments(obj.size)) match {
      case Some(v) => return Some(v) // error
      case None => {}
    }

    obj :+= item

    None
  }

  // Do nothing we will check upgradeability at end
  override protected def updateActualType(item: Any) {}

  override def getManifest(): Manifest[_] = {
    // Helper
    def getManifestFor(x: Any) = x match {
      case _: String =>  manifest[String]
      case _: Int => manifest[Int]
      case _: Long => manifest[Long]
      case _: Double => manifest[Double]
      case _: Boolean => manifest[Boolean]
      case o: StructuredData[_] => o.getManifest()
      case _ => manifest[Any]
    }

    // Now upgrade manifest is necessary
    
    var argTypes = Vector[Manifest[_]]()
    var updated = false

    var i = 0
    for (t <- objType.typeArguments) {
      if (t == manifest[Any] || t == manifest[Nothing]) {
        argTypes :+= getManifestFor(obj(i))
        updated = true
      }
      else argTypes :+= t

      i += 1
    }

    if (updated) {
      createTupleManifest(argTypes : _*) match {
        case Right(r) => r
        case Left(l) => objType
      }
    } else objType
  }
}
