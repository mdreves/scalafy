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

import scalafy.collection.uniform._
import scalafy.types.reifiable.Reifiable

/** Wrapper for UniformMap data */
private[parser] class UniformMapData[A](
  mapType: Manifest[A],
  settings: ParserSettings
) extends ObjectData[A, Symbol](
  if (mapType == manifest[UniformData[Any]]) 
    manifest[UniformMap[Any]].asInstanceOf[Manifest[A]] 
  else mapType, 
  manifest[Symbol], settings
) {
  
  private var obj = UniformMap[Any]()

  override def getItemType(name: Any): Manifest[_] =
    if (objType.typeArguments(0) == manifest[Any]) manifest[UniformData[Any]]
    else objType.typeArguments(0)

  def getObj(): Either[String, A] = {
    Reifiable(getManifest(), obj)(settings.reifiableSettings)
    Right(obj.asInstanceOf[A])
  }

  override protected def internalAdd(name: Any, item: Any): Option[String] = {
    item match {
      case x: String => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Symbol => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Short => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Int => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Long => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Float => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Double => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Boolean => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Char => obj += (name.asInstanceOf[Symbol] -> x)
      case x: Byte => obj += (name.asInstanceOf[Symbol] -> x)
      case x: UniformData[_] => 
        obj += (name.asInstanceOf[Symbol] -> x.asInstanceOf[UniformData[Any]])
      case x => 
        return Some("type mismatch: expecting uniform data not " + x.getClass)
    }
    
    None
  }

  override protected def createManifestUsing[T : Manifest]: Manifest[_] =
    manifest[UniformMap[T]]
}
