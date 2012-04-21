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

/** Wrapper for UniformList data */
private[parser] class UniformListData[A](
  listType: Manifest[A], 
  settings: ParserSettings
) extends ArrayData[A](
  if (listType == manifest[UniformData[Any]]) 
    manifest[UniformList[Any]].asInstanceOf[Manifest[A]] 
  else listType, 
  settings
) {

  private var obj = UniformListBuffer[Any]() 

  override def getItemType(index: Int): Manifest[_] = 
    if (objType.typeArguments(0) == manifest[Any]) manifest[UniformData[Any]]
    else objType.typeArguments(0) 

  def getObj(): Either[String, A] = {
    val result = obj.toUniformList
    Reifiable(getManifest(), result)(settings.reifiableSettings)
    Right(result.asInstanceOf[A])
  }

  override protected def internalAdd(item: Any): Option[String] = {
    item match {
      case x: String => obj += x
      case x: Symbol => obj += x
      case x: Short => obj += x
      case x: Int => obj += x
      case x: Long => obj += x
      case x: Float => obj += x
      case x: Double => obj += x
      case x: Boolean => obj += x
      case x: Char => obj += x
      case x: Byte => obj += x
      case x: UniformData[_] => obj += x.asInstanceOf[UniformData[Any]]
      case x => 
        return Some("type mismatch: expecting uniform data not " + x.getClass)
    }
    
    None
  }

  override protected def createManifestUsing[T : Manifest]: Manifest[_] =
    manifest[UniformList[T]]
}
