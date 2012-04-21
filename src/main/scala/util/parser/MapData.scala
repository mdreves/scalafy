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

/** Wrapper for Map like data */
private[parser] class MapData[A, B](
  mapType: Manifest[A],    // Map[X, Y] 
  mapKeyType: Manifest[B], // X
  settings: ParserSettings
) extends ObjectData[A, B](mapType, mapKeyType, settings) {
  
  private var obj: A = 
    createMap(objType.typeArguments(0), 
        objType.typeArguments(1), objType.erasure) match {
      case Right(r) => r.asInstanceOf[A]
      case Left(l) => throw new Error(l)
    }

  override def getItemType(name: Any): Manifest[_] = 
    objType.typeArguments(1)

  def getObj(): Either[String, A] = {
    Reifiable(getManifest(), obj)(settings.reifiableSettings)
    Right(obj)
  }

  override protected def internalAdd(
      name: Any, item: Any): Option[String] = {
    validateType(item, getItemType(name)) match {
      case Some(v) => return Some(v) // error
      case None => {}
    }

    updateMap(obj, name, item) match {
      case Right(r) => obj = r.asInstanceOf[A]; None
      case Left(l) => Some(l)
    }
  }

  override protected def createManifestUsing[A : Manifest]: Manifest[_] = {
    createMapManifest(objType.erasure)(
        manifest[Symbol], manifest[A]) match {
      case Right(m) => m
      case Left(l) => throw new Error(l)
    }
  }
}
