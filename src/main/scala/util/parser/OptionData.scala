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

/** Base for encapsulations of Option based data being parsed */
private[parser] class OptionData[A](
  objType: Manifest[A],  // type inside Option 
  settings: ParserSettings
) extends StructuredData[A](objType, settings) {

  private var obj: Any = None 

  /** Gets the item type contained in this container.  */
  def getItemType(): Manifest[_] = objType.typeArguments(0)

  /** Gets Option.
    *
    * @return object or String error message if error occurred
    */
  def getObj(): Either[String, A] = {
    if (obj != None) {
      Reifiable(getManifest(), obj)(settings.reifiableSettings)
    }
    Right(obj.asInstanceOf[A])
  }

  /** Adds value to option
    *
    * NOTE: The Some() wrapper will be added here so don't add items
    *       wrapped in Some unless it is an embedded Option
    *
    * @return None if successful else string error
    */
  def add(item: Any): Option[String] = {
    // Unpack any embedded objects
    val newItem = 
      if (item.isInstanceOf[StructuredData[_]]) { 
        item.asInstanceOf[StructuredData[_]].getObj() match {
          case Right(r) => r
          case Left(l) => return Some(l) // error
        }
      } else item

    if (settings.reifiableSettings.enabled) updateActualType(item)
    obj = Some(newItem)
    None // success
  }

  override protected def createManifestUsing[T : Manifest]: Manifest[_] = {
    manifest[Option[T]] 
  }
}
