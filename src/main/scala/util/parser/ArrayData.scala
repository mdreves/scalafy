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

/** Base for encapsulations of array based data (seq,tuples,...) being parsed */
private[parser] abstract class ArrayData[A](
  objType: Manifest[A],  // Type of Object
  settings: ParserSettings
) extends StructuredData[A](objType, settings) {

  /** Gets the item type contained in this container.  */
  def getItemType(index: Int): Manifest[_]

  /** Gets object.
    *
    * This must be overriden by subclasses.
    *
    * NOTE: If a Any was used, then the type returned is still of
    *       type List[Any], etc. Only the manifest is updated to
    *       a track the more specific type so that a safe cast can
    *       be done.
    * 
    * @return object or String error message if error occurred
    */
  def getObj(): Either[String, A]

  /** Add - not intended to be overriden (override internalAdd)
    *
    * Note, this implementation handles upgrading of manifests for array
    * data where all the data is of the same type. If the array data is
    * for multiple types (e.g. tuples, etc) then the updateActualType
    * and getManifest implementations may need to be overridden.
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

    internalAdd(newItem) match {
      case Some(v) => Some(v)  // error
      case None => 
        if (settings.reifiableSettings.enabled) updateActualType(item)
        None
    }
  }

  /** Internal add.
    *
    * This should be overriden. Subclasses are responsible for validating
    * the items type (the validateType method may be used to help).
    * 
    * @return None if successful else string error
    */
  protected def internalAdd(item: Any): Option[String] = None 
}
