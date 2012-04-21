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

/** Base for encapsulations of name/value based data (Maps, Objects) as parsed. 
  *
  * The types here may be a bit confusing, but A is for the object type
  * (Map[Symbol, Int], ...) and B is for the name type used to lookup values
  * in that object type. For reflection based objects this might be Symbol, 
  * but for Maps this could be Symbol, String, ... (depends on what was
  * requested). In other words, if the object type is Map[Double,Int], the
  * nameType should be Double.
  */
private[parser] abstract class ObjectData[A, B](
  objType: Manifest[A],  // type of object (Map[Symbol,Int], ...)
  val nameType: Manifest[B],  // type to lookup values in obj
  settings: ParserSettings
) extends StructuredData[A](objType, settings) {

  /** Gets the item type contained in this container for given field.  */
  def getItemType(name: Any): Manifest[_]

  def getNameManifest() = nameType

  /** Add - not intended to be overriden (override internalAdd)
    *
    * @return None if successful else string error
    */
  def add(name: Any, item: Any): Option[String] = {
    // Unpack any embedded objects
    val newItem = 
      if (item.isInstanceOf[StructuredData[_]]) { 
        item.asInstanceOf[StructuredData[_]].getObj() match {
          case Right(r) => r
          case Left(l) => return Some(l) // error
        }
      } else item
   
    internalAdd(name, newItem) match {
      case Some(v) => Some(v) 
      case None => 
        if (settings.reifiableSettings.enabled) updateActualType(item)
        None 
    }
  }

  /** Internal add
    *
    * This should be overridden. Subclasses are responsible for validating
    * the item type (the validateType method may be used to help).
    *
    * @return None if successful else string error
    */
  protected def internalAdd(name: Any, item: Any): Option[String] = None 
}
