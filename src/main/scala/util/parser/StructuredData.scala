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

/** Trait for encapsulations of structured data (arrays, objects) being parsed
 *
 * Subclasses should encapsulate not only the data parsed, but also  
 * information about the manifest that was requested to be matched AND the
 * manifest for the types actually read. If the requested manifest is set to
 * Any then the parser will automatically choose the best type based on the
 * data being read. By maintaining manifest information for what was actually
 * read in addition to what was requested, we can supported upgrading the 
 * the manifest from Any to a more specific type for use with reifiable types.
 */
private[parser] abstract class StructuredData[A]( 
  val objType: Manifest[A],  // type being parsed (List, Vector, Tuple, ...)
  val settings: ParserSettings
) {

  private var actualItemType: Manifest[_] = null // actual item type used

  /** Returns true if this container is upgradeable to more specific type */
  protected def isContainerUpgradeable(): Boolean = true 
  
  /** Gets object.
    *
    * NOTE: If A is set to Any, then the type returned is will be of type
    *       List[Any], etc. Only the manifest is updated to a track the more
    *       specific type. Any upcasts are done later on.
    * 
    * @return object or String error message if error occurred
    */
  def getObj(): Either[String, A]

  /** Manifest of actual data stored.
    * 
    * NOTE: This will only be different than the object's type if
    *       Any was used for a type parameter AND a better type was determined.
    */
  def getManifest(): Manifest[_] = {
    if (!settings.reifiableSettings.enabled || !isContainerUpgradeable() ||
        actualItemType == manifest[Any]) {
      // We were given a specific type or Any is as good as it gets
      objType
    } else {
      // Upgrade from Any to more specific type
      createManifestUsing(actualItemType)
    }
  }

  /** Creates a new manifest using the given inner type - override
    * 
    * This is used when upgrading from Any to a more specific type and should
    * be overridden if isContainerUpgradeable is set to true.
    */
  protected def createManifestUsing[T : Manifest]: Manifest[_] = {
     throw new Error("Not supported") 
  }

  /** Updates actual type to what was read. */
  protected def updateActualType(item: Any): Unit = item match {
    // We only parse either String, Int, Long, Double, or Boolean
    case _: String =>
      if (actualItemType == null) 
        actualItemType = manifest[String]
      else if (actualItemType != manifest[String]) 
        actualItemType = manifest[Any]
    case _: Int => 
      if (actualItemType == null) 
        actualItemType = manifest[Int]
      else if (actualItemType == manifest[Long])
        actualItemType = manifest[Long]
      else if (actualItemType != manifest[Int]) 
        actualItemType = manifest[Any]
    case _: Long =>
      if (actualItemType == null) 
        actualItemType = manifest[Long]
      else if (actualItemType == manifest[Int])
        actualItemType = manifest[Long]
      else if (actualItemType != manifest[Long])
        actualItemType = manifest[Any]
    case _: Double => 
      if (actualItemType == null) 
        actualItemType = manifest[Double]
      else if (actualItemType != manifest[Double]) 
        actualItemType = manifest[Any]
    case _: Boolean => 
      if (actualItemType == null) 
        actualItemType = manifest[Boolean]
      else if (actualItemType != manifest[Boolean]) 
        actualItemType = manifest[Any]
    case o: StructuredData[_] =>
      if (actualItemType == null)
        actualItemType = o.getManifest()
      else if (actualItemType != o.getManifest())
        actualItemType = manifest[Any]
    case x =>
      // At this point the validation checks have passed so 
      // we will only get types that are valid or don't match
      // the above. Although an valid type might be more specific 
      // the actual type no longer matters so set to Any 
      // (if null wait for a more specific type)
      if (x != null) actualItemType = manifest[Any]
  }

  /** Helper method to validate item type against manifest for type.
    *
    * @return None if successfull or String if error
    */
  protected def validateType[A](x: A, m: Manifest[_]): Option[String] = {
    if (m == manifest[Any] || m == manifest[Nothing]) None // check early
    else if (x == null) {  // Check null
      if (!isNullAllowed(m)) 
        Some("type mismatch: expecting " + m + " not null")
      else None
    }
    else if (x.isInstanceOf[String]) {
      // Special case for Chars since Char data passed as Strings
      if (m == manifest[Char] && x.asInstanceOf[String].length != 1)
        Some("type mismatch: expecting Char not String") 
      else if (m != manifest[String]) 
        Some("type mismatch: expecting " + m + " not String")
      else None
    }
    else if (x.isInstanceOf[Symbol] && m != manifest[Symbol]) 
      Some("type mismatch: expecting " + m + " not Symbol") 
    else if (x.isInstanceOf[Short] && m != manifest[Short]) 
      Some("type mismatch: expecting " + m + " not Short") 
    else if (x.isInstanceOf[Int] && m != manifest[Int]) 
      Some("type mismatch: expecting " + m + " not Int") 
    else if (x.isInstanceOf[Long] && m != manifest[Long]) 
      Some("type mismatch: expecting " + m + " not Long") 
    else if (x.isInstanceOf[Float] && m != manifest[Float]) 
      Some("type mismatch: expecting " + m + " not Float") 
    else if (x.isInstanceOf[Double] && m != manifest[Double]) 
      Some("type mismatch: expecting " + m + " not Double") 
    else if (x.isInstanceOf[Boolean] && m != manifest[Boolean]) 
      Some("type mismatch: expecting " + m + " not Boolean")
    else if (x.isInstanceOf[StructuredData[_]]) {
      val o = x.asInstanceOf[StructuredData[_]]
      // Object in Object 
      if (recursiveCompare(o.objType, m)) None
      else Some("type mismatch: expecting "+ o.objType + " not " + m)
    }
    else None
  }

  // Helper function to recursively compare manifests
  protected def recursiveCompare(
      a: Manifest[_], b: Manifest[_]): Boolean = {
    if (a.typeArguments.size != b.typeArguments.size) return false

    // A must be able to hold B
    if (!a.erasure.isAssignableFrom(b.erasure)) return false

    if (a.typeArguments.size == 0) return true // at end
    else if (a.typeArguments.size == 1) {  // containers of one item
      recursiveCompare(a.typeArguments(0), b.typeArguments(0)) 
    } else if (a.typeArguments.size == 2) {
      // The only two element containers we know are Maps where first
      // param type is Symbol, so just use second param type
      recursiveCompare(a.typeArguments(1), b.typeArguments(1))
    } else false  // Don't know any types...
  }

  /** Return true if type allowed to be null */
  protected def isNullAllowed(m: Manifest[_]): Boolean = 
    m <:< manifest[AnyRef] 
}
