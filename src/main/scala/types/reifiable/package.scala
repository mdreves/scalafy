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
package scalafy.types

import scalafy.types.meta.ObjectCache
import scalafy.util._

/** Reifiable types.
  *
  * The following is a summary of features:
  * {{{
  * // Reifiable types
  * def createList(): List[_] = Reifiable(List[Int]())
  *
  * val x = createList()             // x is type List[_] (unknown list type)
  * x.isType[List[String]]           // false
  * x.isType[List[Int]]              // true 
  *  
  * def createMap(): Map[_,_] = Reifiable(Map[String, Boolean]())
  * val x: Any = createMap()      
  * x match {
  *   case xm if (xm.isType[Map[String, Boolean]]) => println("match")
  *   case _ => println("no match")
  * }
  *
  * // Configuring default settings (default is on)
  * implicit val reifiableSettings = ReifiableSettings(false)
  * }}}
  */
package object reifiable {

  ///////////////////////////////////////////////////////////////////////////
  // vals and implicits 
  ///////////////////////////////////////////////////////////////////////////

  /** Settings for enabling/disabling reifiable types */
  case class ReifiableSettings(enabled: Boolean)

  implicit val reifiableSettings = ReifiableSettings(false)


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  /** Reifiable
    *
    * Stores information about the manifest associated with an object
    * This information can then used later during pattern matching.
    * This is intended to be used in cases where a method returns an
    * object that contains type information that would be lost due to 
    * type erasure. Prior to returning the object Reifiable() can be
    * called to store its manifest for later retrieval.
    *
    * Example: 
    * {{{
    * def createList(): List[_] = Reifiable(List[Int]())
    *
    * val x = createList()             // x is type List[_] (unknown list type)
    * x.isType[List[String]]           // false
    * x.isType[List[Int]]              // true 
    *  
    * def createMap(): Map[_,_] = Reifiable(Map[String, Boolean]())
    * val x = createMap()              // x is type Map[_,_]
    * x match {
    *   case xm if (xm.isType[Map[String, Boolean]]) => println("match")
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Reifiable {
    val ReifiableTag = Symbol("__REIFIABLE__")

    def getManifest(x: Any): Option[Manifest[_]] =
      if (x.isInstanceOf[AnyRef]) {
        ObjectCache.get(x.asInstanceOf[AnyRef], ReifiableTag)
          .asInstanceOf[Option[Manifest[_]]]
      } else toPrimitiveManifest(x.getClass)

    def apply[A](x: A)(
      implicit m: Manifest[A], settings: ReifiableSettings
    ): A = {
      if (settings.enabled && x.isInstanceOf[AnyRef]) 
        ObjectCache.put(x.asInstanceOf[AnyRef], ReifiableTag, manifest[A])
      x 
    }

    /** For cases when have a manifest already */
    def apply[A](m: Manifest[_], x: A)(
      implicit settings: ReifiableSettings
    ): A = {
      if (settings.enabled && x.isInstanceOf[AnyRef]) 
        ObjectCache.put(x.asInstanceOf[AnyRef], ReifiableTag, m)
      x 
    }

    def isType(x: Any, m: Manifest[_]): Boolean = {
      if (x.isInstanceOf[AnyRef]) {
        ObjectCache.get(x.asInstanceOf[AnyRef], ReifiableTag) match {
          case Some(v) => (m >:> v.asInstanceOf[Manifest[_]])
          case None => false
        }
      } else {
        m == toPrimitiveManifest(x.getClass)
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  final class ReifiablePimp(val x: Any) {
    def isType[A : Manifest] = Reifiable.isType(x, manifest[A])
  }

  /** Allows us to use x.isType[...] */
  implicit def any2ReifiablePimp(x: Any) = new ReifiablePimp(x) 

} // end package object
