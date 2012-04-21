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

import scala.ref.{ReferenceQueue, WeakReference}

import scalafy.collection.mutable.WeakIdentityHashMap
import scalafy.collection.uniform._

/** Support for adding meta data to objects.
  *
  * The following is a summary of features:
  * {{{
  * // General meta data
  * val x = List(1,2)
  * x.addMeta('foo, "foo")
  * x.getMeta('foo)            // "foo"
  * x.addMeta('bar, 3)
  * x.removeMeta('bar)         // 3
  * }}}
  */
package object meta {

  ///////////////////////////////////////////////////////////////////////////
  // Settings 
  ///////////////////////////////////////////////////////////////////////////

  /** Settings for enabling/disabling opaque data storage */
  case class OpaqueDataSettings(enabled: Boolean)

  implicit val opaqueDataSettings = OpaqueDataSettings(false)


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  /** Meta 
    *
    * Stores meta data information about an object.
    *
    * WARNING: This inherently has side-effects which typically should be
    *          avoided. 
    *
    * Example: 
    * {{{
    * val x = List(1,2)
    * Meta.add(x, 'foo, "foo")
    * val f = Meta.get(x, 'foo)        // "foo"
    * }}}
    *
    * @author Mike Dreves
    */
  object MetaData {
    val MetaTag = Symbol("__META__")

    def add[A <: AnyRef](x: A, tag: Symbol, data: Any): A = {
      var metaMap = ObjectCache.get(x, MetaTag) match {
        case Some(map) => map.asInstanceOf[Map[Symbol,Any]]
        case None => Map[Symbol, Any]()
      }
      metaMap += (tag -> data)
      ObjectCache.put(x, MetaTag, metaMap)
      x
    }

    def remove[A <: AnyRef](x: A, tag: Symbol): Option[Any] = 
      ObjectCache.get(x, MetaTag) match {
        case Some(map) => 
          val metaMap = map.asInstanceOf[Map[Symbol,Any]]
          metaMap.get(tag) match {
            case Some(data) =>
              val newMetaMap = metaMap - tag
              if (newMetaMap.isEmpty) ObjectCache.remove(x, MetaTag)
              else ObjectCache.put(x, tag, newMetaMap)
              Some(data)
            case None => None
          }
        case None => None
      }

    def get(x: AnyRef, tag: Symbol): Option[Any] = get(x, Seq(tag)) 

    /** Get using Seq of tags into embedded maps of meta data */
    def get(x: AnyRef, tags: Seq[Symbol]): Option[Any] = {
      var tagMap = ObjectCache.get(x, MetaTag) match {
        case Some(m) => m.asInstanceOf[Map[Symbol,Any]]
        case None => return None
      }

      var curTags = tags
      while (curTags.size > 0) {
        val item = 
          if (tagMap.isInstanceOf[scala.collection.Map[_,_]]) {
            tagMap.asInstanceOf[scala.collection.Map[Any,Any]].get(curTags.head)
          } else if (tagMap.isInstanceOf[UniformMap[_]]) {
            tagMap.asInstanceOf[UniformMap[_]].get(curTags.head)
          } else return None

        item match {
          case Some(m) => 
            if (curTags.size == 1) return Some(m)
            else curTags = curTags.tail
          case None => return None
        }
      }
      None
    }

    def apply[A <: AnyRef](x: A, tag: Symbol, data: Any): A = add(x, tag, data)

    def unapply[A <: AnyRef](x: (A, Symbol)): Option[Any] = get(x._1, x._2)
  }


  /** Opaque Data 
    *
    * Stores additional data associated with an object. Opaque data is
    * a special form of meta data that is used to store data left over after
    * a conversion from another type. If an object is later converted back
    * into the original form, then this data can be retrieved.
    *
    * WARNING: This inherently has side-effects which typically should be
    *          avoided. 
    *
    * Example: 
    * {{{
    * case class Foo(s: String)
    * val f = Foo("test")
    * OpaqueData.add(f, Map('i -> 1))
    * val opaque = OpaqueData.get(x)      // Map('i -> 1)
    * }}}
    *
    * @author Mike Dreves
    */
  object OpaqueData {
    val OpaqueTag = Symbol("__OPAQUE__")

    def add[A <: AnyRef](x: A, data: Map[Symbol, Any])(
      implicit settings: OpaqueDataSettings
    ): A = {
      if (settings.enabled) ObjectCache.put(x, OpaqueTag, data)
      x 
    }

    def remove(x: AnyRef): Option[Map[Symbol, Any]] = 
      ObjectCache.remove(x, OpaqueTag).asInstanceOf[Option[Map[Symbol,Any]]]

    def get(x: AnyRef): Option[Map[Symbol, Any]] = 
      ObjectCache.get(x, OpaqueTag).asInstanceOf[Option[Map[Symbol,Any]]]

    def apply[A <: AnyRef](x: A, data: Map[Symbol,Any])(
      implicit settings: OpaqueDataSettings
    ): A = add(x, data) 

    def unapply[A <: AnyRef](x: A): Option[Map[Symbol,Any]] = get(x)
  } 


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  final class MetaDataPimp[A <: AnyRef](val x: A) {
    def addMeta(tag: Symbol, data: Any): A = MetaData.add(x, tag, data)
    def removeMeta(tag: Symbol): Option[Any] = MetaData.remove(x, tag)
    def getMeta(tag: Symbol): Option[Any] = MetaData.get(x, tag)
    def getMeta(tags: Seq[Symbol]): Option[Any] = MetaData.get(x, tags)
  }

  implicit def any2MetaDataPimp[A <: AnyRef](x: A) = new MetaDataPimp(x) 


  ///////////////////////////////////////////////////////////////////////////
  // Helpers 
  ///////////////////////////////////////////////////////////////////////////

  object ObjectCache {

    private val cache = WeakIdentityHashMap[AnyRef, Map[Symbol, Any]]() 

    def get(x: AnyRef, tag: Symbol): Option[Any] = {
      cache.get(x) match {
        case Some(map) => map.get(tag)
        case None => None
      }
    }

    def put(x: AnyRef, tag: Symbol, data: Any): Unit = {
      cache.get(x) match {
        case Some(map) => 
          cache += (x -> (map + (tag -> data)))
        case None => 
          cache += (x -> Map(tag -> data))
      }
    }

    def remove(x: AnyRef, tag: Symbol): Option[Any] = {
      cache.get(x) match {
        case Some(map) =>
          val curValue = map.get(tag)
          val result = map - tag
          if (result.isEmpty) cache -= x
          else cache += (x -> result)
          curValue 
        case None => None
      }
    }

    def size: Int = cache.size 
  }

} // end package object
