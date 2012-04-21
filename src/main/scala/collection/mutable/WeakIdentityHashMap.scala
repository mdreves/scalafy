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
package scalafy.collection.mutable

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder, Map, MapBuilder, MapLike}
import scala.ref.{ReferenceQueue, WeakReference}

/** Combination of WeakHashMap and IdentityHashMap */
class WeakIdentityHashMap[A <: AnyRef, B] extends Map[A, B] 
    with MapLike[A, B, WeakIdentityHashMap[A, B]] { 

  private val cache = Map[WrappedReference, B]()
  private val refQueue = new ReferenceQueue[AnyRef]()

  // Required Map implementations

  def get(key: A): Option[B] = { 
    this.synchronized {
      refCheck()
      cache.get(wrap(key))
    }
  }

  def iterator: Iterator[(A, B)] = {
    this.synchronized {
      refCheck()
      cache.iterator.flatMap { kv => 
        kv._1.get match {
          case Some(key) => Some((key.asInstanceOf[A] -> kv._2))
          case None => None
        }
      }
    }
  }

  def += (kv: (A, B)): WeakIdentityHashMap.this.type = {
    this.synchronized {
      refCheck()
      cache += (wrap(kv._1) -> kv._2)
    }
    this
  }

  def -= (key: A): WeakIdentityHashMap.this.type = {
    this.synchronized {
      refCheck()
      cache -= (wrap(key))
    }
    this
  }


  // Additional overrides

  override def put(key: A, v: B): Option[B] = {
    this.synchronized {
      refCheck()
      cache.put(wrap(key), v)
    }
  }

  override def remove(key: A): Option[B] = {
    this.synchronized {
      refCheck()
      cache.remove(wrap(key))
    }
  }

  override def update(key: A, v: B) {
    this.synchronized {
      refCheck()
      cache.update(wrap(key), v)
    }
  }

  override def clear() = {
    this.synchronized {
      cache.clear()
      refCheck()
    }
  }

  override def size = {
    this.synchronized {
      refCheck()
      cache.size
    }
  }

  override def empty = new WeakIdentityHashMap[A,B]
  
  override def stringPrefix = "WeakIdentityHashMap"


  // Helpers

  private def wrap(x: AnyRef) = new WrappedReference(x, refQueue)

  private def refCheck() {
    var ref = refQueue.poll
    while (!ref.isEmpty) {
      cache.remove(ref.get.asInstanceOf[WrappedReference])
      ref = refQueue.poll
    }
  }
}

object WeakIdentityHashMap {
  def empty[A <: AnyRef, B]: WeakIdentityHashMap[A, B] = 
    new WeakIdentityHashMap[A, B]

  def apply[A <: AnyRef, B](kvs: (A, B)*): WeakIdentityHashMap[A, B] = {
    val xm = new WeakIdentityHashMap[A,B] 
    for (kv <- kvs) xm += kv
    xm
  }

  def unapplySeq[A <: AnyRef, B](
    xm: WeakIdentityHashMap[A, B]
  ): Option[Seq[(A,B)]] = Some(xm.toSeq)  

  def newBuilder[A <: AnyRef, B]: Builder[(A, B), WeakIdentityHashMap[A,B]] =
    new MapBuilder[A, B, WeakIdentityHashMap[A,B]](empty)

  implicit def canBuildFrom[A <: AnyRef, B]: CanBuildFrom[
    WeakIdentityHashMap[_,_], (A, B), WeakIdentityHashMap[A, B]
  ] = 
    new CanBuildFrom[
      WeakIdentityHashMap[_,_], (A, B), WeakIdentityHashMap[A, B]
    ] {
      def apply(from: WeakIdentityHashMap[_,_]) = newBuilder[A, B]
      def apply() = newBuilder[A, B]
    }
}


/////////////////////////////////////////////////////////////////////////////
// Helpers 
/////////////////////////////////////////////////////////////////////////////

private class WrappedReference(
  ref: AnyRef, refQueue: ReferenceQueue[AnyRef]
) extends WeakReference(ref, refQueue) {

  val systemHashCode = System.identityHashCode(ref)

  // NOTE: Don't refer to ref from constructor or we will create a 
  //       Closure and therefore hold a reference to ref preventing its
  //       garbage collection...

  override def equals(that: Any) = {
    if (that == null) false
    else if (that.isInstanceOf[WrappedReference]) {
      that.asInstanceOf[WrappedReference].hashCode == systemHashCode
    } else {
      System.identityHashCode(that) == systemHashCode 
    }
  }

  override def hashCode(): Int = systemHashCode 
}
