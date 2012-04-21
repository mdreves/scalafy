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

import scala.collection.generic.{CanBuildFrom, MutableMapFactory}
import scala.collection.mutable.{Builder, Map, MapBuilder, MapLike}

/** Scala wrapper around Java's IdentityHashMap */
class IdentityHashMap[A, B] extends Map[A, B] 
    with MapLike[A, B, IdentityHashMap[A, B]] {

  val underlying = new java.util.IdentityHashMap[A, B]


  // Required Map implementations

  def get(key: A) = {
    val v = underlying.get(key)
    if (v != null) Some(v)
    else if (underlying.containsKey(key)) Some(null.asInstanceOf[B])
    else None
  }

  def iterator: Iterator[(A, B)] = new Iterator[(A, B)] {
    val iter = underlying.entrySet.iterator

    def hasNext = iter.hasNext

    def next() = { 
      val item = iter.next()
      (item.getKey.asInstanceOf[A] -> item.getValue.asInstanceOf[B]) 
    }
  }

  def += (kv: (A, B)): IdentityHashMap.this.type = {
    underlying.put(kv._1, kv._2)
    this
  }

  def -= (key: A): IdentityHashMap.this.type = {
    underlying.remove(key)
    this
  }


  // Additional overrides

  override def put(key: A, v: B): Option[B] = {
    val result = underlying.put(key, v)
    if (result != null) Some(result) else None
  }

  override def remove(key: A): Option[B] = {
    val result = underlying.remove(key)
    if (result != null) Some(result) else None
  }

  override def update(key: A, v: B) { 
    underlying.put(key, v) 
  }

  override def clear() = underlying.clear()

  override def empty: IdentityHashMap[A,B] = new IdentityHashMap[A, B]

  override def size = underlying.size

  override def stringPrefix = "IdentityHashMap"
}

object IdentityHashMap extends MutableMapFactory[IdentityHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[
    Coll, (A, B), IdentityHashMap[A, B]
  ] = new MapCanBuildFrom[A, B]

  def empty[A, B]: IdentityHashMap[A, B] = new IdentityHashMap[A, B]
}
