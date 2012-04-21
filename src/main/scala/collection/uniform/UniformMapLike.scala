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
package scalafy.collection.uniform

import scalafy.util.toClass

/** Wrapped implementation of Map[A,B] that only allows uniform types.
  *
  * The createMap method needs to be overridden by the subclass in order to
  * build the UniformMap[B] type.  Note that the 'A' type parameter is not
  * used in Map creation. This is because only the 'B' type is checked to
  * confrom to be a UnifiedType.  The 'A' is only provided as a parameter to
  * this trait so that it will be easy to switch between String vs Symbol
  * based keys in the future if desired.
  */
private[uniform] trait UniformMapLike[A, B] {

  // Wrapped container
  private[uniform] val value: Map[A, B]

  private[uniform] var defaultValueFn: (Seq[A]) => Any = null


  /** Subclass must override */
  private[uniform] def createMap[B1](xm: Map[A, B1]): UniformMap[B1] 
 
  /** Subclass must override */
  private[uniform] def createIterator[B1](
    i: Iterator[Map[A, B1]]
  ): Iterator[UniformMap[B1]]


  // What would have been standard Map overrides 

  // NOTE: It would be nice to have the ability to return a Map of our own
  //       type instead of UniformMap[Any], but no feasible solution was
  //       determined. The following were tried:
  //       1) Create separate functions for our specific type
  // 
  //            def + (kv: (A, B)): UniformMap[B] = createMap(value + kv)
  //            def + [B : UniformType](kv: (A, B)): UniformMap[Any] = ... 
  //
  //          This works for specific maps (UniformMap[String], etc) but
  //          does not work with UniformMap[Any] where the returns collide
  //
  //       2) Use the pattern of creating ambiguity to restrict the
  //          methods to only those that are not Any.
  //
  //          Although this compiles, the compiler doesn't use the extra
  //          information to distinguish which method to use and still
  //          complains
  // 
  //       3) Create a separate trait that is only mixed in when the
  //          the types are not Any types.
  //        
  //          Without creating a separate hierarchy for UniformMap itself
  //          this doesn't work because the static type is just UniformMap
  //          without the mixin so it doesn't pick up the new methods
  def + [B1 : UniformType](kv: (A, B1)): UniformMap[Any] =
    createMap(value.asInstanceOf[Map[A,Any]] + normalize(kv)(0))

  def - (key: A): UniformMap[B] = createMap(value - key)

  def get(key: A): Option[B] = value.get(key)

  def iterator: Iterator[(A, B)] = value.iterator


  // These are all the methods that would be implemented by Map[A, B] if
  // mixing in that trait were possible. These methods return UniformMap[B]
  // whenever a Map would have been returned. In addition, wherever possible
  // the context bound UniformType has been added to the type parameters in
  // order to make the calls typesafe.  
  import scala.collection.GenTraversableOnce

  // These + methods support adding mixed types...
  // We should be able to use (A,B)* for the B specific type, but the Scala
  // type inferencer complains. It works if we use specific values
  def + [B1 : UniformType, B2 : UniformType](
    e1: (A, B1), e2: (A, B2)
  ): UniformMap[Any] = createMap(value ++ normalize(e1,e2))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3)
  ): UniformMap[Any] = createMap(value ++ normalize(e1,e2,e3))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3), e4: (A, B4)
  ): UniformMap[Any] = createMap(value ++ normalize(e1,e2,e3,e4))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5)
  ): UniformMap[Any] = createMap(value ++ normalize(e1,e2,e3,e4,e5))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6)
  ): UniformMap[Any] = createMap(value ++ normalize(e1,e2,e3,e4,e5,e6))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, 
      B7 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7)
  ): UniformMap[Any] = createMap(value ++ normalize(e1,e2,e3,e4,e5,e6,e7))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, B7 : UniformType,
      B8 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7), e8: (A, B8)
  ): UniformMap[Any] = 
    createMap(value ++ normalize(e1,e2,e3,e4,e5,e6,e7,e8))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, B7 : UniformType,
      B8 : UniformType, B9 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7), e8: (A, B8), e9: (A, B9)
  ): UniformMap[Any] = 
    createMap(value ++ normalize(e1,e2,e3,e4,e5,e6,e7,e8,e9))
  def + [B1 : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, B7 : UniformType,
      B8 : UniformType, B9 : UniformType, B10 : UniformType] (
    e1: (A, B1), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7), e8: (A, B8), e9: (A, B9), e10: (A, B10)
  ): UniformMap[Any] = 
    createMap(value ++ normalize(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10))

  def ++ [B1 : UniformType](
    xs: GenTraversableOnce[(A, B1)]
  ): UniformMap[Any] = 
    createMap((value : Map[A, Any]).++(normalize(xs.toList : _*)))

  def ++: [B1 : UniformType](xs: Traversable[(A, B1)]): UniformMap[Any] = 
    createMap((value : Map[A, Any]).++:(normalize(xs.toSeq : _*)))
  
  def - (key: A, elems: A*): UniformMap[B] = {
    var xm = value - key
    for (elem <- elems) xm -= elem
    createMap(xm)
  }

  def -- (xs: GenTraversableOnce[A]): UniformMap[B] = {
    var xm = value
    for (x <- xs) xm -= x 
    createMap(xm)
  }

  def drop(n: Int): UniformMap[B] = createMap(value.drop(n)) 

  def dropRight(n: Int): UniformMap[B] = createMap(value.dropRight(n)) 

  def dropWhile(p: ((A, B)) => Boolean): UniformMap[B] = 
    createMap(value.dropWhile(p))

  def empty = createMap(Map[A,B]())

  def filter(p: ((A, B)) => Boolean): UniformMap[B] = 
    createMap(value.filter(p))

  def filterKeys(p: (A) => Boolean): UniformMap[B] = 
    createMap(value.filterKeys(p))

  def filterNot(p: ((A, B)) => Boolean): UniformMap[B] = 
    createMap(value.filterNot(p))

  def groupBy[K](f: ((A, B)) => K): Map[K, UniformMap[B]] = {
    val maps = value.groupBy(f)
    var result = Map[K, UniformMap[B]]()
    for ((k, v) <- maps) result += (k -> createMap(v)) 
    result
  }

  def grouped(size: Int): Iterator[UniformMap[B]] = 
    createIterator(value.grouped(size))

  def init: UniformMap[B] = createMap(value.init) 

  def inits: Iterator[UniformMap[B]] = createIterator(value.inits)

  def partition(p: ((A, B)) => Boolean): (UniformMap[B], UniformMap[B]) = {
    val part = value.partition(p)
    (createMap(part._1), createMap(part._2))
  }

  def slice(from: Int, until: Int): UniformMap[B] = 
    createMap(value.slice(from, until)) 

  def sliding(size: Int, step: Int): Iterator[UniformMap[B]] = 
    createIterator(value.sliding(size, step)) 

  def sliding(size: Int): Iterator[UniformMap[B]] = 
    createIterator(value.sliding(size)) 

  def span(p: ((A, B)) => Boolean): (UniformMap[B], UniformMap[B]) = {
    val part = value.span(p)
    (createMap(part._1), createMap(part._2))
  }

  def splitAt(n: Int): (UniformMap[B], UniformMap[B]) = { 
    val part = value.splitAt(n)
    (createMap(part._1), createMap(part._2))
  }

  def tail: UniformMap[B] = createMap(value.tail)

  def tails: Iterator[UniformMap[B]] = createIterator(value.tails)

  def take(n: Int): UniformMap[B] = createMap(value.take(n))

  def takeRight(n: Int): UniformMap[B] = createMap(value.takeRight(n))
  
  def takeWhile(p: ((A, B)) => Boolean): UniformMap[B] = 
    createMap(value.takeWhile(p))

  def updated[B1 : UniformType](key: A, v: B1): UniformMap[Any] = {
    val update = normalize(key -> v)(0)
    createMap(value.updated(update._1, update._2))
  }

  /** Unlike Map withDefault, this method takes a Seq of keys */
  def withDefault(d: (Seq[A]) => B): UniformMap[B] = {
    defaultValueFn = d
    createMap(value.withDefault(x => d(Seq(x))))
  }

  def withDefaultValue(d: B): UniformMap[B] = {
    defaultValueFn = (x => d)
    createMap(value.withDefaultValue(d))
  }

  def withFilter(p: ((A, B)) => Boolean) = value.withFilter(p)


  // Helpers

  private[uniform] def isType(x: Any, m: Manifest[_]): Boolean =  
    (m == manifest[Any] || toManifest(toClass(x)) == m)

  private[uniform] def typeFilter[B1 >: B](
    m: Manifest[_]
  ): (((A, B1)) => Boolean) =
    kv => isType(kv._2, m)

  private[uniform] def normalize[A,B](kvs: (A,B)*) = 
    UniformMap.normalize(kvs : _*)
}

/** Wrapped implementation of Map companion object for use with UniformMap 
  *
  * Only allows operations with uniform types
  */
private[uniform] trait UniformMapLikeCompanion[A] {
   
  /** Subclass must override */
  private[uniform] def createMap[B](xm: Map[A, B]): UniformMap[B]

  // Apply with (A, B)* won't work because it converts to Any ...
  // We will support apply for up to 10 items

  def apply[B](): UniformMap[B] = createMap(Map[A,B]())

  def apply[B : UniformType] (e1: (A, B)): UniformMap[B] = 
    createMap(Map.apply(normalize(e1)(0)))

  def apply[B : UniformType, B2 : UniformType] (
    e1: (A, B), e2: (A, B2)
  ): UniformMap[Any] = createMap(Map.apply(normalize(e1,e2): _*))

  def apply[B : UniformType, B2 : UniformType, B3 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3)
  ): UniformMap[Any] = createMap(Map.apply(normalize(e1,e2,e3): _*))

  def apply[B : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3), e4: (A, B4)
  ): UniformMap[Any] = createMap(Map.apply(normalize(e1,e2,e3,e4): _*))
  
  def apply[B : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5)
  ): UniformMap[Any] = createMap(Map.apply(normalize(e1,e2,e3,e4,e5): _*))

  def apply[B : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6)
  ): UniformMap[Any] = createMap(Map.apply(normalize(e1,e2,e3,e4,e5,e6): _*))

  def apply[B : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, 
      B7 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7)
  ): UniformMap[Any] = 
    createMap(Map.apply(normalize(e1,e2,e3,e4,e5,e6,e7): _*))

  def apply[B : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, B7 : UniformType,
      B8 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7), e8: (A, B8)
  ): UniformMap[Any] = 
    createMap(Map.apply(normalize(e1,e2,e3,e4,e5,e6,e7,e8): _*))

  def apply[B : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, B7 : UniformType,
      B8 : UniformType, B9 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7), e8: (A, B8), e9: (A, B9)
  ): UniformMap[Any] = 
    createMap(Map.apply(normalize(e1,e2,e3,e4,e5,e6,e7,e8,e9): _*))

  def apply[B : UniformType, B2 : UniformType, B3 : UniformType, 
      B4 : UniformType, B5 : UniformType, B6 : UniformType, B7 : UniformType,
      B8 : UniformType, B9 : UniformType, B10 : UniformType] (
    e1: (A, B), e2: (A, B2), e3: (A, B3), e4: (A, B4), e5: (A, B5), 
    e6: (A, B6), e7: (A, B7), e8: (A, B8), e9: (A, B9), e10: (A, B10)
  ): UniformMap[Any] = 
    createMap(Map.apply(normalize(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10): _*))


  def empty[B : UniformType]: UniformMap[B] = createMap(Map[A,B]())
  def empty: UniformMap[Any] = createMap(Map[A, Any]())


  /** Allows extracting key/values */
  def unapplySeq[B](xm: UniformMap[B]): Option[Seq[(A,B)]] = 
     Some(xm.value.toSeq.asInstanceOf[Seq[(A,B)]])

  // Helpers

  private[uniform] def normalize[A,B](kvs: (A,B)*): Seq[(A,B)] = {
    var result = Vector[(A,B)]()
    for (kv <- kvs) {
      if (kv._2.isInstanceOf[UniformMap[_]])
        result :+= 
          (kv._1, kv._2.asInstanceOf[UniformMap[_]].value.asInstanceOf[B])
      else if (kv._2.isInstanceOf[UniformList[_]])
        result :+=
          (kv._1, kv._2.asInstanceOf[UniformList[_]].value.asInstanceOf[B])
      else result :+= kv 
    }
    result
  }
}
