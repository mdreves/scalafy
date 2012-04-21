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

/** Wrapped implementation of List[A] that only allows uniform types.
  *
  * The createList method needs to be overridden by the subclass in order to
  * build the UniformList[A] type.
  */
private[uniform] trait UniformListLike[A] extends Product {

  // Wrapped container
  private[uniform] val value: List[A]

  /** Subclass must override */
  private[uniform] def createList[A1](xm: List[A1]): UniformList[A1] 
 
  /** Subclass must override */
  private[uniform] def createIterator[A1](
    i: Iterator[List[A1]]
  ): Iterator[UniformList[A1]]

  // Product overrides

  def productArity = value.productArity
  
  def productElement(n: Int): Any = 
    if (n == 0) value.productElement(n)
    else createList(value.productElement(n).asInstanceOf[List[Any]])
  
  override def productPrefix = "UniformList"


  // What would have been standard List overrides 


  // These are all the methods that would be implemented by List[A] if
  // mixing in LinearSeq trait were possible. These methods return
  // UniformList[A] whenever a List would have been returned. In addition,
  // wherever possible the context bound UniformType has been added to the
  // type parameters in order to make the calls typesafe.  
  import scala.collection.GenTraversableOnce

  // NOTE: It would be nice to have the ability to return a List of our own
  //       type instead of UniformList[Any], but no feasible solution was
  //       determined (see UniformList for more details).

  def ++ [A1 : UniformType](xs: GenTraversableOnce[A1]): UniformList[Any] = 
    createList((value: List[Any]).++(normalize(xs.toList : _*)))

  def ++: [A1 : UniformType](xs: Traversable[A1]): UniformList[Any] = 
    createList((value: List[Any]).++:(normalize(xs.toSeq : _*)))

  def +: [A1 : UniformType](elem: A1): UniformList[Any] = 
    createList((value: List[Any]).+:(normalize(elem)(0)))

  def :+ [A1 : UniformType](elem: A1): UniformList[Any] = 
    createList((value: List[Any]).:+(normalize(elem)(0)))

  def :: [A1 : UniformType](elem: A1): UniformList[Any] = 
    createList((value: List[Any]).::(normalize(elem)(0)))

  def ::: [A1 : UniformType](xs: List[A1]): UniformList[Any] =
    createList((value: List[Any]).:::(normalize(xs : _*).toList))

  def combinations(n: Int): Iterator[UniformList[A]] = 
    createIterator(value.combinations(n))

  def diff[A1 >: A](that: Seq[A1]): UniformList[A] = 
    createList(value.diff(that))

  def distinct: UniformList[A] = createList(value.distinct)

  def drop(n: Int): UniformList[A] = createList(value.drop(n)) 

  def dropRight(n: Int): UniformList[A] = createList(value.dropRight(n)) 

  def dropWhile(p: (A) => Boolean): UniformList[A] = 
    createList(value.dropWhile(p))

  def filter(p: (A) => Boolean): UniformList[A] = 
    createList(value.filter(p))

  def filterNot(p: (A) => Boolean): UniformList[A] = 
    createList(value.filterNot(p))

  def groupBy[K](f: (A) => K): Map[K, UniformList[A]] = {
    val lists = value.groupBy(f)
    var result = Map[K, UniformList[A]]()
    for ((k, l) <- lists) result += (k -> createList(l)) 
    result
  }

  def grouped(size: Int): Iterator[UniformList[A]] = 
    createIterator(value.grouped(size))

  def init: UniformList[A] = createList(value.init) 

  def inits: Iterator[UniformList[A]] = createIterator(value.inits)

  def intersect[A1 >: A](that: Seq[A1]): UniformList[Any] = 
    createList((value: List[Any]).intersect(that))

  def padTo(len: Int, elem: A): UniformList[A] = 
    createList(value.padTo(len, elem))
  
  def partition(p: (A) => Boolean): (UniformList[A], UniformList[A]) = {
    val part = value.partition(p)
    (createList(part._1), createList(part._2))
  }

  def patch[A1](from: Int, that: Seq[A1], replaced: Int): UniformList[Any] =
    createList(value.patch(from, that, replaced))

  def permutations: Iterator[UniformList[A]] = 
    createIterator(value.permutations) 
   
  def reverse: UniformList[A] = createList(value.reverse) 
  
  def reverse_:::[A1 : UniformType](prefix: List[A1]): UniformList[Any] =
    createList(value.reverse_:::(normalize(prefix : _*).toList))

  def scan[A1 >: A](z: A1)(op: (A1, A1) => A1): UniformList[Any] =
    createList(value.scan(z)(op))

  def slice(from: Int, until: Int): UniformList[A] = 
    createList(value.slice(from, until)) 

  def sliding(size: Int, step: Int): Iterator[UniformList[A]] = 
    createIterator(value.sliding(size, step)) 

  def sliding(size: Int): Iterator[UniformList[A]] = 
    createIterator(value.sliding(size)) 

  def sortBy[B](f: (A) => B)(implicit ord: Ordering[B]): UniformList[A] =
    createList(value.sortBy(f)(ord))

  def sortWith(lt: (A, A) => Boolean): UniformList[A] =
    createList(value.sortWith(lt))

  def sorted[B >: A](implicit ord: Ordering[B]): UniformList[A] =
    createList(value.sorted(ord))

  def span(p: (A) => Boolean): (UniformList[A], UniformList[A]) = {
    val part = value.span(p)
    (createList(part._1), createList(part._2))
  }

  def splitAt(n: Int): (UniformList[A], UniformList[A]) = { 
    val part = value.splitAt(n)
    (createList(part._1), createList(part._2))
  }

  def tail: UniformList[A] = createList(value.tail)

  def tails: Iterator[UniformList[A]] = createIterator(value.tails)

  def take(n: Int): UniformList[A] = createList(value.take(n))

  def takeRight(n: Int): UniformList[A] = createList(value.takeRight(n))
  
  def takeWhile(p: (A) => Boolean): UniformList[A] = 
    createList(value.takeWhile(p))

  def union[A1 : UniformType](that: Seq[A1]): UniformList[Any] =
    createList((value: List[Any]).union(normalize(that: _*)))

  // We must make this an A1 type and not A otherwise we have an
  // issue with UniformList[Any] where any value will be able to be added
  def updated[A1 : UniformType](index: Int, v: A1): UniformList[Any] =
    createList((value: List[Any]).updated(index, normalize(v)(0)))

  def withFilter(p: (A) => Boolean) = value.withFilter(p) 


  // Helpers

  private[uniform] def isType(x: Any, m: Manifest[_]): Boolean =  
    (m == manifest[Any] || toManifest(toClass(x)) == m)

  private[uniform] def typeFilter[A1 >: A](m: Manifest[_]): ((A1) => Boolean) =
    x => isType(x, m)

  private[uniform] def normalize[A](xs: A*) = UniformList.normalize(xs : _*)
}

/** Wrapped implementation of companion List object for use with UniformList
  *
  * Only allows operations with uniform types
  */
private[uniform] trait UniformListLikeCompanion {
   
  /** Subclass must override */
  private[uniform] def createList[A](xm: List[A]): UniformList[A]

  // Apply with A* won't work because it converts to Any ...
  // We will support apply for up to 10 items

  def apply[A](): UniformList[A] = createList(List[A]())

  def apply[A : UniformType] (e1: A): UniformList[A] = 
    createList(List.apply(normalize(e1)(0)))

  def apply[A : UniformType, A2 : UniformType] (
    e1: A, e2: A2
  ): UniformList[Any] = createList(List.apply(normalize(e1,e2): _*))

  def apply[A : UniformType, A2 : UniformType, A3 : UniformType] (
    e1: A, e2: A2, e3: A3
  ): UniformList[Any] = createList(List.apply(normalize(e1,e2,e3): _*))

  def apply[A : UniformType, A2 : UniformType, A3 : UniformType, 
      A4 : UniformType] (
    e1: A, e2: A2, e3: A3, e4: A4
  ): UniformList[Any] = createList(List.apply(normalize(e1,e2,e3,e4): _*))
  
  def apply[A : UniformType, A2 : UniformType, A3 : UniformType, 
      A4 : UniformType, A5 : UniformType] (
    e1: A, e2: A2, e3: A3, e4: A4, e5: A5
  ): UniformList[Any] = createList(List.apply(normalize(e1,e2,e3,e4,e5): _*))

  def apply[A : UniformType, A2 : UniformType, A3 : UniformType, 
      A4 : UniformType, A5 : UniformType, A6 : UniformType] (
    e1: A, e2: A2, e3: A3, e4: A4, e5: A5, e6: A6
  ): UniformList[Any] = createList(List.apply(normalize(e1,e2,e3,e4,e5,e6): _*))

  def apply[A : UniformType, A2 : UniformType, A3 : UniformType, 
      A4 : UniformType, A5 : UniformType, A6 : UniformType, 
      A7 : UniformType] (
    e1: A, e2: A2, e3: A3, e4: A4, e5: A5, e6: A6, e7: A7
  ): UniformList[Any] = 
    createList(List.apply(normalize(e1,e2,e3,e4,e5,e6,e7): _*))

  def apply[A : UniformType, A2 : UniformType, A3 : UniformType, 
      A4 : UniformType, A5 : UniformType, A6 : UniformType, A7 : UniformType,
      A8 : UniformType] (
    e1: A, e2: A2, e3: A3, e4: A4, e5: A5, e6: A6, e7: A7, e8: A8
  ): UniformList[Any] = 
    createList(List.apply(normalize(e1,e2,e3,e4,e5,e6,e7,e8): _*))

  def apply[A : UniformType, A2 : UniformType, A3 : UniformType, 
      A4 : UniformType, A5 : UniformType, A6 : UniformType, A7 : UniformType,
      A8 : UniformType, A9 : UniformType] (
    e1: A, e2: A2, e3: A3, e4: A4, e5: A5, e6: A6, e7: A7, e8: A8, e9: A9
  ): UniformList[Any] = 
    createList(List.apply(normalize(e1,e2,e3,e4,e5,e6,e7,e8,e9): _*))

  def apply[A : UniformType, A2 : UniformType, A3 : UniformType, 
      A4 : UniformType, A5 : UniformType, A6 : UniformType, A7 : UniformType,
      A8 : UniformType, A9 : UniformType, A10 : UniformType] (
    e1: A, e2: A2, e3: A3, e4: A4, e5: A5, e6: A6, e7: A7, e8: A8, e9: A9, 
    e10: A10
  ): UniformList[Any] = 
    createList(List.apply(normalize(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10): _*))

  def concat[A : UniformType](xss: Traversable[A]*): UniformList[A] =   
    createList(List.concat(xss : _*))

  def empty[A : UniformType]: UniformList[A] = createList(List[A]())

  def empty: UniformList[Any] = createList(List())

  def fill[A : UniformType](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(
    elem: => A
  ): UniformList[List[List[List[List[A]]]]] = 
    createList(List.fill(n1,n2,n3,n4,n5)(elem))

  def fill[A : UniformType](n1: Int, n2: Int, n3: Int, n4: Int)(
    elem: => A
  ): UniformList[List[List[List[A]]]] = createList(List.fill(n1,n2,n3,n4)(elem))

  def fill[A : UniformType](n1: Int, n2: Int, n3: Int)(
    elem: => A
  ): UniformList[List[List[A]]] = createList(List.fill(n1,n2,n3)(elem))

  def fill[A : UniformType](n1: Int, n2: Int)(
    elem: => A
  ): UniformList[List[A]] = createList(List.fill(n1,n2)(elem))
  
  def fill[A : UniformType](n: Int)(elem: => A): UniformList[A] = 
    createList(List.fill(n)(elem))

  def iterate[A : UniformType](start: A, len: Int)(f: (A) => A):UniformList[A] =
    createList(List.iterate(start, len)(f))

  def range[T](start: T, end: T, step: T)(
    implicit arg0: Integral[T], ev: UniformType[T]
  ): UniformList[T] = createList(List.range(start, end, step))

  def range[T](start: T, end: T)(
    implicit arg0: Integral[T], ev: UniformType[T]
  ): UniformList[T] = createList(List.range(start, end))

  def tabulate[A : UniformType](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(
    f: (Int, Int, Int, Int, Int) => A
  ): UniformList[List[List[List[List[A]]]]] = 
    createList(List.tabulate(n1,n2,n3,n4,n5)(f))

  def tabulate[A : UniformType](n1: Int, n2: Int, n3: Int, n4: Int)(
    f: (Int, Int, Int, Int) => A
  ): UniformList[List[List[List[A]]]] = 
    createList(List.tabulate(n1,n2,n3,n4)(f))

  def tabulate[A : UniformType](n1: Int, n2: Int, n3: Int)(
    f: (Int, Int, Int) => A
  ): UniformList[List[List[A]]] = createList(List.tabulate(n1,n2,n3)(f))

  def tabulate[A : UniformType](n1: Int, n2: Int)(
    f: (Int, Int) => A
  ): UniformList[List[A]] = createList(List.tabulate(n1,n2)(f))

  def tabulate[A : UniformType](n: Int)(f: (Int) => A): UniformList[A] = 
    createList(List.tabulate(n)(f))

  def unapplySeq[A](xs: UniformList[A]): Option[List[A]] = Some(xs.value)
  

  // Helpers

  private[uniform] def normalize[A](xs: A*): Seq[A] = {
    var result = Vector[A]()
    for (x <- xs) {
      if (x.isInstanceOf[UniformList[_]])
        result :+= x.asInstanceOf[UniformList[_]].value.asInstanceOf[A]
      else if (x.isInstanceOf[UniformMap[_]])
        result :+= x.asInstanceOf[UniformMap[_]].value.asInstanceOf[A]
      else result :+= x 
    }
    result
  }
}
