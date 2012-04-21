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
package scalafy

package object util {

  ///////////////////////////////////////////////////////////////////////////
  // vals and implicits 
  ///////////////////////////////////////////////////////////////////////////

  case class PrettyPrintSettings(enabled: Boolean, indent: Int)


  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  /** Creates manifest that goes with class */
  private[scalafy] def toPrimitiveManifest(
    clazz: Class[_]
  ): Option[Manifest[_]] = {
    if (clazz == classOf[String]) Some(manifest[String])
    else if (clazz == classOf[Symbol]) Some(manifest[Symbol])
    else if (clazz == classOf[Int]) Some(manifest[Int])
    else if (clazz == classOf[Short]) Some(manifest[Short])
    else if (clazz == classOf[Long]) Some(manifest[Long])
    else if (clazz == classOf[Float]) Some(manifest[Float])
    else if (clazz == classOf[Double]) Some(manifest[Double])
    else if (clazz == classOf[Boolean]) Some(manifest[Boolean])
    else if (clazz == classOf[Char]) Some(manifest[Char])
    else if (clazz == classOf[Byte]) Some(manifest[Byte])
    else None
  }

  /** Ensures that boxed types are returned as primitive types */
  private[scalafy] def toClass(x: Any): Class[_] = x match {
    case _: Int => java.lang.Integer.TYPE
    case _: Short => java.lang.Short.TYPE
    case _: Long => java.lang.Long.TYPE
    case _: Float => java.lang.Float.TYPE
    case _: Double => java.lang.Double.TYPE
    case _: Char => java.lang.Character.TYPE
    case _: Boolean => java.lang.Boolean.TYPE
    case _: Byte => java.lang.Byte.TYPE
    case _ => x.getClass
  }

  /** Return true if type allowed to be null */
  private[scalafy] def isNullAllowed(clazz: java.lang.Class[_]): Boolean = {
    if (clazz == classOf[Int]) false
    else if (clazz == classOf[Short]) false
    else if (clazz == classOf[Long]) false
    else if (clazz == classOf[Float]) false
    else if (clazz == classOf[Double]) false
    else if (clazz == classOf[Boolean]) false
    else if (clazz == classOf[Char]) false
    else if (clazz == classOf[Byte]) false
    else true
  }

  /** Converts type to Refs (only boxed types can be used with reflection) */
  private[scalafy] def toRefArray(xs: Iterable[Any]): Array[AnyRef] = {
    // Helper to convert to Ref types
    def toRef(x: Any): AnyRef = x match {
      case i: Int => Int.box(i) 
      case s: Short => Short.box(s)
      case l: Long => Long.box(l)
      case f: Float => Float.box(f)
      case d: Double => Double.box(d)
      case b: Boolean => Boolean.box(b)
      case c: Char => Char.box(c)
      case b: Byte => Byte.box(b) 
      case x => x.asInstanceOf[AnyRef]
    }

    val refArray = new Array[AnyRef](xs.size)
    var i = 0
    for (x <- xs) {
      refArray(i) = toRef(x)
      i += 1
    }
    refArray
  }

  /** Gets ordering for type associated with manifest */
  private[scalafy] def getOrdering(m: Manifest[_]): Option[Ordering[_]] = {
    if (m == manifest[String]) Some(Ordering[String])
    else if (m == manifest[Int]) Some(Ordering[Int])
    else if (m == manifest[Short]) Some(Ordering[Short])
    else if (m == manifest[Long]) Some(Ordering[Long])
    else if (m == manifest[Float]) Some(Ordering[Float])
    else if (m == manifest[Double]) Some(Ordering[Double])
    else if (m == manifest[Boolean]) Some(Ordering[Boolean])
    else if (m == manifest[Char]) Some(Ordering[Char])
    else if (m == manifest[Byte]) Some(Ordering[Byte])
    else None
  }

  private[scalafy] def isPrimitiveType(m: Manifest[_]): Boolean = 
    isPrimitiveType(m.erasure)

  /** Runtime check if type is primtive type (String, Symbol are included) */
  private[scalafy] def isPrimitiveType(clazz: Class[_]): Boolean = {
    (clazz == classOf[String] || clazz == classOf[Symbol] || 
      !isNullAllowed(clazz)) 
  }

  private[scalafy] def isTupleType(m: Manifest[_]): Boolean = 
    isTupleType(m.erasure)

  private[scalafy] def getTupleCount(clazz: Class[_]): Int = {
     if (clazz == classOf[Tuple2[_,_]]) 2
     else if (clazz == classOf[Tuple3[_,_,_]]) 3
     else if (clazz == classOf[Tuple4[_,_,_,_]]) 4
     else if (clazz == classOf[Tuple5[_,_,_,_,_]]) 5
     else if (clazz == classOf[Tuple6[_,_,_,_,_,_]]) 6
     else if (clazz == classOf[Tuple7[_,_,_,_,_,_,_]]) 7
     else if (clazz == classOf[Tuple8[_,_,_,_,_,_,_,_]]) 8
     else if (clazz == classOf[Tuple9[_,_,_,_,_,_,_,_,_]]) 9
     else if (clazz == classOf[Tuple10[_,_,_,_,_,_,_,_,_,_]]) 10
     else if (clazz == classOf[Tuple11[_,_,_,_,_,_,_,_,_,_,_]]) 11
     else if (clazz == classOf[Tuple12[_,_,_,_,_,_,_,_,_,_,_,_]]) 12
     else if (clazz == classOf[Tuple13[_,_,_,_,_,_,_,_,_,_,_,_,_]]) 13
     else if (clazz == classOf[Tuple14[_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 14
     else if (clazz == classOf[Tuple15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 15
     else if (clazz == classOf[Tuple16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 16
     else if (clazz == classOf[Tuple17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 17
     else if (clazz == classOf[Tuple18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 18
     else if (clazz == classOf[Tuple19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 19
     else if (clazz == classOf[Tuple20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 20
     else if (clazz == classOf[Tuple21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 21
     else if (clazz == classOf[Tuple22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]) 22
     else -1
  }

  /** Runtime check if type is a TupleN type */
  private[scalafy] def isTupleType(clazz: Class[_]): Boolean =
    getTupleCount(clazz) != -1 

  /** Creates manifest with same type for all N */
  private[scalafy] def createTupleManifest(
    m : Manifest[_], n: Int
  ): Either[String, Manifest[_]] = {
    if (n == 2) 
      Right(createTuple2Manifest(m,m)) 
    else if (n == 3) 
      Right(createTuple3Manifest(m,m,m)) 
    else if (n == 4) 
      Right(createTuple4Manifest(m,m,m,m)) 
    else if (n == 5) 
      Right(createTuple5Manifest(m,m,m,m,m))       
    else if (n == 6)
      Right(createTuple6Manifest(m,m,m,m,m,m))      
    else if (n == 7)
      Right(createTuple7Manifest(m,m,m,m,m,m,m))      
    else if (n == 8)
      Right(createTuple8Manifest(m,m,m,m,m,m,m,m))      
    else if (n == 9)
      Right(createTuple9Manifest(m,m,m,m,m,m,m,m,m))      
    else if (n == 10)
      Right(createTuple10Manifest(m,m,m,m,m,m,m,m,m,m))      
    else Left("tuples of size " + n + " are not supported")
  }

  private[scalafy] def createTupleManifest(
    types : Manifest[_]*
  ): Either[String, Manifest[_]] = {
    if (types.size == 2) 
      Right(createTuple2Manifest(types(0), types(1))) 
    else if (types.size == 3) 
      Right(createTuple3Manifest(types(0), types(1), types(2))) 
    else if (types.size == 4) 
      Right(createTuple4Manifest(types(0), types(1), types(2), types(3))) 
    else if (types.size == 5) 
      Right(createTuple5Manifest(types(0), types(1), types(2), types(3),       
        types(4))) 
    else if (types.size == 6)
      Right(createTuple6Manifest(types(0), types(1), types(2), types(3),      
        types(4), types(5))) 
    else if (types.size == 7)
      Right(createTuple7Manifest(types(0), types(1), types(2), types(3), 
        types(4), types(5), types(6))) 
    else if (types.size == 8)
      Right(createTuple8Manifest(types(0), types(1), types(2), types(3), 
        types(4), types(5), types(6), types(7))) 
    else if (types.size == 9)
      Right(createTuple9Manifest(types(0), types(1), types(2), types(3), 
        types(4), types(5), types(6), types(7), types(8))) 
    else if (types.size == 10)
      Right(createTuple10Manifest(types(0), types(1), types(2), types(3), 
        types(4), types(5), types(6), types(7), types(8), types(9)))
    else Left("tuples of size " + types.size + " are not supported")
  }

  private[scalafy] def createTuple2Manifest[
    T1 : Manifest, T2 : Manifest
  ] = manifest[Tuple2[T1, T2]]
  private[scalafy] def createTuple3Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest
  ] = manifest[Tuple3[T1, T2, T3]]
  private[scalafy] def createTuple4Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest
  ] = manifest[Tuple4[T1, T2, T3, T4]]
  private[scalafy] def createTuple5Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest
  ] = manifest[Tuple5[T1, T2, T3, T4, T5]]
  private[scalafy] def createTuple6Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest,
    T6 : Manifest
  ] = manifest[Tuple6[T1, T2, T3, T4, T5, T6]]
  private[scalafy] def createTuple7Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest,
    T6 : Manifest, T7 : Manifest
  ] = manifest[Tuple7[T1, T2, T3, T4, T5, T6, T7]]
  private[scalafy] def createTuple8Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest,
    T6 : Manifest, T7 : Manifest, T8 : Manifest
  ] = manifest[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]
  private[scalafy] def createTuple9Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest,
    T6 : Manifest, T7 : Manifest, T8 : Manifest, T9 : Manifest
  ] = manifest[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]
  private[scalafy] def createTuple10Manifest[
    T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest,
    T6 : Manifest, T7 : Manifest, T8 : Manifest, T9 : Manifest, T10 : Manifest
  ] = manifest[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]
  // TODO ... 11 -> 22


  /** Runtime TupleN creation */
  private[scalafy] def createTuple(
    xs: Any*
  ): Either[String, Any] = {
    if (xs.size == 2)
      Right(Tuple2(xs(0),xs(1)))
    else if (xs.size == 3)
      Right(Tuple3(xs(0),xs(1),xs(2)))
    else if (xs.size == 4)
      Right(Tuple4(xs(0),xs(1),xs(2),xs(3)))
    else if (xs.size == 5)
      Right(Tuple5(xs(0),xs(1),xs(2),xs(3),xs(4)))
    else if (xs.size == 6)
      Right(Tuple6(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5)))
    else if (xs.size == 7)
      Right(Tuple7(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6)))
    else if (xs.size == 8)
      Right(Tuple8(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7)))
    else if (xs.size == 9)
      Right(Tuple9(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8)))
    else if (xs.size == 10)
      Right(Tuple10(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),
        xs(9)))
    else if (xs.size == 11)
      Right(Tuple11(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10)))
    else if (xs.size == 12)
      Right(Tuple12(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11)))
    else if (xs.size == 13)
      Right(Tuple13(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12)))
    else if (xs.size == 14)
      Right(Tuple14(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13)))
    else if (xs.size == 15)
      Right(Tuple15(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14)))
    else if (xs.size == 16)
      Right(Tuple16(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14),xs(15)))
    else if (xs.size == 17)
      Right(Tuple17(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14),xs(15),xs(16)))
    else if (xs.size == 18)
      Right(Tuple18(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14),xs(15),xs(16),xs(17)))
    else if (xs.size == 19)
      Right(Tuple19(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14),xs(15),xs(16),xs(17),xs(18)))
    else if (xs.size == 20)
      Right(Tuple20(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14),xs(15),xs(16),xs(17),xs(18),xs(19)))
    else if (xs.size == 21)
      Right(Tuple21(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14),xs(15),xs(16),xs(17),xs(18),xs(19),
        xs(20)))
    else if (xs.size == 22)
      Right(Tuple22(xs(0),xs(1),xs(2),xs(3),xs(4),xs(5),xs(6),xs(7),xs(8),xs(9),
        xs(10), xs(11),xs(12),xs(13),xs(14),xs(15),xs(16),xs(17),xs(18),xs(19),
        xs(20),xs(21)))
    else Left("Tuples greater than 22 not supported")
  }

  /** Checks if type is a list type */
  private[scalafy] def isSeqType(m: Manifest[_]): Boolean = {
    isSeqType(m.erasure)
  }

  /** Runtime check if type is a list type */
  private[scalafy] def isSeqType(clazz: java.lang.Class[_]): Boolean = {
    ( classOf[List[_]].isAssignableFrom(clazz) ||
      classOf[::[_]].isAssignableFrom(clazz) ||
      classOf[Vector[_]].isAssignableFrom(clazz) ||
      classOf[IndexedSeq[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.LinearSeq[_]].isAssignableFrom(clazz) ||
      classOf[Seq[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.HashSet[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.TreeSet[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.SortedSet[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.ListSet[_]].isAssignableFrom(clazz) ||
      classOf[Set[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.Stack[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.Queue[_]].isAssignableFrom(clazz) ||
      classOf[Stream[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.ListBuffer[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.PriorityQueue[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.Queue[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.HashSet[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.LinkedHashSet[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.Set[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.ArrayBuffer[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.ResizableArray[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.ArrayStack[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.Stack[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.LinkedList[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.DoubleLinkedList[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.MutableList[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.ArraySeq[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.IndexedSeq[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.LinearSeq[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.Seq[_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.Buffer[_]].isAssignableFrom(clazz) || 
      classOf[scala.collection.mutable.UnrolledBuffer[_]].isAssignableFrom(clazz) )
  }

  /** Runtime Seq Manifest creation */
  private[scalafy] def createSeqManifest[A : Manifest](
    clazz: java.lang.Class[_]
  ): Either[String, Manifest[_]] = {
    if (clazz == classOf[List[_]]) {
      Right(manifest[List[A]]) 
    } else if (clazz == classOf[::[_]]) {
      Right(manifest[::[A]]) 
    } else if (clazz == classOf[Vector[_]]) {
      Right(manifest[Vector[A]]) 
    } else if (clazz == classOf[Seq[_]]) {
      Right(manifest[Seq[A]]) 
    } else if (clazz == classOf[IndexedSeq[_]]) {
      Right(manifest[IndexedSeq[A]]) 
    } else if (clazz == classOf[scala.collection.immutable.LinearSeq[_]]) {
      Right(manifest[scala.collection.immutable.LinearSeq[A]]) 
    } else if (clazz == classOf[scala.collection.immutable.HashSet[_]]) {
      Right(manifest[scala.collection.immutable.HashSet[A]]) 
    } else if (clazz == classOf[scala.collection.immutable.TreeSet[_]]) {
      Right(manifest[scala.collection.immutable.TreeSet[A]]) 
    } else if (clazz == classOf[scala.collection.immutable.SortedSet[_]]) {
      Right(manifest[scala.collection.immutable.SortedSet[A]]) 
    } else if (clazz == classOf[scala.collection.immutable.ListSet[_]]) {
      Right(manifest[scala.collection.immutable.ListSet[A]]) 
    } else if (clazz == classOf[Set[_]]) {
      Right(manifest[Set[A]]) 
    } else if (clazz == classOf[scala.collection.immutable.Stack[_]]) {
      Right(manifest[scala.collection.immutable.Stack[A]]) 
    } else if (clazz == classOf[scala.collection.immutable.Queue[_]]) {
      Right(manifest[scala.collection.immutable.Queue[A]]) 
    } else if (clazz == classOf[Stream[_]]) {
      Right(manifest[Stream[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.ListBuffer[_]]) {
      Right(manifest[scala.collection.mutable.ListBuffer[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.PriorityQueue[_]]) {
      Right(manifest[scala.collection.mutable.PriorityQueue[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.Queue[_]]) {
      Right(manifest[scala.collection.mutable.Queue[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.HashSet[_]]) {
      Right(manifest[scala.collection.mutable.HashSet[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.LinkedHashSet[_]]) {
      Right(manifest[scala.collection.mutable.LinkedHashSet[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.Set[_]]) {
      Right(manifest[scala.collection.mutable.Set[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.ArrayBuffer[_]]) {
      Right(manifest[scala.collection.mutable.ArrayBuffer[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.ResizableArray[_]]) {
      Right(manifest[scala.collection.mutable.ResizableArray[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.ArrayStack[_]]) {
      Right(manifest[scala.collection.mutable.ArrayStack[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.Stack[_]]) {
      Right(manifest[scala.collection.mutable.Stack[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.LinkedList[_]]) {
      Right(manifest[scala.collection.mutable.LinkedList[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.DoubleLinkedList[_]]) {
      Right(manifest[scala.collection.mutable.DoubleLinkedList[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.MutableList[_]]) {
      Right(manifest[scala.collection.mutable.MutableList[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.ArraySeq[_]]) {
      Right(manifest[scala.collection.mutable.ArraySeq[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.IndexedSeq[_]]) {
      Right(manifest[scala.collection.mutable.IndexedSeq[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.LinearSeq[_]]) {
      Right(manifest[scala.collection.mutable.LinearSeq[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.Seq[_]]) {
      Right(manifest[scala.collection.mutable.Seq[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.UnrolledBuffer[_]]) {
      Right(manifest[scala.collection.mutable.UnrolledBuffer[A]]) 
    } else if (clazz == classOf[scala.collection.mutable.Buffer[_]]) {
      Right(manifest[scala.collection.mutable.Buffer[A]]) 
    } else { 
      Left("Unknown type: " + clazz)
    }
  }

  /** Runtime Seq creation (e.g. can't use CanBuildFrom .. :( ) */
  private[scalafy] def createSeq[A : Manifest](
    clazz: java.lang.Class[_]
  ): Either[String, Any] = {
    if (clazz == classOf[List[_]]) {
      Right(List[A]())
    } else if (clazz == classOf[Vector[_]]) {
      Right(Vector[A]())
    } else if (clazz == classOf[::[_]]) {
      Right(Nil)
    } else if (clazz == classOf[IndexedSeq[_]]) {
      Right(IndexedSeq[A]())
    } else if (clazz == classOf[scala.collection.immutable.LinearSeq[_]]) {
      Right(scala.collection.immutable.LinearSeq[A]())
    } else if (clazz == classOf[Seq[_]]) {
      Right(Seq[A]())
    } else if (clazz == classOf[scala.collection.immutable.HashSet[_]]) {
      Right(scala.collection.immutable.HashSet[A]())
    } else if (clazz == classOf[scala.collection.immutable.TreeSet[_]]) {
      val ordering = getOrdering(manifest[A])
      if (!ordering.isEmpty) {
        Right(scala.collection.immutable.TreeSet[A](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("TreeSet not supported for type: " + manifest[A])
      }
    } else if (clazz == classOf[scala.collection.immutable.SortedSet[_]]) {
      val ordering = getOrdering(manifest[A])
      if (!ordering.isEmpty) {
        Right(scala.collection.immutable.SortedSet[A](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("SortedSet not supported for type: " + manifest[A])
      }
    } else if (clazz == classOf[scala.collection.immutable.ListSet[_]]) {
      Right(scala.collection.immutable.ListSet[A]()) 
    } else if (clazz == classOf[Set[_]]) {
      Right(Set[A]())
    } else if (clazz == classOf[scala.collection.immutable.Stack[_]]) {
      Right(scala.collection.immutable.Stack[A]())
    } else if (clazz == classOf[scala.collection.immutable.Queue[_]]) {
      Right(scala.collection.immutable.Queue[A]())
    } else if (clazz == classOf[Stream[_]]) {
      Right(Stream[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.ListBuffer[_]]) {
      Right(scala.collection.mutable.ListBuffer[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.PriorityQueue[_]]) {
      val ordering = getOrdering(manifest[A])
      if (!ordering.isEmpty) {
        Right(scala.collection.mutable.PriorityQueue[A](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("PriorityQueue not supported for type: " + manifest[A])
      }
    } else if (clazz == classOf[scala.collection.mutable.Queue[_]]) {
      Right(scala.collection.mutable.Queue[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.HashSet[_]]) {
      Right(scala.collection.mutable.HashSet[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.LinkedHashSet[_]]) {
      Right(scala.collection.mutable.LinkedHashSet[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.Set[_]]) {
      Right(scala.collection.mutable.Set[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.ArrayBuffer[_]]) {
      Right(scala.collection.mutable.ArrayBuffer[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.ResizableArray[_]]) {
      Right(scala.collection.mutable.ResizableArray[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.ArrayStack[_]]) {
      Right(scala.collection.mutable.ArrayStack[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.Stack[_]]) {
      Right(scala.collection.mutable.Stack[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.LinkedList[_]]) {
      Right(scala.collection.mutable.LinkedList[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.DoubleLinkedList[_]]) {
      Right(scala.collection.mutable.DoubleLinkedList[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.MutableList[_]]) {
      Right(scala.collection.mutable.MutableList[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.ArraySeq[_]]) {
      Right(scala.collection.mutable.ArraySeq[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.IndexedSeq[_]]) {
      Right(scala.collection.mutable.IndexedSeq[A]())
    } else if (clazz == classOf[scala.collection.mutable.LinearSeq[_]]) {
      Right(scala.collection.mutable.LinearSeq[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.Seq[_]]) {
      Right(scala.collection.mutable.Seq[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.UnrolledBuffer[_]]) {
      Right(scala.collection.mutable.UnrolledBuffer[A]()) 
    } else if (clazz == classOf[scala.collection.mutable.Buffer[_]]) {
      Right(scala.collection.mutable.Buffer[A]()) 
    } else {
      Left("Unknown type: " + clazz)
    }
  }

  /** Runtime Seq update */
  private[scalafy] def updateSeq(obj: Any, item: Any): Either[String, Any] = {
    // NOTE: Can probably clean this up to consolidate types with common 
    //       interfaces, but for now this aligns with createSeq 
    if (obj.isInstanceOf[List[_]]) {
      Right(item :: obj.asInstanceOf[List[_]])
    } else if (obj.isInstanceOf[::[_]]) {
      Right(item :: obj.asInstanceOf[::[_]])
    } else if (obj.isInstanceOf[Vector[_]]) {
      Right(obj.asInstanceOf[Vector[_]] :+ item)
    } else if (obj.isInstanceOf[Seq[_]]) {
      Right(obj.asInstanceOf[Seq[_]] :+ item)
    } else if (obj.isInstanceOf[IndexedSeq[_]]) {
      Right(obj.asInstanceOf[IndexedSeq[_]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.immutable.LinearSeq[_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.LinearSeq[_]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.immutable.HashSet[_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.HashSet[Any]] + item)
    } else if (obj.isInstanceOf[scala.collection.immutable.TreeSet[_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.TreeSet[Any]] + item)
    } else if (obj.isInstanceOf[scala.collection.immutable.SortedSet[_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.SortedSet[Any]] + item)
    } else if (obj.isInstanceOf[scala.collection.immutable.ListSet[_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.ListSet[Any]] + item)
    } else if (obj.isInstanceOf[Set[_]]) {
      Right(obj.asInstanceOf[Set[Any]] + item)
    } else if (obj.isInstanceOf[scala.collection.immutable.Stack[_]]) {
      Right(item +: obj.asInstanceOf[scala.collection.immutable.Stack[_]])
    } else if (obj.isInstanceOf[scala.collection.immutable.Queue[_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.Queue[_]] :+ item)
    } else if (obj.isInstanceOf[Stream[_]]) {
      Right(item #:: obj.asInstanceOf[Stream[Any]])
    } else if (obj.isInstanceOf[scala.collection.mutable.ListBuffer[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.ListBuffer[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.PriorityQueue[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.PriorityQueue[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.Queue[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.Queue[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.HashSet[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.HashSet[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.LinkedHashSet[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.LinkedHashSet[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.Set[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.Set[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.ArrayBuffer[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.ArrayBuffer[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.ResizableArray[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.ResizableArray[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.ArrayStack[_]]) {
      Right(item +: obj.asInstanceOf[scala.collection.mutable.ArrayStack[Any]])
    } else if (obj.isInstanceOf[scala.collection.mutable.Stack[_]]) {
      Right(item +: obj.asInstanceOf[scala.collection.mutable.Stack[Any]])
    } else if (obj.isInstanceOf[scala.collection.mutable.LinkedList[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.LinkedList[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.DoubleLinkedList[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.DoubleLinkedList[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.MutableList[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.MutableList[Any]] += item)
    } else if (obj.isInstanceOf[scala.collection.mutable.ArraySeq[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.ArraySeq[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.IndexedSeq[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.IndexedSeq[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.LinearSeq[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.LinearSeq[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.Seq[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.Seq[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.UnrolledBuffer[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.UnrolledBuffer[Any]] :+ item)
    } else if (obj.isInstanceOf[scala.collection.mutable.Buffer[_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.Buffer[Any]] :+ item)
    } else {
      Left("Unknown type: " + obj.getClass) 
    }
  }

  private[scalafy] def isMapType(m: Manifest[_]): Boolean = {
    isMapType(m.erasure)
  }

  /** Runtime check if type is a map type */
  private[scalafy] def isMapType(clazz: Class[_]): Boolean = {
    ( classOf[Map[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.HashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.TreeMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.SortedMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.immutable.ListMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.Map[_,_]].isAssignableFrom(clazz) || 
      classOf[scala.collection.mutable.HashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.WeakHashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.LinkedHashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.OpenHashMap[_,_]].isAssignableFrom(clazz) ||
      classOf[scala.collection.mutable.ListMap[_,_]].isAssignableFrom(clazz) ) 
  }

  /** Runtime Map manifest creation */
  private[scalafy] def createMapManifest[A : Manifest, B : Manifest](
    clazz: java.lang.Class[_]
  ): Either[String, Manifest[_]] = {
    if (clazz == classOf[Map[_,_]]) {
      Right(manifest[Map[A, B]])
    } else if (clazz == classOf[scala.collection.immutable.HashMap[_,_]]) {
      Right(manifest[scala.collection.immutable.HashMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.immutable.TreeMap[_,_]]) {
      Right(manifest[scala.collection.immutable.TreeMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.immutable.SortedMap[_,_]]) {
      Right(manifest[scala.collection.immutable.SortedMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.immutable.ListMap[_,_]]) {
      Right(manifest[scala.collection.immutable.ListMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.mutable.Map[_,_]]) {
      Right(manifest[scala.collection.mutable.Map[A, B]]) 
    } else if (clazz == classOf[scala.collection.mutable.HashMap[_,_]]) {
      Right(manifest[scala.collection.mutable.HashMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.mutable.WeakHashMap[_,_]]) {
      Right(manifest[scala.collection.mutable.WeakHashMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.mutable.LinkedHashMap[_,_]]) {
      Right(manifest[scala.collection.mutable.LinkedHashMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.mutable.OpenHashMap[_,_]]) {
      Right(manifest[scala.collection.mutable.OpenHashMap[A, B]]) 
    } else if (clazz == classOf[scala.collection.mutable.ListMap[_,_]]) {
      Right(manifest[scala.collection.mutable.ListMap[A, B]]) 
    } else {
      Left("Unknown type: " + clazz)
    }
  }

  /** Runtime Map creation (e.g. can't use CanBuildFrom .. :( ) */
  private[scalafy] def createMap[A, B](
    a: Manifest[A],
    b: Manifest[B],
    clazz: java.lang.Class[_]
  ): Either[String, Any] = {a
    if (clazz == classOf[Map[_,_]]) {
      Right(Map[A, B]())
    } else if (clazz == classOf[scala.collection.immutable.HashMap[_,_]]) {
      Right(scala.collection.immutable.HashMap[A, B]()) 
    } else if (clazz == classOf[scala.collection.immutable.TreeMap[_,_]]) {
      val ordering = getOrdering(a)
      if (!ordering.isEmpty) {
        Right(scala.collection.immutable.TreeMap[A,B](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("TreeMap not supported for type: " + a)
      }
    } else if (clazz == classOf[scala.collection.immutable.SortedMap[_,_]]) {
      val ordering = getOrdering(a)
      if (!ordering.isEmpty) {
        Right(scala.collection.immutable.SortedMap[A,B](
          )(ordering.get.asInstanceOf[Ordering[A]]))
      } else {
        Left("SortedMap not supported for type: " + a)
      }
    } else if (clazz == classOf[scala.collection.immutable.ListMap[_,_]]) {
      Right(scala.collection.immutable.ListMap[A, B]()) 
    } else if (clazz == classOf[scala.collection.mutable.Map[_,_]]) {
      Right(scala.collection.mutable.Map[A, B]()) 
    } else if (clazz == classOf[scala.collection.mutable.HashMap[_,_]]) {
      Right(scala.collection.mutable.HashMap[A, B]()) 
    } else if (clazz == classOf[scala.collection.mutable.WeakHashMap[_,_]]) {
      Right(scala.collection.mutable.WeakHashMap[A, B]()) 
    } else if (clazz == classOf[scala.collection.mutable.LinkedHashMap[_,_]]) {
      Right(scala.collection.mutable.LinkedHashMap[A, B]()) 
    } else if (clazz == classOf[scala.collection.mutable.OpenHashMap[_,_]]) {
      Right(scala.collection.mutable.OpenHashMap[A, B]())
    } else if (clazz == classOf[scala.collection.mutable.ListMap[_,_]]) {
      Right(scala.collection.mutable.ListMap[A, B]()) 
    } else {
      Left("Unknown type: " + clazz)
    }
  }

  /** Runtime map update */
  private[scalafy] def updateMap(
    obj: Any, name: Any, item: Any
  ): Either[String, Any] = {
    // NOTE: Can probably clean this up to consolidate types with common 
    //       interfaces, but for now this aligns with createMap
    if (obj.isInstanceOf[Map[_,_]]) {
      Right(obj.asInstanceOf[Map[Any,Any]] + (name -> item))
    } else if (obj.isInstanceOf[scala.collection.immutable.HashMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.HashMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.immutable.TreeMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.TreeMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.immutable.SortedMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.SortedMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.immutable.ListMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.immutable.ListMap[Any,Any]] + 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.mutable.Map[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.Map[Any,Any]] += 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.mutable.HashMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.HashMap[Any,Any]] += 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.mutable.WeakHashMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.WeakHashMap[Any,Any]] += 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.mutable.LinkedHashMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.LinkedHashMap[Any,Any]] += 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.mutable.OpenHashMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.OpenHashMap[Any,Any]] += 
          (name -> item))
    } else if (obj.isInstanceOf[scala.collection.mutable.ListMap[_,_]]) {
      Right(obj.asInstanceOf[scala.collection.mutable.ListMap[Any,Any]] += 
          (name -> item))
    } else {
      Left("Unknown type: " + obj.getClass)
    }
  }

} // end package object
