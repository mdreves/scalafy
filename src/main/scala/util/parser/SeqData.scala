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
import scalafy.util._

/** Wrapper for sequences */
private[parser] class SeqData[A](
  listType: Manifest[A], 
  settings: ParserSettings
) extends ArrayData[A](listType, settings) {

  private var obj: Any = { // Using Any so can use ListBuffer for List
    // Special case for List/Seq, since we need to reverse it, we will
    // use a ListBuffer until the list is requested
    val clazz = 
      if (objType.erasure == classOf[List[_]] || 
          objType.erasure == classOf[::[_]] || 
          objType.erasure == classOf[Seq[_]] || 
          objType.erasure == classOf[Stream[_]] || 
          objType.erasure == classOf[Iterator[_]] || 
          objType.erasure == classOf[Iterable[_]] || 
          objType.erasure == classOf[collection.immutable.LinearSeq[_]]) {
        classOf[collection.mutable.ListBuffer[_]]
      } else objType.erasure

    createSeq(clazz)(objType.typeArguments(0)) match {
      case Right(r) => r
      case Left(l) => throw new Error(l)
    }
  }

  override def getItemType(index: Int): Manifest[_] = objType.typeArguments(0)

  def getObj(): Either[String, A] = {
    // Get List from ListBuffer if List used
    val result = 
      if (objType.erasure == classOf[List[_]] ||
          objType.erasure == classOf[::[_]]) {
        obj.asInstanceOf[collection.mutable.ListBuffer[_]].toList
      } else if (objType.erasure == classOf[Seq[_]]) {
        obj.asInstanceOf[collection.mutable.ListBuffer[_]].toSeq
      } else if (objType.erasure == 
          classOf[collection.immutable.LinearSeq[_]]) {
        obj.asInstanceOf[collection.mutable.ListBuffer[_]].toSeq
      } else if (objType.erasure == classOf[Stream[_]]) {
        obj.asInstanceOf[collection.mutable.ListBuffer[_]].toStream
      } else if (objType.erasure == classOf[Iterator[_]]) {
        obj.asInstanceOf[collection.mutable.ListBuffer[_]].toIterator
      } else if (objType.erasure == classOf[Iterable[_]]) {
        obj.asInstanceOf[collection.mutable.ListBuffer[_]].toIterable
      } else obj

    Reifiable(getManifest(), result)(settings.reifiableSettings)
    Right(result.asInstanceOf[A])
  }

  override protected def internalAdd(item: Any): Option[String] = {
    validateType(item, objType.typeArguments(0)) match {
      case Some(v) => return Some(v) // error
      case None => {}
    }

    updateSeq(obj, item) match {
      case Right(r) => obj = r; None
      case Left(l) => Some(l)
    }
  }

  override protected def createManifestUsing[T : Manifest]: Manifest[_] = {
    createSeqManifest(objType.erasure)(manifest[T]) match {
      case Right(m) => m
      case Left(l) => throw new Error(l)
    }
  }
}
