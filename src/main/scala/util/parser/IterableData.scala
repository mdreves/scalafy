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
import scalafy.util.casing._

/** Wrapper for Iterable */
private[parser] class IterableData[A](
  val iter: Iterator[ParseEvent],
  iterableType: Manifest[A],
  settings: ParserSettings
) extends ArrayData[A](iterableType, settings) {

  private var obj: A = 
    if (objType <:< manifest[Iterable[_]])
      createIterable(objType.typeArguments(0)).asInstanceOf[A]
    else if (objType <:< manifest[Iterator[_]])
      createIterator(objType.typeArguments(0)).asInstanceOf[A]
    else throw new Error("Unsupported type: " + objType) // can't happen 

  override def getItemType(index: Int): Manifest[_] = objType.typeArguments(0)

  def getObj(): Either[String, A] = {
    Reifiable(getManifest(), obj)(settings.reifiableSettings)
    Right(obj)
  }

  override protected def createManifestUsing[A : Manifest]: Manifest[_] = {
    if (objType <:< manifest[Iterable[_]]) manifest[Iterable[A]]
    else if (objType <:< manifest[Iterator[_]]) manifest[Iterator[A]]
    else throw new Error("Unsupported type: " + objType) // can't happen 
  }

  def createIterable[B : Manifest]: Iterable[B] = { 
    // Annonymous iterable
    new Iterable[B]() { 
      override def iterator: Iterator[B] = createIterator[B] 
    }
  }

  def createIterator[B : Manifest]: Iterator[B] =  {
    // Annonymous iterator
    new Iterator[B]() {
      private var cur: Any = null
      private var done = false 

      def hasNext: Boolean = {
        if (cur != null) return true 
        if (done) return false 

        TextParser.parseNext(iter, settings)(objType.typeArguments(0)) match {
          case Left(l) => done = true; false
          case Right(r) => r match { 
            case Some(v) =>
              // Unwrap object if inside another container
              if (v.isInstanceOf[StructuredData[_]]) {
                v.asInstanceOf[StructuredData[_]].getObj match {
                  case Right(r) => cur = r; true
                  case Left(l) => done = true; false
                }
              } else { cur = v; true }
            case None => done = true; false // Done
          }
        }
      }

      def next(): B = {
        if (cur == null) {
          hasNext // trigger retrieval, if haven't already
          if (done || cur == null) {
            throw new NoSuchElementException("next on empty iterator")
          }
        }

        val result = cur
        cur = null
        result.asInstanceOf[B] 
      }
    }
  }
}
