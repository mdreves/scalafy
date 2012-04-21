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

/** Wrapper for Streams */
private[parser] class StreamData[A](
  val iter: Iterator[ParseEvent],
  streamType: Manifest[A],
  settings: ParserSettings
) extends ArrayData[A](streamType, settings) {

  private var obj: A = 
    createStream(objType.typeArguments(0)).asInstanceOf[A]

  override def getItemType(index: Int): Manifest[_] = objType.typeArguments(0)

  def getObj(): Either[String, A] = {
    Reifiable(getManifest(), obj)(settings.reifiableSettings)
    Right(obj)
  }

  override protected def createManifestUsing[A : Manifest]: Manifest[_] = {
    manifest[Stream[A]]
  }

  /** Recursive stream definition... */
  private def createStream[B : Manifest]: Stream[B] = {
    TextParser.parseNext(iter, settings)(objType.typeArguments(0)) match {
      case Left(l) => Stream.empty.asInstanceOf[Stream[B]]
      case Right(r) => r match { 
        case Some(v) =>
          // Unwrap object if inside another container
          if (v.isInstanceOf[StructuredData[_]]) {
            v.asInstanceOf[StructuredData[_]].getObj match {
              case Right(r) => Stream.cons(r.asInstanceOf[B], createStream) 
              case Left(l) => Stream.empty.asInstanceOf[Stream[B]]
            }
          } else { 
            Stream.cons(v.asInstanceOf[B], createStream) 
          }
        case None => Stream.empty.asInstanceOf[Stream[B]]
      }
    }
  }
}
