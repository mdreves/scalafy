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

import collection.mutable.Buffer

/** 
  * Default implementation of chunked iterator using underlying iterator 
  *
  * Subclass must pass a BufferedIterator and a converter to convert from
  * buffered items of type A to a type B.
  */
class DefaultChunkedIterator[A,B](
  private val iter: BufferedIterator[B], private val converter: Seq[B] => A
) extends ChunkedIterator[A,B] {

  private var default: B = _

  private var itemsRead = 0
  private var stopped = false 
  private var chunkValid = false 

  // Iterator methods

  def hasNext: Boolean = {
    if (!iter.hasNext) false
    else if (isEscape(iter.head)) { // skip over escape
      iter.next()
      iter.hasNext
    } else if (isOnStopMarker(nextStop)) {
      chunkValid = true
      stopped = true
      false  // stop on chunk boundaries
    } else true 
  }

  def next(): B = {
    chunkValid = false
    itemsRead += 1
    iter.next
  }

  // BufferedIterator methods

  def head: B = iter.head

  // ChunkedIterator methods

  def hasNextChunk: Boolean = iter.hasNext 

  def nextChunk(): Option[A] = {
    
    if (nextStop == default) { 
      val buf = createBuffer
      while (iter.hasNext) {
        val item = iter.next
        itemsRead += 1
        if (isEscape(item)) {
          if (iter.hasNext) {
            addToBuffer(buf, iter.next)
            itemsRead += 1
          }
        } else addToBuffer(buf, item)
      }
      chunkValid = true
      stopped = true
      Some(converter(buf))
    } else {
      nextChunk(nextStop)
    }
  }

  def nextChunk(stop: B): Option[A] = {
    // skip over chunk boundary
    if (stopped && isOnStopMarker(nextStop)) {
      iter.next
      itemsRead += 1
    }

    val buf = createBuffer 
    while (iter.hasNext && !isOnStopMarker(stop)) {
      val item = iter.next
      itemsRead += 1
      if (isEscape(item)) {
        if (iter.hasNext) {
          addToBuffer(buf, iter.next)
          itemsRead += 1
        }
      } else addToBuffer(buf, item)
    }

    if (!stopRequired || iter.hasNext && isOnStopMarker(stop)) {
      chunkValid = true
      stopped = true
      Some(converter(buf))
    } else {
      chunkValid = false
      None
    }
  }

  def lastChunkValid() = chunkValid

  def readCount() = itemsRead + 1

  // Helpers

  protected def createBuffer: Seq[B] = Buffer[B]()
 
  // Shouldn't be needed, but StringBuilder is not playing by the rules...
  protected def addToBuffer(buf: Seq[B], item: B) = buf :+ item

  /** Checks if item at index is stop item */
  protected def isOnStopMarker(stop: B): Boolean = 
    (stop != default && iter.head == stop)

  /** Checks if item used for escaping */
  protected def isEscape(item: B): Boolean = 
    (escape != default && item == escape)
}

object DefaultChunkedIterator {
  def apply(
    iter: BufferedIterator[Char]
  ): DefaultChunkedIterator[String,Char] = {
    val converter: Seq[Char] => String = strBuf => strBuf.toString 
    new DefaultChunkedIterator(iter, converter) {
      override def createBuffer = new StringBuilder
      override def addToBuffer(buf: Seq[Char], item: Char) = 
        buf.asInstanceOf[StringBuilder] + item
    }
  }

  def apply(
    iter: BufferedIterator[Char], escape: Char
  ): DefaultChunkedIterator[String,Char] = {
    val result = apply(iter)
    result.escape = escape
    result
  }

  def apply[B, A](iter: BufferedIterator[B], converter: Seq[B] => A) =
    new DefaultChunkedIterator(iter, converter)
}
