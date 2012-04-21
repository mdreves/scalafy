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

import java.io.Reader

import scala.collection.mutable.Buffer
import scala.io.Source

/** Chunked iterator.
  *
  * The type A is the underlying type of the iterator (for example Char) and
  * type B is the chunk type (for example String).
  *
  * Note that the semantics of the underlying buffered iterator hasNext
  * method is slighly different when used with a chunked iterator. If the
  * next value is a normal boundary marker for a chunk, then hasNext will
  * return false. A subsequent call to next will skip over the boundary
  * marker and reset hasNext to return true (unless iteration has finished).
  * This is to allow the next method to be used to iterate values within a
  * given chunk stopping at the chunk boundaries. To check that iteration is
  * complete, use hasNextChunk.
  */
trait ChunkedIterator[A,B] extends BufferedIterator[B] {

  /** Boundary marker for chunk.
    *
    * This is the boundary marker used with next and nextChunk. The default
    * is to use the end of the underlying iterator for B.
    */
  var nextStop: B = _ 

  /** Escape value to escape a boundary marker.  */
  var escape: B = _

  /** Set to true to require the chunk boundary marker to exist 
    *
    * If false, reaching the end of iteration will be allowed to terminate a
    * chunk
    */
  var stopRequired: Boolean = false 

  protected var default: B = _
  protected var itemsRead = 0
  protected var stopped = false 
  protected var chunkValid = false 

  // Iterator methods

  def hasNext: Boolean = {
    if (!internalHasNext) false
    else if (isEscape(internalHead)) { // skip over escape
      internalNext
      itemsRead += 1
      internalHasNext
    } else if (isOnStopMarker(nextStop)) {
      chunkValid = true
      stopped = true
      false  // stop on chunk boundaries
    } else true 
  }

  def next(): B = {
    if (!internalHasNext)
      throw new NoSuchElementException("next on empty iterator")

    chunkValid = false
    val item = internalNext
    itemsRead += 1
    item
  }


  // BufferedIterator methods

  def head: B = {
    if (!internalHasNext)
      throw new NoSuchElementException("next on empty iterator")

    internalHead
  }


  // ChunkedIterator methods

  /** Checks if iteration is complete */
  def hasNextChunk: Boolean = internalHasNext

  /** Reads next chunk stopping at normal boundary (for example end of str) 
    *
    * If the normal boundary is not the end of the iterator, then the value
    * representing the boundary will not be returned with the chunk nor will
    * it be returned with any subsequent iteration operations 
    */
  def nextChunk(): Option[A] = {
    
    if (nextStop == default) { 
      if (!internalHasNext)
        throw new NoSuchElementException("next on empty iterator")

      val buf = createBuffer
      while (internalHasNext) {
        val item = internalNext
        itemsRead += 1
        if (isEscape(item)) {
          if (internalHasNext) {
            addToBuffer(buf, internalNext)
            itemsRead += 1
          }
        } else addToBuffer(buf, item)
      }
      chunkValid = true
      stopped = true
      Some(convertFromBuffer(buf))
    } else {
      nextChunk(nextStop)
    }
  }

  /** Reads next chunk stopping at given value.
    *
    * As per nextChunk the stop marker will not be returned with the chunk
    * nor will it be returned with any subsequent iteration operatons.
    */
  def nextChunk(stop: B): Option[A] = {
    if (!internalHasNext)
      throw new NoSuchElementException("next on empty iterator")

    // skip over chunk boundary
    if (stopped && isOnStopMarker(nextStop)) {
      internalNext
      itemsRead += 1
    }

    val buf = createBuffer 
    while (internalHasNext && !isOnStopMarker(stop)) {
      val item = internalNext 
      itemsRead += 1
      if (isEscape(item)) {
        if (internalHasNext) {
          addToBuffer(buf, internalNext)
          itemsRead += 1
        }
      } else addToBuffer(buf, item)
    }

    if (!stopRequired || internalHasNext && isOnStopMarker(stop)) {
      chunkValid = true
      stopped = true
      Some(convertFromBuffer(buf))
    } else {
      chunkValid = false
      None
    }
  }

  /** Returns true if last chunk read was valid */
  def lastChunkValid(): Boolean = chunkValid

  /** Returns number of values of type B read during iteration */
  def readCount(): Int = itemsRead + 1

  override def toIterable: Iterable[B] = {
    val self = this

    new Iterable[B] { def iterator = self }
  }

  /** Checks if item at index is stop item */
  protected def isOnStopMarker(stop: B): Boolean = 
    (stop != default && internalHead == stop)

  /** Checks if item used for escaping */
  protected def isEscape(item: B): Boolean =
    (escape != default && item == escape)

  // Internal implementations
  @inline protected def internalHasNext: Boolean
  @inline protected def internalHead: B 
  @inline protected def internalNext: B
  @inline protected def createBuffer: Any
  @inline protected def addToBuffer(buf: Any, item: B)
  @inline protected def convertFromBuffer(buf: Any): A
}

object ChunkedIterator {
  def apply(s: String): ChunkedIterator[String,Char] = 
    new StringChunkedIterator(s)

  def apply(
    iter: BufferedIterator[Char]
  ): ChunkedIterator[String,Char] = 
    new CharChunkedBufferedIterator(iter)

  def apply(
    reader: Reader
  ): ChunkedIterator[String,Char] = 
    new ReaderChunkedIterator(reader)

  def apply(
    source: Source
  ): ChunkedIterator[String,Char] = 
    new CharChunkedBufferedIterator(source.buffered)

  def apply[B, A](
    iter: BufferedIterator[B], converter: Seq[B] => A
  ): ChunkedIterator[A,B] = 
    new ConverterChunkedBufferedIterator(iter, converter)
}


///////////////////////////////////////////////////////////////////////////
// Implementations 
///////////////////////////////////////////////////////////////////////////

/** Chunked iterator optimized for Strings */
class StringChunkedIterator(private val str: String) 
    extends ChunkedIterator[String,Char] {

  escape = '\\'

  @inline protected def internalHasNext: Boolean = (itemsRead < str.length) 
  @inline protected def internalHead: Char = str(itemsRead) 
  @inline protected def internalNext: Char = str(itemsRead) 
  @inline protected def createBuffer: Any = new StringBuilder
  @inline protected def addToBuffer(buf: Any, item: Char) =
    buf.asInstanceOf[StringBuilder].append(item)
  @inline protected def convertFromBuffer(buf: Any): String =
    buf.asInstanceOf[StringBuilder].toString
}

/** Chunked iterator based on buffered iterator */
abstract class ChunkedBufferedIterator[A,B](
  private val iter: BufferedIterator[B]
) extends ChunkedIterator[A,B] {

  @inline protected def internalHasNext: Boolean = iter.hasNext
  @inline protected def internalHead: B = iter.head 
  @inline protected def internalNext: B = iter.next
}

/* Chunked iterator based on buffered iterator optimized for chars */
class CharChunkedBufferedIterator(
  private val iter: BufferedIterator[Char]
) extends ChunkedBufferedIterator[String, Char](iter) {

  escape = '\\'

  @inline protected def createBuffer = new StringBuilder
  @inline protected def addToBuffer(buf: Any, item: Char) = 
    buf.asInstanceOf[StringBuilder] + item
  @inline protected def convertFromBuffer(buf: Any) =
    buf.asInstanceOf[StringBuilder].toString
}

/* Chunked iterator based on buffered iterator using converter fn */
class ConverterChunkedBufferedIterator[A,B](
  private val iter: BufferedIterator[B],
  private val converter: Seq[B] => A
) extends ChunkedBufferedIterator[A,B](iter) {

  @inline protected def createBuffer: Any = Buffer[B]()
  @inline protected def addToBuffer(buf: Any, item: B) = 
    buf.asInstanceOf[Buffer[B]] :+ item
  @inline protected def convertFromBuffer(buf: Any) = 
    converter(buf.asInstanceOf[Seq[B]])
}

/* Chunked iterator based on Reader */
class ReaderChunkedIterator(
  private val reader: Reader
) extends ChunkedIterator[String, Char] {

  escape = '\\'

  var curChar: Char = 0
  var done = false

  @inline protected def internalHasNext: Boolean = {
    read()
    !done
  }

  @inline protected def internalHead: Char = {
    read()
    curChar
  }

  @inline protected def internalNext: Char = {
    read()
    val item = curChar
    curChar = 0
    item
  }

  @inline protected def createBuffer = new StringBuilder
  @inline protected def addToBuffer(buf: Any, item: Char) = 
    buf.asInstanceOf[StringBuilder] + item
  @inline protected def convertFromBuffer(buf: Any) =
    buf.asInstanceOf[StringBuilder].toString

  @inline protected def read() = {
    if (curChar == 0 && !done) {
      val data = reader.read()
      if (data == -1) done = true
      else curChar = data.asInstanceOf[Char]
    }
  }
}
