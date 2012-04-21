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

/** String chunked iterator */
class StringChunkedIterator(private val str: String) 
    extends ChunkedIterator[String,Char] {

  private var charsRead = 0
  private var stopped = false 
  private var chunkValid = false 

  // Iterator methods

  def hasNext: Boolean = {
    if (charsRead >= str.length) false
    else if (isEscape(str(charsRead))) { // skip over escape
      charsRead += 1
      (charsRead < str.length)
    } else if (isOnStopMarker(nextStop)) {
      chunkValid = true
      stopped = true
      false  // stop on chunk boundaries
    }
    else true
  }

  def next(): Char = {
    if (charsRead >= str.length)
      throw new NoSuchElementException("next on empty iterator")

    chunkValid = false
    charsRead += 1
    str(charsRead - 1)
  }

  // BufferedIterator methods

  def head: Char = {
    if (charsRead >= str.length)
      throw new NoSuchElementException("next on empty iterator")

    str(charsRead)
  }

  // ChunkedIterator methods

  def hasNextChunk: Boolean = charsRead < str.length

  def nextChunk(): Option[String] = {
    if (nextStop == 0) {
      if (charsRead >= str.length)
        throw new NoSuchElementException("next on empty iterator")

      val buf = new StringBuffer
      while (charsRead < str.length) {
        val c = str(charsRead)
        charsRead += 1
        if (isEscape(c)) {
          if (charsRead < str.length) {
            buf.append(str(charsRead))
            charsRead += 1
          }
        } else buf.append(c) 
      }
      chunkValid = true
      stopped = true
      Some(buf.toString)
    } else {
      nextChunk(nextStop)
    }
  }

  def nextChunk(stop: Char): Option[String] = {
    if (charsRead >= str.length)
      throw new NoSuchElementException("next on empty iterator")

    // skip over chunk boundary
    if (stopped && isOnStopMarker(nextStop)) charsRead += 1

    val buf = new StringBuilder
    while (charsRead < str.length && !isOnStopMarker(stop)) {
      val c = str(charsRead)
      charsRead += 1
      if (isEscape(c)) {
        if (charsRead < str.length) {
          buf.append(str(charsRead))
          charsRead += 1
        }
      } else buf.append(c)
    }

    if (!stopRequired || charsRead < str.length && isOnStopMarker(stop))  {
      chunkValid = true
      stopped = true
      Some(buf.toString)
    } else {
      chunkValid = false
      None
    }
  }

  def lastChunkValid() = chunkValid

  def readCount() = charsRead + 1

  // Helpers

  /** Checks if char at index is stop char */
  protected def isOnStopMarker(stop: Char): Boolean =
    (stop != 0 && str(charsRead) == stop)

  /** Return true if the current character has been escaped.
    * 
    * By default escaping is checked based on \ characters
    */
  protected def isEscape(c: Char): Boolean =
    (escape != 0 && c == escape)
}

object StringChunkedIterator {
  def apply(s: String): StringChunkedIterator = new StringChunkedIterator(s)

  def apply(s: String, escape: Char): StringChunkedIterator = { 
    val result = new StringChunkedIterator(s)
    result.escape = escape
    result
  }
}
