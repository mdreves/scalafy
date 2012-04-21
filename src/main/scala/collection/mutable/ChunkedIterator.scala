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

  /** Checks if iteration is complete */
  def hasNextChunk: Boolean

  /** Reads next chunk stopping at normal boundary (for example end of str) 
    *
    * If the normal boundary is not the end of the iterator, then the value
    * representing the boundary will not be returned with the chunk nor will
    * it be returned with any subsequent iteration operations 
    */
  def nextChunk(): Option[A]

  /** Reads next chunk stopping at given value.
    *
    * As per nextChunk the stop marker will not be returned with the chunk
    * nor will it be returned with any subsequent iteration operatons.
    */
  def nextChunk(stop: B): Option[A]

  /** Returns true if last chunk read was valid */
  def lastChunkValid(): Boolean 

  /** Returns number of values of type B read during iteration */
  def readCount(): Int

  override def toIterable: Iterable[B] = {
    val self = this

    new Iterable[B] { def iterator = self }
  }
}
