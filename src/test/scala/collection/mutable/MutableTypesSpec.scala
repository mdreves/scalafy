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
package test.scalafy.collection.mutable

import org.specs2.mutable.Specification

import scalafy.collection.mutable._

/** Test specification for mutable collection package */
object MutableTypesSpec extends Specification {

  "The DefaultChunkedIterator" should {
    "support normal BufferedIterator functionality" in {
      val iter = DefaultChunkedIterator(
        "foo\" \\ bar 1 , 2 3".iterator.buffered)
      
      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('"')
      iter.hasNext
      iter.next.mustEqual(' ')
      iter.hasNext
      iter.next.mustEqual('\\')
    }

    "support chunking functionality" in {
      var iter = DefaultChunkedIterator(
        "fo\\\" \\ bar 1 , 2 3".iterator.buffered)
      iter.nextStop = '"'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('\\')
      iter.hasNext.mustEqual(false)
      iter.hasNextChunk.mustEqual(true)
      iter.nextChunk.mustEqual(Some(" \\ bar 1 , 2 3"))
      
      iter = DefaultChunkedIterator(
        "\"fo\\\" \\ bar 1 , 2 3\"".iterator.buffered)
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo\\"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(" \\ bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)

      iter = DefaultChunkedIterator(
        "foo\"bar\"bat".iterator.buffered)
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false)

      iter = DefaultChunkedIterator(
        "foo,bar,bat".iterator.buffered)
      iter.nextStop = ','

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false)
    }

    "support escaping functionality" in {
      var iter = DefaultChunkedIterator(
        "fo\\\" \\ bar 1 , 2 3".iterator.buffered)
      iter.escape = '\\'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('"')

      iter = DefaultChunkedIterator(
        "\"fo\\\" \\\\ bar 1 , 2 3\"".iterator.buffered)
      iter.escape = '\\'
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo\" \\ bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)
    }

    "support changing parameters during iteration" in {
      var iter = DefaultChunkedIterator(
        "{\"fo\\\"o\": \"bar\",\"foo2\": 1}".iterator.buffered)
      iter.escape = '\\'

      iter.hasNextChunk
      iter.head.mustEqual('{')
      iter.next
      iter.hasNextChunk
      iter.head.mustEqual('"')
      iter.next
      iter.nextStop = '"'
      var buf = new StringBuilder
      while (iter.hasNext) buf + iter.next
      buf.toString.mustEqual("fo\"o")
      iter.hasNextChunk
      iter.next
      iter.nextStop = 0
      iter.hasNextChunk
      iter.head.mustEqual(':')
    }
  }

  "The StringChunkedIterator" should {
    "support normal BufferedIterator functionality" in {
      val iter = StringChunkedIterator("foo\" \\ bar 1 , 2 3")
      
      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('"')
      iter.hasNext
      iter.next.mustEqual(' ')
      iter.hasNext
      iter.next.mustEqual('\\')
    }

    "support chunking functionality" in {
      var iter = StringChunkedIterator("fo\\\" \\ bar 1 , 2 3")
      iter.nextStop = '"'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('\\')
      iter.hasNext.mustEqual(false)
      iter.hasNextChunk.mustEqual(true)
      iter.nextChunk.mustEqual(Some(" \\ bar 1 , 2 3"))
      
      iter = StringChunkedIterator("\"fo\\\" \\ bar 1 , 2 3\"")
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo\\"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(" \\ bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)

      iter = StringChunkedIterator("foo\"bar\"bat")
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false)

      iter = StringChunkedIterator("foo,bar,bat")
      iter.nextStop = ','

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false) 
    }

    "support escaping functionality" in {
      var iter = StringChunkedIterator("fo\\\" \\ bar 1 , 2 3")
      iter.escape = '\\'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('"')

      iter = StringChunkedIterator("\"fo\\\" \\\\ bar 1 , 2 3\"")
      iter.escape = '\\'
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo\" \\ bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)
    }

    "support changing parameters during iteration" in {
      var iter = StringChunkedIterator("{\"fo\\\"o\": \"bar\",\"foo2\": 1}")
      iter.escape = '\\'

      iter.hasNextChunk
      iter.head.mustEqual('{')
      iter.next
      iter.hasNextChunk
      iter.head.mustEqual('"')
      iter.next
      iter.nextStop = '"'
      var buf = new StringBuilder
      while (iter.hasNext) buf + iter.next
      buf.toString.mustEqual("fo\"o")
      iter.hasNextChunk
      iter.next
      iter.nextStop = 0
      iter.hasNextChunk
      iter.head.mustEqual(':')
    }
  }
}
