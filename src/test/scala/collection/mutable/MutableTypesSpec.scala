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

import java.io.StringReader

import org.specs2.mutable.Specification

import scalafy.collection.mutable._

/** Test specification for mutable collection package */
object MutableTypesSpec extends Specification {

  "The IdentityHashMap" should {
    "support adding objects that are equal" in {
      var t = Test1("foo")
      var xm = IdentityHashMap[Any, Any]()
      xm += (t -> 1)
      xm.get(t).mustEqual(Some(1))
      var t2 = Test1("foo")
      xm += (t2 -> 2)
      xm.size.mustEqual(2)
    }
  }
 
  "The WeakIdentityHashMap" should {
    "support adding objects that are equal" in {
      var t = Test1("foo")
      var xm = WeakIdentityHashMap[AnyRef, Any]()
      xm += (t -> 1)
      xm.get(t).mustEqual(Some(1))
      var t2 = Test1("foo")
      xm += (t2 -> 2)
      xm.size.mustEqual(2)
    }

    "remove unused references" in {
      var t = Test1("foo")
      var xm = WeakIdentityHashMap[AnyRef, Any]()
      xm += (t -> 1)
      xm.size.mustEqual(1) 
      t = null
      Thread.sleep(100)
      System.gc
      xm.size.mustEqual(0) 
    }
  }
/*
  "The StringChunkedIterator" should {
    "support normal BufferedIterator functionality" in {
      val iter = ChunkedIterator("foo\" \\\\ bar 1 , 2 3")
      
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
      var iter = ChunkedIterator("fo\" bar 1 , 2 3")
      iter.nextStop = '"'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext.mustEqual(false)
      iter.hasNextChunk.mustEqual(true)
      iter.nextChunk.mustEqual(Some(" bar 1 , 2 3"))
      
      iter = ChunkedIterator("\"fo\" bar 1 , 2 3\"")
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(" bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)

      iter = ChunkedIterator("foo\"bar\"bat")
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false)

      iter = ChunkedIterator("foo,bar,bat")
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
      var iter = ChunkedIterator("fo\\\" \\ bar 1 , 2 3")
      iter.escape = '\\'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('"')

      iter = ChunkedIterator("\"fo\\\" \\\\ bar 1 , 2 3\"")
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
      var iter = ChunkedIterator("{\"fo\\\"o\": \"bar\",\"foo2\": 1}")
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

  "The ChunkedBufferedIterator" should {
    "support normal BufferedIterator functionality" in {
      val iter = ChunkedIterator(
        "foo\" \\\\ bar 1 , 2 3".iterator.buffered)
      
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
      var iter = ChunkedIterator(
        "fo\" bar 1 , 2 3".iterator.buffered)
      iter.nextStop = '"'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext.mustEqual(false)
      iter.hasNextChunk.mustEqual(true)
      iter.nextChunk.mustEqual(Some(" bar 1 , 2 3"))
      
      iter = ChunkedIterator(
        "\"fo\" bar 1 , 2 3\"".iterator.buffered)
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(" bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)

      iter = ChunkedIterator(
        "foo\"bar\"bat".iterator.buffered)
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false)

      iter = ChunkedIterator(
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
      var iter = ChunkedIterator(
        "fo\\\" \\ bar 1 , 2 3".iterator.buffered)
      iter.escape = '\\'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('"')

      iter = ChunkedIterator(
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
      var iter = ChunkedIterator(
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

  "The ReaderChunkedIterator" should {
    "support normal BufferedIterator functionality" in {
      val reader = new StringReader("foo\" \\\\ bar 1 , 2 3")
      val iter = ChunkedIterator(reader)
      
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
      reader.close()
      true.mustEqual(true) // keep specs happy
    }

    "support chunking functionality" in {
      var reader = new StringReader("fo\" bar 1 , 2 3")
      var iter = ChunkedIterator(reader)
      iter.nextStop = '"'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext.mustEqual(false)
      iter.hasNextChunk.mustEqual(true)
      //println(java.util.Arrays.toString(iter.nextChunk.get.toCharArray))
      iter.nextChunk.mustEqual(Some(" bar 1 , 2 3"))
     
      reader.close()

      reader = new StringReader("\"fo\" bar 1 , 2 3\"")
      iter = ChunkedIterator(reader)
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(" bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)

      reader.close()

      reader = new StringReader("foo\"bar\"bat")
      iter = ChunkedIterator(reader)
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false)

      reader.close()

      reader = new StringReader("foo,bar,bat")
      iter = ChunkedIterator(reader)
      iter.nextStop = ','

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("foo"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bar"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("bat"))
      iter.hasNextChunk.mustEqual(false) 

      reader.close()
      true.mustEqual(true) // keep specs happy
    }

    "support escaping functionality" in {
      var reader = new StringReader("fo\\\" \\ bar 1 , 2 3")
      var iter = ChunkedIterator(reader)
      iter.escape = '\\'

      iter.hasNext
      iter.next.mustEqual('f')
      iter.hasNext
      iter.next.mustEqual('o')
      iter.hasNext
      iter.next.mustEqual('"')

      reader.close()

      reader = new StringReader("\"fo\\\" \\\\ bar 1 , 2 3\"")
      iter = ChunkedIterator(reader)
      iter.escape = '\\'
      iter.nextStop = '"'

      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some("fo\" \\ bar 1 , 2 3"))
      iter.hasNextChunk
      iter.nextChunk.mustEqual(Some(""))
      iter.hasNextChunk.mustEqual(false)
      
      reader.close()
      true.mustEqual(true) // keep specs happy
    }

    "support changing parameters during iteration" in {
      val reader = new StringReader("{\"fo\\\"o\": \"bar\",\"foo2\": 1}")
      val iter = ChunkedIterator(reader)
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
      reader.close()
      true.mustEqual(true) // keep specs happy
    }
  } */ 
}

// Test Classes
case class Test1(s: String)
