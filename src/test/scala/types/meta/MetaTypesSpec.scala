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
package test.scalafy.types.meta

import scalafy.types.meta._

import org.specs2.mutable.Specification

/** Test specification for meta data package */
object MetaTypesSpec extends Specification {

  "The meta types package" should {
    "support meta data" in {
      val t1 = Test1("foo")
      t1.addMeta('testit, 3)
      t1.getMeta('testit).mustEqual(Some(3))
      t1.removeMeta('testit)
      t1.getMeta('testit).mustEqual(None)
    }

    "support opaque data" in {
      implicit val testSettings = OpaqueDataSettings(true)
      
      val t1 = Test1("foo")
      OpaqueData.add(t1, Map('test -> "value"))
      OpaqueData.get(t1).mustEqual(Some(Map('test -> "value")))

      OpaqueData.remove(t1)
      OpaqueData.get(t1).mustEqual(None)

      // Reset
      implicit val opaqueDataSettings = OpaqueDataSettings(false)
      true.mustEqual(true)  // Keep specs happy
    }

    "clean up the ObjectCache properly" in {
      var t = Test1("foo")
      t.addMeta('testit, "foo")
      t = null
      Thread.sleep(100)
      System.gc
      ObjectCache.size.mustEqual(0) 
    }
  }
}

// Test Classes
case class Test1(s: String)
