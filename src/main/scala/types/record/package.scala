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
package scalafy.types

import scala.collection.generic.FilterMonadic

import scalafy.util._
import scalafy.util.converters._

/** Support for record types.
  *
  * A record type is a trait that can be mixed in to any object to provide Map
  * like features. The trait itself is marker interface. All the features are
  * provided by pimps.
  *
  * Records vs Uniform types vs Converters:
  * <li> Records should be used when you are only dealing with keys/values for 
  * one particular object at a time and it's possible/desireable to tag these
  * as records. Records will constrain you to only using keys that belong to
  * that particular object (else throws exception)
  * <li> If dealing with multiple objects/keys/values at a time and you wish 
  * to maintain a pure data form, use Uniform types instead. Uniform types
  * constrain you to only using primitive data forms, but you can add any
  * number of keys and convert to any compatible object when you are done.
  * <li> If dealing with multiple objects at a time and you need to deal with
  * the objects in either Map or object forms at different times, use
  * converters. Converters impose no constraints other than the types must be
  * convertable (either strictly or on a best-effort basis)
  *  
  * Some additional features for dealing with mixed types have been added:
  * getAsType, iteratorWithType[Type], withFilter[Type]. In addition the
  * ability to lookup values based on a sequence of keys into embedded objects
  * is also supported.
  *
  * Note that some operations like drop, take, etc require that an empty
  * constructor exist to create new objects. Also, since the implicit pimp
  * returns a record and not the object, for some operations it is necessary
  * to explcitly use type ascription in order to convert the result back to 
  * an object.
  *
  * The following is a summary of features:
  * {{{
  * case class Foo(s: String, i: Int, b: Boolean) extends Record
  * case class EmbeddedFoo(f: Foo) extends Record
  * val f = Foo("test", 10, false)
  * val ef = EmbeddedFoo(f)
  * 
  * // Value retrieval
  * f('s)                          // "test"
  * f.get('s)                      // Some("test"), type: Option[Any]
  * f.getAsType[String]('s)        // Some("test"), type: Option[String]
  * f.getAsType[Int]('s)           // None 
  * f.getAsType[Int]('i)           // Some(10), type: Option[Int]
  * ef.getAsType[Int]('f :: 'i :: Nil) // Some(10)
  *
  * // Iteration 
  * val iter = f.iterator
  * iter.next                      // ('s -> "test")
  * iter.next                      // ('i -> 10)
  * 
  * val iter = f.iteratorWithType[Int]
  * iter.next                      // ('i -> 10)
  * 
  * // Filtering
  * f.withFilter[Int].map(_._2 +1) // List(11)
  *
  * // Immutable updates
  * val f2 = f + ('i -> 2)         // FooRecord("test", 2, false) - e.g. wrapped
  * val f2: Foo = f + ('i -> 2)    // Foo("test", 2, false)
  * 
  * // Mutable updates
  * class Bar(var s: String) extends Record
  * var b = new Bar("test")
  * b('s) = "test2"                 // Bar("test2")
  * b += ('s -> "test3")            // Bar("test3")
  * 
  * // Using withDefault
  * f.withDefaultValue(5)('t)       // 5 
  * f.withDefault(x => "x")('t)     // "x"
  *
  * // Map operations
  * f.keySet                        // Set('s, 'i, 'b) 
  * f.head                          // ('s -> test)
  * f.last                          // ('b -> false)
  * f.isDefinedAt('s)               // true
  * f.isDefinedAt('t)               // false
  * f.toMap                         // Map('s -> test, 'i -> 10, 'b -> false)
  * ...
  *
  * // Using record as a function
  * List('s).map(f)                 // List("test")
  *
  * // Drop, Take, ... (requires class with empty constructor)
  * class Baz() extends Record {
  *   var s: String = "initial"
  *   var i: Int = 1
  * }
  * val b = new Baz()
  * b.s = "new-value"
  * b.i = 2
  * val b2: Baz = b.drop(1)         // Baz("initial", 2)
  * val b3: Baz = b.take(1)         // Baz("new-value", 1)
  * }}}
  */
package object record {

  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  /** Marker trait. Implement this trait to add record features */
  trait Record

  /** Record implementation */
  trait RecordLike[+A] extends Iterable[(Symbol, Any)] 
      with collection.Map[Symbol, Any] 
      with collection.MapLike[Symbol, Any, RecordLike[A]]
      with Product {

    val obj: A

    var defaultValueFn: (Seq[Symbol]) => Any = null


    // Special functions

    /** Gets value associated with field but only if matches type.
      *
      * Example:
      * {{{
      * case Foo(s: String, i: Int)
      * val f = Foo("test", 2)
      * f.getAsType[String]('s)    // Some("test"), type is Option[String]
      * f.getAsType[Int]('s)       // None
      * xm.getAsType[Any]('s)      // Some("test"), type is Option[Any] 
      * }}}
      */
    def getAsType[B1 : Manifest](key: Symbol): Option[B1] = getAsType(Seq(key)) 

    /** Get's value specified by list of keys into a multi-level object 
      *
      * Example:
      * {{{
      * case Foo(s: String, i: Int)
      * case Bar(foo: Foo)
      * val b = Bar(Foo("test", 2)) 
      * b.getAsType[Int]('foo :: 'i :: Nil)  // Some(2)
      * }}}
      */
    def getAsType[B1 : Manifest](keys: Seq[Symbol]): Option[B1] = {
      // Helper to check type
      def checkType(v: Any): Option[B1] = {
        if (manifest[B1] == manifest[Nothing] || manifest[B1] == manifest[Any]) 
          Some(v.asInstanceOf[B1])
        else if (toClass(v) == manifest[B1].erasure)
          Some(v.asInstanceOf[B1])
        else None
      }

      var leadPath = Vector[Symbol]() 
      var trailPath = keys
      var cur : Any = obj 

      while (trailPath.size > 0) {

        val record = new RecordLike[Any] { val obj = cur }
        record.get(trailPath.head) match {

          case Some(v) =>
            if (trailPath.size == 1) return checkType(v) 
            else if (!isPrimitiveType(v.getClass)) {
              cur = v
              leadPath :+= trailPath.head
              trailPath = trailPath.tail
            } else return None
        
          case None =>
            if (defaultValueFn != null) {
              leadPath :+= trailPath.head
              return checkType(defaultValueFn(leadPath))
            } else return None
        }
      }

      None
    }

    /** apply that takes multiple keys */
    def apply(keys: Seq[Symbol]): Any = getAsType[Any](keys) match {
      case Some(v) => v
      case None => throw new NoSuchElementException("key not found: " + keys)
    }
   
    /** get that takes multiple keys */
    def get(keys: Seq[Symbol]): Option[Any] = getAsType[Any](keys)

    /** Non strict filtering based on type. 
      *
      * NOTE: The method returns a List not UniformList
      *
      * Example:
      * {{{
      * val f = Foo("foo", 2)
      * f.withFilter[Int].map(_._2 + 1)   // List(3) 
      * }}}
      */
    def withFilter[
      B1 : Manifest
    ]: FilterMonadic[(Symbol,B1),RecordLike[B1]] = { 

      val p: ((Symbol,Any)) => Boolean = (kv) => !getAsType[B1](kv._1).isEmpty
      withFilter(p).asInstanceOf[FilterMonadic[(Symbol,B1),RecordLike[B1]]]
    }

    /** Filters obj fields based on type. 
      *
      * Example:
      * {{{
      * case Foo(s: String, i: Int)
      * val f = Foo("test", 2)
      * f.iteratorWithType[String].next  // ('s -> "test")
      * f.iteratorWithType[Int].next     // ('i -> 2)
      * }}}
      */  
    def iteratorWithType[B1 : Manifest]: Iterator[(Symbol, B1)] = 
      new Iterator[(Symbol, B1)] {
        val fields = obj.getClass.getDeclaredFields
        var offset = 0
        var nextValue: (Symbol, B1) = null
   
        def hasNext: Boolean = {
          if (nextValue != null) return true
          if (offset == fields.size) return false
   
          val key = Symbol(fields(offset).getName)
          getAsType[B1](key) match {
            case Some(v) => 
              nextValue = (key -> v)
              offset += 1
              true
            case None => 
              offset += 1
              hasNext
          }
        }
   
        def next(): (Symbol, B1) = {
          if (!hasNext) 
            throw new NoSuchElementException("next on empty iterator")
         
          val cur = nextValue
          nextValue = null
          cur
        }
      }


    // Required MapLike implementations

    def get(key: Symbol): Option[Any] = try {
      Some(obj.getClass.getMethod(key.name).invoke(obj))
    } catch {
      case e: NoSuchMethodException => None 
    }

    def iterator: Iterator[(Symbol, Any)] = iteratorWithType[Any]
 
    def + [B1 >: Any](kv: (Symbol, B1)): RecordLike[A] = {
      var xm = to[Map[Symbol,Any]](obj) match {
        case Some(v) => v
        case None => throw new Error("Unable to read fields from class")
      }
      
      xm += kv

      val manifest = new Manifest[A] { def erasure = obj.getClass }

      val newObj = to(xm, true)(manifest) match {
        case Some(v) => v
        case None => throw new Error("Unable to create new object")
      }

      new RecordLike[A] { val obj = newObj.asInstanceOf[A] }
    }
 
    def - (key: Symbol): RecordLike[A] = throw new Error("Not implemented")

    override def empty: RecordLike[A] = {
      val ctors = obj.getClass.getDeclaredConstructors.filter {
        ctor => ctor.getParameterTypes.size == 0 
      }
 
      if (ctors.isEmpty) 
        throw new Error("Not implemented: class must support empty constructor")
 
      new RecordLike[A] { 
        val obj = ctors(0).newInstance().asInstanceOf[A]
      }
    }


    // Mutable methods

    def += (kv: (Symbol, Any)): A = {
      val method = obj.getClass.getDeclaredMethods.find {
        m => m.getName == (kv._1.name + "_$eq")
      }
 
      if (method.isEmpty) {
        if (obj.getClass.getDeclaredFields.forall(f => f != kv._1.name))
          throw new Error("Field not modifiable: " + kv._1.name)
        else throw new NoSuchElementException(kv._1.name)
      }

      method.get.invoke(obj, toRefArray(Seq(kv._2)) : _*)
      obj 
    }

    def update(key: Symbol, value: Any): Unit = += (key, value)


    // toMap for easy conversion to immutable Map

    def toMap: Map[Symbol, Any] = to[Map[Symbol,Any]](obj, true).get


    // Product overrides

    def productArity : Int = size
   
    def productElement(n: Int): Any = iterator.drop(n).next._2


    // withDefault functions

    override def default(key: Symbol): Any = {
      if (defaultValueFn == null) 
        throw new NoSuchElementException("key not found: " + key)
      else defaultValueFn(Seq(key))
    }

    /** Note: Unlike Map withDefault the function takes a Seq of keys */
    def withDefault(d: (Seq[Symbol]) => Any): RecordLike[A] = {
      defaultValueFn = d
      this
    }

    def withDefaultValue(d: Any): RecordLike[A] = {
      defaultValueFn = (x => d)
      this
    }


    // General overrides
    
    override def equals(that: Any): Boolean = { 
      if (that == null) false
      else if (that == obj) true
      else if (that.isInstanceOf[RecordLike[_]]) 
        that.asInstanceOf[RecordLike[_]].obj == obj
      else if (isMapType(that.getClass))
        toMap.equals(that)
      else false
    }
    
    override def canEqual(that: Any): Boolean = { 
      (that != null && (that.isInstanceOf[RecordLike[_]] ||
        that.getClass == obj.getClass || isMapType(that.getClass)))
    }

    override def hashCode = obj.hashCode

    override def toString = {
      val split = obj.toString.split("\\(")
      if (split.length == 2) split(0) + "Record(" + split(1)
      else obj.toString + "Record"
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  implicit def record2RecordLike[A <: Record](x: A) = 
    new RecordLike[A] { val obj = x }
  
  implicit def recordLike2Record[A](r: RecordLike[A]): A = r.obj
 
} // end of package
