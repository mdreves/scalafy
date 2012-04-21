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
package scalafy.collection

import java.util.Date
import java.util.TimeZone

import scalafy.types.meta.OpaqueDataSettings
import scalafy.util._
import scalafy.util.converters.ConversionSettings

/** Uniform data types.
  *
  * Uniform data types are data types that only work with primitive types
  * or Lists/Maps of primitive types. Primitive types include String, Symbol,
  * Int, Short, Long, Float, Double, Boolean, Char, Byte, BigInt, BigDecimal,
  * Date, and TimeZone.
  *
  * Uniform types vs Records vs Converters:
  * <li> Uniform types should be used when working with multiple 
  * objects/keys/values at a time and you wish to maintain a pure data form.
  * Uniform types constrain you to only using primitive data forms, but you
  * can add any number of keys and convert to any compatible object when you
  * are done.
  * <li> If you are dealing with keys/values for one type of object at a time 
  * and it's possible/desireable to tag these as records, then Records should be
  * used. Records will constrain you to only using keys that belong to that
  * particular object (else throws exception)
  * <li> If dealing with multiple objects at a time and you need to deal with
  * the objects in either Map or object forms at different times, use
  * converters. Converters impose no constraints other than the types must be
  * convertable (either strictly or on a best-effort basis).
  *
  * The following is a summary of features:
  * {{{
  * // Objects to/from UniformData
  * case class Foo(s: String, i: Int)
  *
  * val u = toUniformMap(Foo("test", 1))  // UniformMap('s -> "test", 'i -> 1)
  * val foo = fromUniformMap[Foo](u)      // Some(Foo("test", 1))
  * u.get('s)                             // "test"
  * 
  * val u = toUniformList(List(Foo("x",1))// UniformList(Map('s -> "x",'i -> 1))
  * val list = fromUniformList(u)         // Some(List(Foo("x", 1)))
  *
  * // Working with UniformData types directly
  * var um = UniformMap('test -> "foo", 'test2 -> 2)
  * um += ('test3 -> true)        
  *
  * var ul = "foo" :: 2 :: UniformNil
  * var ul = UniformList("foo", 2)
  * ul += true
  *
  * // Filtering/retrieval based on value type
  * um.getAsType[Int]('test2)            // Some(2)
  * um.getAsType[Int]('test1)            // None 
  * ul.getAsType[Int](1)                 // Some(2) 
  * ul.getAsType[String](1)              // None 
  * um.filter[String]                    // UniformMap('test -> "foo")
  * ul.filter[String]                    // UniformList("foo")
  * ul.filter[String]                    // UniformList("foo")
  * ul.withFilter[Int].map(_ + 1)        // List(3) 
  * um.iteratorWithType[String].next     // 'test -> "foo" 
  *
  * // Multi-level retrieval using List of keys
  * val um = UniformMap('t -> UniformMap('t2 -> UniformMap('t3 -> "foo"))
  * um.get('t :: 't2 :: 't3 :: Nil)      // Some("foo")
  * um.get('t :: 't2 :: 't4 :: Nil)      // None
  * um('t :: 't2 :: 't3 :: Nil)          // "foo"
  *
  * // All the typical Map/List operations...
  * um.head                              // ('s -> "test")
  * um.last                              // ('i -> 1)
  * um.keySet                            // Set('s, 'i)
  * um.isDefinedAt('s)                   // true 
  * ul.head                              // "foo"
  * UniformMap('t -> 1) ++ List('t2 -> 2)   // UniformMap('t -> 1, 't2 -> 2) 
  * UniformList(1,2,3).filter[Int].map { _ + 1 } // UniformList(2,3,4)
  * ...
  *
  * // Working with Tuples (note tuples stored as UniformLists)
  * fromUniformList[List[Tuple2[String,Int]]](UniformList(
  *   UniformList("x",1), UniformList("y",2)))  // Some(List("x" -> 1,"y" -> 2))
  *
  * // Embedded Lists (embedded Lists must only contain primitives)
  * case class Foo(xs: List[Int])
  * val u = toUniformMap(Foo(List(1,2)))  // UniformMap('xs -> List(1,2))
  * fromUniformMap[Foo](u))               // Some(Foo(List(1,2)))
  *
  * // Embedded Maps (embedded Maps must only contain Symbol -> primitives)
  * case class Bar(xm: Map[Symbol,Int])
  * val u = toUniformMap(Bar(Map('t -> 2))) // UniformMap('xm -> Map('t -> 2))
  * fromUniformMap[Bar](u)                  // Some(Bar(Map('t -> 2)))
  * 
  * // Conversion to other types
  * fromUniformList[Vector[Int]](UniformList(1,2,3))// Some(Vector(1,2,3))
  * // Some(SortedMap("i" -> "2", "s" -> "test"))
  * fromUniformData[SortedMap[String,String]](Foo("test", 2))
  *
  * // Extraction
  * UniformList("foo", 2) match {
  *   case UniformList(h, tail @ _*) => println("head = " + h)
  *   case _ => println("no match")
  * }
  *
  * UniformMap('test -> "foo", 'test2 -> 2) match {
  *  case UniformMap(h, tail @ _*) => println("head key = " + h._1)
  *  case _ => println("no match")
  * }
  *
  * // Configuring default settings
  * implicit val uniformDataSettings = UniformDataSettings(
  *   ConversionSettings(
  *     false,                      // all or nothing conversions (default)
  *     OpaqueDataSettings(false)   // don't store opaque data (default)
  *   )
  * )
  * }}}
  */
package object uniform {

  ///////////////////////////////////////////////////////////////////////////
  // Settings 
  ///////////////////////////////////////////////////////////////////////////

  case class UniformDataSettings(conversionSettings: ConversionSettings)

  implicit val uniformDataSettings = UniformDataSettings(
    ConversionSettings(false, OpaqueDataSettings(false))
  )


  ///////////////////////////////////////////////////////////////////////////
  // Package Functions
  ///////////////////////////////////////////////////////////////////////////

  /** Converts from a UniformMap value to a given object/map. 
    *
    * If additional data is left over after the conversion and the 
    * storeOpaqueData settings is true, this the extra data will be cached as
    * opaque data for later retrieval when the value is converted back into a
    * uniform data type. This data is only tied to the original object and the
    * mapping will be lost if any additional transformation are done.
    *
    * Examples:
    * {{{
    * case class Foo(s: String, i: Int)
    * val u = toUniformMap(Foo("test", 1))
    * val foo = fromUniformMap[Foo](u)
    * }}}
    */
  def fromUniformMap[A](x: UniformMap[_])(
    implicit m: Manifest[A], settings: UniformDataSettings
  ): Option[A] = UniformDataParser.fromUniformData[A](x, settings) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }

  /** Converts from a UniformList value to a given seq/tuple. 
    *
    * Examples:
    * {{{
    * case class Foo(s: String, i: Int)
    * val u = toUniformList(List(Foo("test", 1)))
    * val foo = fromUniformList[List[Foo]](u)
    *
    * val u = toUniformList("test" -> 1)
    * val tuple = fromUniformList[Tuple2[String,Int]](u)
    * }}}
    */
  def fromUniformList[A](x: UniformList[_])(
    implicit m: Manifest[A], settings: UniformDataSettings
  ): Option[A] = 
      UniformDataParser.fromUniformData[A](x, settings) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }

  /** Converts from a UniformPrimitive value to an associated primitive value 
    *
    * Examples:
    * {{{
    * val str = fromUniformPrimitive(UniformString("test"))
    * }}}
    */
  def fromUniformPrimitive[A](x: UniformPrimitive[A])(
    implicit m: Manifest[A], settings: UniformDataSettings
  ): Option[A] = 
      UniformDataParser.fromUniformData[A](x, settings)(m) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }

  /** Converts from a UniformData value to given type.  */
  def fromUniformData[A](x: UniformData[_])(
    implicit m: Manifest[A], settings: UniformDataSettings
  ): Option[A] = 
      UniformDataParser.fromUniformData[A](x, settings)(m) match {
    case Right(r) => Some(r)
    case Left(l) => None
  }


  /** Converts an object/Map value into a UniformMap value. 
    *
    * If the storeOpaqueData setting is enabled and additional data was
    * stored when UnifiedMap data was originally converted to an object, that
    * data will be included in the output. 
    *
    * Examples:
    * {{{
    * case class Foo(s: String, i: Int)
    * val u = toUniformMap(Foo("test", 5))
    * }}}
    */
  def toUniformMap(x: Any)(
    implicit settings: UniformDataSettings
  ): Option[UniformMap[Any]] = {
    val u = UniformDataParser.toUniformData(x, settings)
    if (u.isInstanceOf[UniformMap[_]]) Some(u.asInstanceOf[UniformMap[Any]])
    else None
  }

  /** Converts a list/tuple of values into a UniformList value. 
    *
    * Examples:
    * {{{
    * case class Foo(s: String, i: Int)
    * val u = toUniformList(List(Foo("test", 1), Foo("test2", 2)))
    *
    * val u = toUniformList("test" -> 1)
    * }}}
    */
  def toUniformList(x: Any)(
    implicit settings: UniformDataSettings
  ): Option[UniformList[Any]] = {
    val u = UniformDataParser.toUniformData(x, settings)
    if (u.isInstanceOf[UniformList[_]]) Some(u.asInstanceOf[UniformList[Any]])
    else None
  }

  /** Converts a primitive value into a uniform primitive value. 
    *
    * Examples:
    * {{{
    * val u = toUniformPrimitive("a string")
    * val u = toUniformPrimitive(10)
    * }}}
    */
  def toUniformPrimitive[A, That](x: A)(implicit 
    notUsed: CanConvertTo[A, That], // allows to return proper type 
    settings: UniformDataSettings
  ): That = UniformDataParser.toUniformData(x, settings).asInstanceOf[That]

  /** Converts any value into a UniformData value.  */
  def toUniformData(x: Any)(
    implicit settings: UniformDataSettings
  ): UniformData[Any] = 
    UniformDataParser.toUniformData(x, settings)

  /** Checks if object is uniform primitive */
  def isUniformPrimitive(x: Any): Boolean = {
    isUniformPrimitiveClass(toClass(x))
  }

  /** Checks if object is uniform list (or can be converted to uniform list) */
  def isUniformList(x: Any): Boolean = {
    if (x.isInstanceOf[UniformList[_]]) return true 
    if (!x.isInstanceOf[List[_]]) return false
    for (v <- x.asInstanceOf[List[_]]) {
      val clazz = toClass(v)

      if (clazz == classOf[Map[_,_]]) {
        if (!isUniformMap(v.asInstanceOf[Map[_,_]])) return false
      } else if (clazz == classOf[List[_]]) {
        if (!isUniformList(v.asInstanceOf[List[_]])) return false
      } else if (!(isUniformPrimitiveClass(clazz) || 
         clazz == classOf[UniformMap[_]] || clazz == classOf[UniformList[_]])) {
        return false
      }
    }
    true
  }

  /** Checks if object is uniform map (or can be converted to uniform map) */
  def isUniformMap(x: Any): Boolean = {
    if (x.isInstanceOf[UniformMap[_]]) return true 
    if (!x.isInstanceOf[Map[_,_]]) return false
    for ((k, v) <- x.asInstanceOf[Map[_,_]]) {
      if (toClass(k) != classOf[UniformMapKey]) return false

      val clazz = toClass(v)

      if (clazz == classOf[Map[_,_]]) {
        if (!isUniformMap(v.asInstanceOf[Map[_,_]])) return false
      } else if (clazz == classOf[List[_]]) {
        if (!isUniformList(v.asInstanceOf[List[_]])) return false
      } else if (!(isUniformPrimitiveClass(clazz) || 
         clazz == classOf[UniformMap[_]] || clazz == classOf[UniformList[_]])) {
        return false
      }
    }
    true
  }


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  /** Primitive (String, Int, ..), List, or Map */
  sealed trait UniformData[+A] {
    
    override def equals(that: Any) = (canEqual(that) && that == this) 

    def canEqual(that: Any) = that != null && that.isInstanceOf[UniformData[_]]
  }

  /** Primitive
    *
    * One of: 
    *   String, Short, Int, Long, Float, Double, Boolean, Char, Byte, Symbol,
    *   BigInt, BigDecimal, Date, TimeZone
    *
    * Note, these are only defined for the sake of completeness. These would
    * only be used if the top level object itself is a primitive (rare). The
    * List and Map types do NOT use the UniformPrimitive types to wrap their
    * values, they store the actual primitive values.
    */
  trait UniformPrimitive[A] extends UniformData[A] {
    val value: A 

    override def toString = value.toString

    override def equals(that: Any) = 
      (canEqual(that) && that.asInstanceOf[UniformPrimitive[_]].value == value) 

    override def canEqual(that: Any) = 
      that != null && that.isInstanceOf[UniformPrimitive[_]]

    override def hashCode = value.hashCode
  }
  case class UniformString(value: String) extends UniformPrimitive[String]
  case class UniformSymbol(value: Symbol) extends UniformPrimitive[Symbol]
  case class UniformShort(value: Short) extends UniformPrimitive[Short]
  case class UniformInt(value: Int) extends UniformPrimitive[Int]
  case class UniformLong(value: Long) extends UniformPrimitive[Long]
  case class UniformFloat(value: Float) extends UniformPrimitive[Float]
  case class UniformDouble(value: Double) extends UniformPrimitive[Double]
  case class UniformBoolean(value: Boolean) extends UniformPrimitive[Boolean]
  case class UniformChar(value: Char) extends UniformPrimitive[Char]
  case class UniformByte(value: Byte) extends UniformPrimitive[Byte]
  case class UniformBigInt(value: BigInt) extends UniformPrimitive[BigInt]
  case class UniformBigDecimal(value: BigDecimal) 
    extends UniformPrimitive[BigDecimal]
  case class UniformDate(value: Date) extends UniformPrimitive[Date]
  case class UniformTimeZone(value: TimeZone) extends UniformPrimitive[TimeZone]

  /** Uniform List 
    *
    * This trait guarantees a List like data structure that only contains
    * primitives or Lists and Maps. It offers similar features to that
    * of a regular list, but it is not a List itself, so some operations behave
    * slightly differently than expected of a normal List (see implementation
    * notes). 
    *
    * A few special methods have been added in addition the normal List
    * methods: getAsType, filter[Type], withFilter[Type], and
    * iteratorWithType[Type]. The getAsType method returns only values for
    * indicies that exist and the indicies value type matches the specified
    * type. The filter[Type] method can be used to convert a UniformList[Any]
    * type into specific type by filtering out types that don't match the type
    * provided. This most often used when performing map, etc operations with a
    * specific type. The withFilter[Type] does non-strict filtering using
    * FilterMonadic, and iteratorWithType[Type] iterates only values that match
    * the given type.
    *
    * To allow :: to work seemlessly, a special UniformNil class is also
    * supported (e.g. "foo" :: UniformNil)
    *
    * Examples:
    * {{{
    * val xs = UniformList("foo", 1) // must be unified types 
    * 
    * // Cons operation
    * "foo" :: UniformNil      // UniformList("foo")
    *
    * xs.getAsType[String](0)  // Some("foo")
    * xs.getAsType[Int](1)     // Some(1) 
    * xs.getAsType[Int](0)     // None
    * xs.getAsType[String](1)  // None
    *
    * // Type based filtering
    * xs.filter[String]        // UniformList("foo")
    * xs.filter[Int]           // UniformList(1)
    * xs.withFilter[Int]       // FilterMonadic[Int, List[Int]]
    * xs.iteratorWithType[String].next // "foo" 
    *
    * // Type based filtering used in combination with other operations
    * xs.withFilter[Int].foldLeft(0){ (z, x) => z + x }    // 2
    * xs.filter[Int].copyToArray(new Array[Int](10))
    *
    * // Extraction
    * UniformList("foo", 1) match {
    *  case UniformList(h, tail @ _*) => println("head = " + h)
    *  case _ => println("no match")
    * }
    *
    * // All the list-like operations ...
    * xs(0)                                  // "foo"
    * xs(2)                                  // IndexOutOfBoundsException 
    * xs ++ List(1, 2)
    * List(1) ++ xs
    * ...
    * }}}
    * 
    *
    * Implementation Notes:
    *
    * In order to guarantee that only primitives and Lists and other Maps are
    * used we need support for disjunction types that Scala does not provide.
    * There are two options: (1) wrap all our primitivies into uniform versions
    * so that all types extend from UniformData or (2) use the "type class"
    * pattern to limit the types based on implicit evidence available at the
    * time of insertion that is constrainted to only be available for
    * primitives, etc. We have opted for (2) in order to avoid unnecessary
    * object wrapping. Since this is a List, we would have also liked to mixin
    * the LinearSeqLike[A] trait. The problem is that the drop, etc operations
    * for List that we need to override do not take implicit parameters. We can
    * overload them but that defeats the purpose as our goal is to not allow
    * them to be called unless an implicit value is available. If the non
    * implicit method is also available then this won't work. Unfortunately,
    * this means that this class can not mixin the List trait. However, we have
    * provided all the methods of List either exlicitly or via a pimp.
    *
    * Since UniformList is not a List, the builder pattern cannot be used to
    * create new collections. There were two alternatives: (1) change the result
    * type to be 'Any' and return a UniformList when possible else return the
    * type that would normally be returned for the embedded list value, or (2)
    * just return the type for the embedded List as is. We have opted for (2)
    * since this gives a type safe result. This means that in some cases, if
    * a UniformList is required after a transformation, it needs to be 
    * reconstructed manually. This effects the following methods:
    * <pre>
    *   collect
    *   flatMap
    *   map
    *   reverseMap
    *   scanLeft
    *   scanRight
    *   zip
    *   zipWithIndex
    * </pre>
    * Note that except for zip and zipWithIndex that produce pair types that
    * are not supported uniform types, using filter[Type] in combination with
    * these methods solves the problem.
    *
    * When working with operations that add values to an existing UniformList
    * the result will always be UniformList[Any]. Although a work around
    * exists whereby uniform list types such as UniformList[String], etc can 
    * return UniformLists's of their own types whenever additions are
    * done with their element type, these work arounds do not work with
    * UniformList[Any] so the approach was abandoned. This effects the 
    * following methods:
    * <pre>
    *   +:
    *   :+
    *   ::
    *   ++
    *   ++:
    *   :::
    *   intersect 
    *   patch
    *   reverse_:::
    *   union
    *   updated
    * </pre>
    *
    * The filter[Type] method can be used to convert back to a specific 
    * UniformList type if needed. Note also that the padTo method only allows
    * padding with elements of this UniformList's type unlike List.
    *
    * It would be nice to do pattern matching with UniformLists and ::, but
    * to do so would require support for implicit conversions (e.g. pimps)
    * in combination with pattern matching and Scala does not support this.
    * This means you cannot use UniformList(x,..) match { case h :: t ...}
    */
  trait UniformList[A] extends UniformData[List[A]]
      with UniformListLike[A] { //  would prefer LinearSeqLike[A]... 

    // UniformList specific methods

    /** Filters list based on type. 
      *
      * Example:
      * {{{
      * val xs = UniformList("foo", 2)
      * xs.filter[String]        // UniformList("foo")
      * xs.filter[Int]           // UniformList(2)
      * xs.filter[Any]           // UniformList("foo", 2)
      * }}}
      */
    def filter[A1](
      implicit m: Manifest[A1], ev: UniformType[A1]
    ): UniformList[A1] = {
      val xs = value.filter(typeFilter(m))
      new UniformList[A1] { val value = xs.asInstanceOf[List[A1]] }
    }

    /** Non strict filtering based on type. 
      *
      * NOTE: The method returns a List not UniformList
      *
      * Example:
      * {{{
      * val xs = UniformList("foo", 2)
      * xs.withFilter[Int].map(_ + 1)   // List(3) 
      * }}}
      */
    def withFilter[A1](implicit m: Manifest[A1], ev: UniformType[A1]) =
      value.withFilter(x => isType(x, m)).asInstanceOf[
        scala.collection.generic.FilterMonadic[A1, List[A1]]]


    /** Iteration based on type.
      *
      * Example:
      * {{{
      * val xs = UniformList("foo", 2)
      * xs.iteratorwithType[String].next  // "foo"
      * xs.iteratorwithType[Int].next     //  2
      * }}}
      */
    def iteratorWithType[A1](
      implicit m: Manifest[A1], ev: UniformType[A1]
    ): Iterator[A1] = new Iterator[A1] {
      var cur: Any = null
      val iter = value.iterator

      def hasNext = {
        if (cur != null) true
        else if (!iter.hasNext) false
        else {
          val nextCur = iter.next()
          if (isType(nextCur, m)) {
            cur = nextCur
            true
          } else hasNext
        }
      }
      
      def next() = {
        if (!hasNext) 
          throw new NoSuchElementException("next on empty iterator")

        val result = cur
        cur = null
        result.asInstanceOf[A1]
      }
    }

    /** Gets value associated with index but only if matches type.
      *
      * Example:
      * {{{
      * val xm = UniformList("foo", 2)
      * xm.getAsType[String](0)   // Some("foo"), type is Option[String]
      * xm.getAsType[Int](0)      // None
      * xm.getAsType[Any](0)      // Some("foo"), type is Option[Any] 
      * }}}
      */
    def getAsType[A1 : Manifest](
      index: Int 
    ): Option[A1] = try {
      val v = value(index)
      if (manifest[A1] == manifest[Nothing] || manifest[A1] == manifest[Any]) {
        if (v.isInstanceOf[Map[_,_]]) 
          Some((new UniformMap[Any] { 
            val value = v.asInstanceOf[Map[UniformMapKey, Any]]
          }).asInstanceOf[A1])
        else if (v.isInstanceOf[List[_]]) {
          Some((new UniformList[Any] { 
            val value = v.asInstanceOf[List[Any]] 
          }).asInstanceOf[A1])
        } else Some(v.asInstanceOf[A1])
      } else if (isType(v, manifest[A1])) Some(v.asInstanceOf[A1])
      else None
    } catch {
      case e: IndexOutOfBoundsException => None
    }


    // General overrides

    override def equals(that: Any) =
      (canEqual(that) && that.asInstanceOf[UniformList[_]].value.equals(value))

    override def canEqual(that: Any) = 
      that != null && that.isInstanceOf[UniformList[_]]

    override def hashCode = value.hashCode

    def stringPrefix = "UniformList"

    override def toString = "Uniform" + value.toString

    /** Returns immutable List instead of mutable */
    def toList = value


    // Helpers

    private[uniform] def createList[A1](
      xs: List[A1]
    ): UniformList[A1] =
      new UniformList[A1] { val value = xs }

    private[uniform] def createIterator[A1](
      i: Iterator[List[A1]]
    ): Iterator[UniformList[A1]] = new Iterator[UniformList[A1]]() {
      def hasNext = i.hasNext
      def next() = new UniformList[A1] { val value = i.next() }
    }
  }

  /** UniformList singleton */
  object UniformList extends UniformListLikeCompanion {

    private[uniform] def createList[A](
      xs: List[A]
    ): UniformList[A] =
      new UniformList[A] { val value = xs }

    private[uniform] def createIterator[A](
      i: Iterator[List[A]]
    ): Iterator[UniformList[A]] = {
      new Iterator[UniformList[A]]() {
        def hasNext = i.hasNext
        def next() = new UniformList[A] { val value = i.next() }
      }
    }
  }

  /** UniformList's equivalent of Nil */
  object UniformNil extends UniformList[Any] with Product with Serializable {
    private[uniform] val value = List[Any]()
  }


  /** Uniform Map 
    *
    * This trait guarantees a Map like data structure that only contains
    * primitives or Lists and Maps. It offers similar features to that
    * of a regular map, but it is not a Map itself so some operations behave
    * slightly differently than expected of a normal Map (see implementation
    * notes). 
    * 
    * A few special methods have been added in addition the normal Map methods:
    * getAsType, filter[Type], withFilter[Type], and iteratorWithType[Type].
    * The getAsType method is the same as get only it requires type
    * information. Only values for keys that exist and the value's type matches
    * the specified type will be be returned.  The filter[Type] method can be
    * used to convert a UniformMap[Any] type into specific type by filtering
    * out types that don't match the type provided.  This most often used when
    * performing map, etc operations with a specific type. The withFilter[Type]
    * does non-strict filtering by type using FilterMonadic and
    * iteratorWithType[Type] iterates over values of only the given type.
    *
    * Examples:
    * {{{
    * val xm = UniformMap('test -> "foo", 'test2 -> 1) // must be unified types 
    *
    * xm.getAsType[String]('test)  // Some("foo")
    * xm.getAsType[Int]('test2)    // Some(1) 
    * xm.getAsType[Int]('test)     // None
    * xm.getAsType[String]('test3) // None
    *
    * // Type based filtering
    * xm.filter[String]        // UniformMap('test -> "foo")
    * xm.filter[Int]           // UniformMap('test2 -> 1)
    * xm.withFilter[Int]       // FilterMonadic[(Symbol,Int),Map[Symbol,Int]]
    * xm.iteratorWithType[String].next // 'test -> "foo" 
    *
    * // Type based filtering used in combination with other operations
    * xm.withFilter[Int].foldLeft(0){ (z, kv) => z + kv._2 }    // 2
    * xm.filter[Int].copyToArray(new Array[(Symbol,Int)](10))
    *
    * // Extraction (as seq of key/value pairs)
    * UniformMap('test -> "foo", 'test2 -> 1) match {
    *  case UniformMap(h, tail @ _*) => println(h)     // ('test, "foo")
    *  case _ => println("no match")
    * }
    *
    * // All the map-like operations ...
    * xm('test)                                // "foo"
    * xm('test3)                               // NoSuchElementException
    * xm.get('test2)                           // Some(1) 
    * xm + ('test3 -> "bar", 'test4 -> true)   // must be unified types 
    * xm - ('test3)
    * xm ++ List('test3 -> 1, 'test4 -> 2)
    * List('test3 -> 1) ++ xm
    * ...
    * }}}
    * 
    *
    * Implementation Notes:
    *
    * In order to guarantee that only primitives and Lists and other Maps are
    * used we need support for disjunction types that Scala does not provide.
    * There are two options: (1) wrap all our primitivies into uniform versions
    * so that all types extend from UniformData or (2) use the "type class"
    * pattern to limit the types based on implicit evidence available at the
    * time of insertion that is constrainted to only be available for
    * primitives, etc. We have opted for (2) in order to avoid unnecessary
    * object wrapping. Since this is a Map, we would have also liked to mixin
    * the Map[A, B] trait.  The problem is that the +, -, etc operations for
    * Map that we need to override do not take implicit parameters. We can
    * overload them but that defeats the purpose as our goal is to not allow
    * the +, etc to be called unless an implicit value is available. If the non
    * implicit method is also available then this won't work. Unfortunately,
    * this means that this class can not mixin the Map trait. However, we have
    * provided all the methods of Map either exlicitly or via a pimp.
    *
    * Since UniformMap is not a Map, the builder pattern cannot be used to
    * create new collections. There were two alternatives: (1) change the result
    * type to be 'Any' and return a UniformMap when possible else return the
    * type that would normally be returned for the embedded map value, or (2)
    * just return the type for the embedded map as is. We have opted for (2)
    * since this gives a type safe result. This means that in some cases, if
    * a UniformMap is required after a transformation, it needs to be 
    * reconstructed manually. This effects the following methods:
    * <pre>
    *   collect
    *   flatMap
    *   map
    *   mapValues
    *   scanLeft
    *   scanRight
    *   zip
    *   zipWithIndex
    * </pre>
    * Note that using filter[Type] in combination with these methods solves
    * the problem.
    *
    * When working with operations that add values to an existing UniformMap
    * the result will always be UniformMap[Any]. Although a work around
    * exists whereby uniform map types such as Uniform[String], etc can 
    * work to return UniformMap's of their own types whenever additions are
    * done with their element type, these work arounds do not work with
    * UniformMap[Any] so the approach was abandoned. This effects the 
    * following methods:
    * <pre>
    *   +
    *   ++
    *   updated
    * </pre>
    * The filter[Type] method can be used to convert back to a specific 
    * UniformMap type if needed.
    */
  trait UniformMap[B] extends UniformData[Map[UniformMapKey, B]]
      with UniformMapLike[Symbol, B] { //  would prefer MapLike[A,B]... 

    // UniformMap specific methods

    /** Filters map based on type. 
      *
      * Example:
      * {{{
      * val xm = UniformMap('foo -> "foo", 'bar -> 2)
      * xm.filter[String]          // UniformMap('foo -> "foo")
      * xm.filter[Int]             // UniformMap('bar -> 2)
      * xm.filter[Any]             // UniformMap('foo -> "foo", 'bar -> 2)
      * }}}
      */
    def filter[B1](
      implicit m: Manifest[B1], ev: UniformType[B1]
    ): UniformMap[B1] = {
      val xm = value.filter(typeFilter(m))
      new UniformMap[B1] { val value = xm.asInstanceOf[Map[UniformMapKey, B1]] }
    }

    /** Non strict filtering based on type. 
      *
      * NOTE: The method returns a Map not UniformMap
      *
      * Example:
      * {{{
      * val xs = UniformMap('foo -> "foo", 'bar -> 2)
      * xs.withFilter[Int].map(_._2 + 1)   // Map('bar -> 3 
      * }}}
      */
    def withFilter[A1](implicit m: Manifest[A1], ev: UniformType[A1]) =
      value.withFilter(x => isType(x._2, m)).asInstanceOf[
        scala.collection.generic.FilterMonadic[
          (UniformMapKey,A1), Map[UniformMapKey,A1]]]

    /** Iteration based on type.
      *
      * Example:
      * {{{
      * val xm = UniformMap('foo -> "foo", 'bar -> 2)
      * xm.iteratorWithType[String].next  // 'foo -> "foo"
      * xm.iteratorWithType[Int].next     // 'bar -> 2
      * }}}
      */
    def iteratorWithType[B1](
      implicit m: Manifest[B1], ev: UniformType[B1]
    ): Iterator[(UniformMapKey, B1)] = new Iterator[(UniformMapKey, B1)] {
      var cur: Any = null
      val iter = value.iterator

      def hasNext = {
        if (cur != null) true
        else if (!iter.hasNext) false
        else {
          val nextCur = iter.next()
          if (isType(nextCur._2, m)) {
            cur = nextCur
            true
          } else hasNext
        }
      }
      
      def next() = {
        if (!hasNext) 
          throw new NoSuchElementException("next on empty iterator")

        val result = cur
        cur = null
        result.asInstanceOf[(UniformMapKey, B1)]
      }
    }

    /** Get's value specified by list of keys from a multi-level map 
      *
      * Example:
      * {{{
      * val xm = UniformMap('level1 -> UniformMap('level2 -> 3))
      * xm.get('level1 :: 'level2 :: Nil)    // Some(3) 
      * }}}
      */
    def get(keys: Seq[UniformMapKey]): Option[B] = try { 
      getAsType[Any](keys).asInstanceOf[Option[B]]
    } catch {
      case e => throw e
    }

    /** apply that takes multiple keys */
    def apply(keys: Seq[UniformMapKey]): B = getAsType[Any](keys) match {
      case Some(v) => v.asInstanceOf[B] 
      case None => throw new NoSuchElementException("key not found: " + keys)
    }

    /** Gets value associated with key but only if matches type.
      *
      * Example:
      * {{{
      * val xm = UniformMap('foo -> "foo", 1 -> 2)
      * xm.getAsType[String]('foo)   // Some("foo"), type is Option[String]
      * xm.getAsType[Int]('foo)      // None
      * xm.getAsType[Any]('foo)      // Some("foo"), type is Option[Any] 
      * }}}
      */
    def getAsType[B1 : Manifest](
      key: UniformMapKey
    ): Option[B1] = getAsType(Seq(key))

    /** Get's value specified by list of keys from a multi-level map 
      *
      * Example:
      * {{{
      * val xm = UniformMap('level1 -> UniformMap('level2 -> 3))
      * xm.getAsType[Int]('level1 :: 'level2 :: Nil)   // Some(3)
      * }}}
      */
    def getAsType[B1 : Manifest](keys: Seq[UniformMapKey]): Option[B1] = {
      // Helper to check type
      def checkType(v: Any): Option[B1] = {
        if (manifest[B1] == manifest[Nothing] || 
            manifest[B1] == manifest[Any]) { 
          if (v.isInstanceOf[Map[_,_]]) 
            Some((new UniformMap[Any] { 
              val value = v.asInstanceOf[Map[UniformMapKey, Any]]
            }).asInstanceOf[B1])
          else if (v.isInstanceOf[List[_]]) {
            Some((new UniformList[Any] { 
              val value = v.asInstanceOf[List[Any]] 
            }).asInstanceOf[B1])
          } else Some(v.asInstanceOf[B1])
        } else if (isType(v, manifest[B1])) Some(v.asInstanceOf[B1])
        else None
      }

      var leadPath = Vector[Symbol]() 
      var trailPath = keys
      var cur = value.asInstanceOf[scala.collection.Map[Any, Any]]

      while (trailPath.size > 0) {
        cur.get(trailPath.head) match {
          case Some(v) =>
            if (trailPath.size == 1) return checkType(v) 
            else if (v.isInstanceOf[scala.collection.Map[_,_]]) {
              cur = v.asInstanceOf[scala.collection.Map[Any, Any]]
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


    // General overrides

    override def equals(that: Any) =
      (canEqual(that) && that.asInstanceOf[UniformMap[_]].value.equals(value))

    override def canEqual(that: Any) = 
      that != null && that.isInstanceOf[UniformMap[_]]

    override def hashCode = value.hashCode

    def stringPrefix = "UniformMap"

    override def toString = "Uniform" + value.toString

    /** Returns immutable map instead of mutable */
    def toMap = value


    // Helpers

    private[uniform] def createMap[B1](
      xm: Map[UniformMapKey,B1]
    ): UniformMap[B1] =
      new UniformMap[B1] { val value = xm }

    private[uniform] def createIterator[B1](
      i: Iterator[Map[UniformMapKey, B1]]
    ): Iterator[UniformMap[B1]] = new Iterator[UniformMap[B1]]() {
      def hasNext = i.hasNext
      def next() = new UniformMap[B1] { val value = i.next() }
    }
  }


  /** UniformMap singleton */
  object UniformMap extends UniformMapLikeCompanion[UniformMapKey] {

    private[uniform] def createMap[B](
      xm: Map[UniformMapKey,B]
    ): UniformMap[B] =
      new UniformMap[B] { val value = xm }

    private[uniform] def createIterator[B](
      i: Iterator[Map[UniformMapKey,B]]
    ): Iterator[UniformMap[B]] = {
      new Iterator[UniformMap[B]]() {
        def hasNext = i.hasNext
        def next() = new UniformMap[B] { val value = i.next() }
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  implicit def uniformList2List[A](xs: UniformList[A]) = xs.value 
  
  implicit def uniformMap2Map[B](xm: UniformMap[B]) = xm.value 

  
  ///////////////////////////////////////////////////////////////////////////
  // Types
  ///////////////////////////////////////////////////////////////////////////

  // Sealed classes for type constraints using "type class" pattern
  sealed trait UniformType[A]
  implicit object UniformStringType extends UniformType[String]
  implicit object UniformSymbolType extends UniformType[Symbol]
  implicit object UniformIntType extends UniformType[Int]
  implicit object UniformShortType extends UniformType[Short]
  implicit object UniformLongType extends UniformType[Long]
  implicit object UniformFloatType extends UniformType[Float]
  implicit object UniformDoubleType extends UniformType[Double]
  implicit object UniformBooleanType extends UniformType[Boolean]
  implicit object UniformCharType extends UniformType[Char]
  implicit object UniformByteType extends UniformType[Byte]
  implicit object UniformBigIntType extends UniformType[BigInt]
  implicit object UniformBigDecimalType extends UniformType[BigDecimal]
  implicit object UniformDateType extends UniformType[Date]
  implicit object UniformTimeZoneType extends UniformType[TimeZone]
  implicit def toUniformListType[A : UniformType] = 
    new UniformType[UniformList[A]] {}
  // Special support for UniformList[Any] since no UniformType[Any] 
  implicit object UniformListAnyType extends UniformType[UniformList[Any]]
  implicit def toUniformMapType[A : UniformType] = 
    new UniformType[UniformMap[A]] {}
  // Special support for UniformMap[Any] since no UniformType[Any] 
  implicit object UniformMapAnyType extends UniformType[UniformMap[Any]]
  implicit def toUniformDataType[A : UniformType] = 
    new UniformType[UniformData[A]] {}
  // Special support for UniformData[Any] since no UniformType[Any] 
  implicit object UniformDataAnyType extends UniformType[UniformData[Any]]
  // Any Seq 
  //   implicit def uniformSeq[
  //     T : UniformType, U[X] <: Seq[X]]: UniformType[U[T]]
  //   ] = new UniformType[U[T]] {}


  // The CanConvertTo type is similar to the CanBuildFrom pattern only
  // the builder function is not used. This is only used so we can return
  // a proper type for the toPrimitiveType. Ideally we would like to use this 
  // pattern and have only a toUniformData function, but it won't work for Any
  sealed trait CanConvertTo[-From, +To] extends AnyRef
  implicit def canConvertToString: CanConvertTo[String, UniformString] =
    new CanConvertTo[String, UniformString] {}
  implicit def canConvertToSymbol: CanConvertTo[Symbol, UniformSymbol] =
    new CanConvertTo[Symbol, UniformSymbol] {}
  implicit def canConvertToInt: CanConvertTo[Int, UniformInt] =
    new CanConvertTo[Int, UniformInt] {}
  implicit def canConvertToShort: CanConvertTo[Short, UniformShort] =
    new CanConvertTo[Short, UniformShort] {}
  implicit def canConvertToLong: CanConvertTo[Long, UniformLong] =
    new CanConvertTo[Long, UniformLong] {}
  implicit def canConvertToFloat: CanConvertTo[Float, UniformFloat] =
    new CanConvertTo[Float, UniformFloat] {}
  implicit def canConvertToDouble: CanConvertTo[Double, UniformDouble] =
    new CanConvertTo[Double, UniformDouble] {}
  implicit def canConvertToBoolean: CanConvertTo[Boolean, UniformBoolean] =
    new CanConvertTo[Boolean, UniformBoolean] {}
  implicit def canConvertToChar: CanConvertTo[Char, UniformChar] =
    new CanConvertTo[Char, UniformChar] {}
  implicit def canConvertToByte: CanConvertTo[Byte, UniformByte] =
    new CanConvertTo[Byte, UniformByte] {}
  implicit def canConvertToBigInt: CanConvertTo[BigInt, UniformBigInt] =
    new CanConvertTo[BigInt, UniformBigInt] {}
  implicit def canConvertToBigDecimal
    : CanConvertTo[BigDecimal, UniformBigDecimal] =
      new CanConvertTo[BigDecimal, UniformBigDecimal] {}
  implicit def canConvertToDate: CanConvertTo[Date, UniformDate] =
    new CanConvertTo[Date, UniformDate] {}
  implicit def canConvertToTimeZone: CanConvertTo[TimeZone, UniformTimeZone] =
    new CanConvertTo[TimeZone, UniformTimeZone] {}


  // Existential type so we can have same type for all existential types
  type UniformExistential = A forSome { type A }
  type UniformMapKey = Symbol


  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  private[scalafy] def toPrimitiveManifestFromUniform(m: Manifest[_]) = {
    if (m == manifest[UniformString] || 
        m == manifest[UniformPrimitive[String]] ||
        m == manifest[UniformData[String]]) 
      manifest[String]
    else if (m == manifest[UniformSymbol] || 
         m == manifest[UniformPrimitive[Symbol]] ||
         m == manifest[UniformData[Symbol]])
      manifest[Symbol]
    else if (m == manifest[UniformInt] || 
        m == manifest[UniformPrimitive[Int]] ||
        m == manifest[UniformData[Int]])
      manifest[Int]
    else if (m == manifest[UniformShort] ||
        m == manifest[UniformPrimitive[Short]] ||
        m == manifest[UniformData[Short]])
      manifest[Short]
    else if (m == manifest[UniformLong] || 
        m == manifest[UniformPrimitive[Long]] ||
        m == manifest[UniformData[Long]])
      manifest[Long]
    else if (m == manifest[UniformFloat] || 
        m == manifest[UniformPrimitive[Float]] ||
        m == manifest[UniformData[Float]])
      manifest[Float]
    else if (m == manifest[UniformDouble] || 
        m == manifest[UniformPrimitive[Double]] ||
        m == manifest[UniformData[Double]])
      manifest[Double]
    else if (m == manifest[UniformBoolean] || 
        m == manifest[UniformPrimitive[Boolean]] ||
        m == manifest[UniformData[Boolean]])
      manifest[Boolean]
    else if (m == manifest[UniformChar] || 
        m == manifest[UniformPrimitive[Char]] ||
        m == manifest[UniformData[Char]])
      manifest[Char]
    else if (m == manifest[UniformByte] ||
        m == manifest[UniformPrimitive[Byte]] ||
        m == manifest[UniformData[Byte]])
      manifest[Byte]
    else if (m == manifest[UniformBigInt] ||
        m == manifest[UniformPrimitive[BigInt]] ||
        m == manifest[UniformData[BigInt]])
      manifest[BigInt]
    else if (m == manifest[UniformBigDecimal] ||
        m == manifest[UniformPrimitive[BigDecimal]] ||
        m == manifest[UniformData[BigDecimal]])
      manifest[BigDecimal]
    else if (m == manifest[UniformDate] ||
        m == manifest[UniformPrimitive[Date]] ||
        m == manifest[UniformData[Date]])
      manifest[Date]
    else if (m == manifest[UniformTimeZone] ||
        m == manifest[UniformPrimitive[TimeZone]] ||
        m == manifest[UniformData[TimeZone]])
      manifest[TimeZone]
    else throw new Error("not a uniform primitive manifest")
  }

  private[scalafy] def isUniformPrimitiveClass(clazz: Class[_]): Boolean = {
    if (clazz == classOf[String]) true 
    else if (clazz == classOf[Symbol]) true
    else if (clazz == classOf[Int]) true 
    else if (clazz == classOf[Short]) true 
    else if (clazz == classOf[Long]) true 
    else if (clazz == classOf[Float]) true 
    else if (clazz == classOf[Double]) true 
    else if (clazz == classOf[Boolean]) true 
    else if (clazz == classOf[Char]) true 
    else if (clazz == classOf[Byte]) true 
    else if (clazz == classOf[BigInt]) true 
    else if (clazz == classOf[BigDecimal]) true 
    else if (clazz == classOf[Date]) true 
    else if (clazz == classOf[TimeZone]) true 
    else if (classOf[UniformPrimitive[_]].isAssignableFrom(clazz)) true 
    else false 
  }

  private[uniform] def toManifest(clazz: Class[_]): Manifest[_] = {
    toPrimitiveManifest(clazz) match {
      case Some(m) => m
      case _ =>
        if (clazz == classOf[UniformList[_]]) 
          manifest[List[Any]]
        else if (clazz == classOf[UniformMap[_]]) 
          manifest[Map[UniformMapKey,Any]]
        else if (isSeqType(clazz)) 
          createSeqManifest[Any](clazz) match {
            case Right(r) => r
            case Left(l) => manifest[Any] 
          }
        else if (isMapType(clazz)) 
          createMapManifest[UniformMapKey, Any](clazz) match {
            case Right(r) => r
            case Left(l) => manifest[Any] 
          }
        else manifest[Any]
    }
  }

} // end of package
