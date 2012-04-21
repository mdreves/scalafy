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

/** Contains extractors for basic types.
  *
  * The following is a summary of features:
  * {{{
  * // Extracting lists of basic types
  * x match {
  *   // Matching on basic types
  *   case ListOfString(xs @ _*) =>
  *     println("ListOfString: " + xs)
  *   case ListOfInt(xs @ _*) =>
  *     println("ListOfInt: " + xs)
  *   case ListOfShort(xs @ _*) =>
  *     println("ListOfShort: " + xs)
  *   case ListOfLong(xs @ _*) =>
  *     println("ListOfLong: " + xs)
  *   case ListOfFloat(xs @ _*) =>
  *     println("ListOfFloat: " + xs)
  *   case ListOfDouble(xs @ _*) =>
  *     println("ListOfDouble: " + xs)
  *   case ListOfBoolean(xs @ _*) =>
  *     println("ListOfBoolean: " + xs)
  *   case ListOfChar(xs @ _*) =>
  *     println("ListOfChar: " + xs)
  *   case ListOfByte(xs @ _*) =>
  *     println("ListOfByte: " + xs)
  *   ...
  *   // Matching in combination with List
  *   case List(ListOfString(xs @ _*)) =>
  *     println("ListOfListOfString: " + xs) // verifies first only
  *   ...
  *   case _ =>
  *     println("no match")
  * }
  *
  * // Extracting Maps of basic types
  * x match {
  *   // Matching String -> ...
  *   case MapOfStringToString(kvs) =>
  *     println("MapOfStringToString: " + kvs)
  *   case MapOfStringToInt(kvs) =>
  *     println("MapOfStringToInt: " + kvs)
  *   case MapOfStringToShort(kvs) =>
  *     println("MapOfStringToShort: " + kvs)
  *   case MapOfStringToLong(kvs) =>
  *     println("MapOfStringToLong: " + kvs)
  *   case MapOfStringToLong(kvs) =>
  *     println("MapOfStringToLong: " + kvs)
  *   ...
  *   // Matching Int -> ...
  *   case MapOfIntToString(kvs) =>
  *     println("MapOfIntToString: " + kvs)
  *   ...
  *   // Matching in combination with Map
  *   case Map(String(s) -> MapOfStringToInt(kvs)) =>
  *     println("MapOfStringToMapOfStringToInt: " + s + " -> " + kvs)
  *   ..
  *   case _ =>
  *     println("no match")
  * }
  * }}}
  */
package object extractors {

  /** Basic extractor trait
    *
    * Defines extrator for basic types.
    *
    * @author Mike Dreves
    */
  trait BasicExtractor {
    type A

    /** Checks if proper type (must do in concrete class due to type erasure) */
    def isType(x: Any): Boolean

    /** Conversion from string to type */
    def toType(s: String): Option[A]

    /** Extractor for type itself and types that can be converted from String */
    def unapply(x: Any): Option[A] = 
      if (isType(x)) return Some(x.asInstanceOf[A])
      else if (x.isInstanceOf[String]) {
        try {
          return toType(x.asInstanceOf[String])
        } catch {
          case _ => return None
        }
      }
      else None
  }

  /** List[A] extractor
    *
    * Verifies that a list contains only type A during pattern matching.
    * It should be used in places where List[A] would normally be used
    * but can't be due to type erasure. Concrete versions of this trait
    * can be found within this package (e.g.  [[scalafy.types.extractors.ListOfString]],
    * etc). The extraction operation is O(n).
    *
    * @author Mike Dreves
    */
  trait ListOfA {
    type A

    /** Check if proper instance of A
      *
      * Due to type erasure, we can't check isInstanceOf[A]. This method
      * delegates the check to the class or object that mixes in this trait.
      */
    def isInstanceOfA(x: Any): scala.Boolean

    def unapplySeq(xs: List[_]): Option[Seq[A]] =
      if (xs forall { isInstanceOfA(_) })
        Some(xs.asInstanceOf[List[A]])
      else
        None
  }

  /** List[String] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfString extends ListOfA {
    type A = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
  }

  /** List[Int] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfInt extends ListOfA {
    type A = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
  }

  /** List[Short] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfShort extends ListOfA {
    type A = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
  }

  /** List[Long] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfLong extends ListOfA {
    type A = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
  }

  /** List[Float] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfFloat extends ListOfA {
    type A = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
  }

  /** List[Double] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfDouble extends ListOfA {
    type A = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
  }

  /** List[Boolean] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfBoolean extends ListOfA {
    type A = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
  }

  /** List[Char] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfChar extends ListOfA {
    type A = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
  }

  /** List[Byte] extractor
    *
    * See [[scalafy.types.extractors.ListOfA]]
    *
    * @author Mike Dreves
    */
  object ListOfByte extends ListOfA {
    type A = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[A, B] extractor
    *
    * Verifies that a map contains only (A, B) pairs during pattern matching.
    * It should be used in places where Map[A, B] would normally be used but
    * can't be due to type erasure. Concrete version of this trait can be
    * found within this package (e.g.
    * [[scalafy.types.extractors.MapOfStringToString]], etc). Those objects
    * (and this trait) are expected to be used in combination with the ->
    * object and as an alternative to the more general Map object defined in
    * this package (although the Map object can be used in combination with
    * this one). Note that extractor returns a key/value sequence not a Map.
    * The extraction operation is O(n).
    *
    * @author Mike Dreves
    */
  trait MapOfAToB {
    type A
    type B

    /** Check if proper instance of A
      *
      * Due to type erasure, we can't check isInstanceOf[A]. This method
      * delegates the check to the class or object that mixes in this trait.
      */
    def isInstanceOfA(x: Any): scala.Boolean

    /** Check if proper instance of B
      *
      * Due to type erasure, we can't check isInstanceOf[B]. This method
      * delegates the check to the class or object that mixes in this trait.
      */
    def isInstanceOfB(x: Any): scala.Boolean

    def unapplySeq(ms: collection.immutable.Map[_, _]): Option[Seq[(A, B)]] =
      if (ms forall {
        kv => isInstanceOfA(kv._1) && isInstanceOfB(kv._2)
      })
        Some(ms.toSeq.asInstanceOf[Seq[(A, B)]])
      else
        None
  }

  /** Map[String, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToString extends MapOfAToB {
    type A = String
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[String, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToInt extends MapOfAToB {
    type A = String
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[String, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToShort extends MapOfAToB {
    type A = String
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[String, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToLong extends MapOfAToB {
    type A = String
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[String, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToFloat extends MapOfAToB {
    type A = String
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[String, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToDouble extends MapOfAToB {
    type A = String
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[String, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToBoolean extends MapOfAToB {
    type A = String
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[String, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToChar extends MapOfAToB {
    type A = String
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[String, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfStringToByte extends MapOfAToB {
    type A = String
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[String]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Int, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToString extends MapOfAToB {
    type A = Int
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Int, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToInt extends MapOfAToB {
    type A = Int
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Int, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToShort extends MapOfAToB {
    type A = Int
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Int, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToLong extends MapOfAToB {
    type A = Int
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Int, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToFloat extends MapOfAToB {
    type A = Int
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Int, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToDouble extends MapOfAToB {
    type A = Int
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Int, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToBoolean extends MapOfAToB {
    type A = Int
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Int, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToChar extends MapOfAToB {
    type A = Int
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Int, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfIntToByte extends MapOfAToB {
    type A = Int
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Int]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Short, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToString extends MapOfAToB {
    type A = Short
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Short, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToInt extends MapOfAToB {
    type A = Short
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Short, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToShort extends MapOfAToB {
    type A = Short
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Short, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToLong extends MapOfAToB {
    type A = Short
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Short, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToFloat extends MapOfAToB {
    type A = Short
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Short, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToDouble extends MapOfAToB {
    type A = Short
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Short, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToBoolean extends MapOfAToB {
    type A = Short
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Short, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToChar extends MapOfAToB {
    type A = Short
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Short, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfShortToByte extends MapOfAToB {
    type A = Short
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Short]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Long, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToString extends MapOfAToB {
    type A = Long
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Long, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToInt extends MapOfAToB {
    type A = Long
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Long, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToShort extends MapOfAToB {
    type A = Long
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Long, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToLong extends MapOfAToB {
    type A = Long
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Long, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToFloat extends MapOfAToB {
    type A = Long
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Long, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToDouble extends MapOfAToB {
    type A = Long
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Long, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToBoolean extends MapOfAToB {
    type A = Long
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Long, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToChar extends MapOfAToB {
    type A = Long
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Long, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfLongToByte extends MapOfAToB {
    type A = Long
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Long]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Float, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToString extends MapOfAToB {
    type A = Float
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Float, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToInt extends MapOfAToB {
    type A = Float
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Float, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToShort extends MapOfAToB {
    type A = Float
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Float, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToLong extends MapOfAToB {
    type A = Float
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Float, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToFloat extends MapOfAToB {
    type A = Float
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Float, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToDouble extends MapOfAToB {
    type A = Float
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Float, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToBoolean extends MapOfAToB {
    type A = Float
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Float, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToChar extends MapOfAToB {
    type A = Float
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Float, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfFloatToByte extends MapOfAToB {
    type A = Float
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Float]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Double, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToString extends MapOfAToB {
    type A = Double
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Double, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToInt extends MapOfAToB {
    type A = Double
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Double, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToShort extends MapOfAToB {
    type A = Double
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Double, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToLong extends MapOfAToB {
    type A = Double
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Double, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToFloat extends MapOfAToB {
    type A = Double
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Double, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToDouble extends MapOfAToB {
    type A = Double
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Double, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToBoolean extends MapOfAToB {
    type A = Double
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Double, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToChar extends MapOfAToB {
    type A = Double
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Double, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfDoubleToByte extends MapOfAToB {
    type A = Double
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Double]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Boolean, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToString extends MapOfAToB {
    type A = Boolean
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Boolean, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToInt extends MapOfAToB {
    type A = Boolean
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Boolean, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToShort extends MapOfAToB {
    type A = Boolean
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Boolean, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToLong extends MapOfAToB {
    type A = Boolean
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Boolean, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToFloat extends MapOfAToB {
    type A = Boolean
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Boolean, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToDouble extends MapOfAToB {
    type A = Boolean
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Boolean, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToBoolean extends MapOfAToB {
    type A = Boolean
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Boolean, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToChar extends MapOfAToB {
    type A = Boolean
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Boolean, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfBooleanToByte extends MapOfAToB {
    type A = Boolean
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Boolean]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Char, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToString extends MapOfAToB {
    type A = Char
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Char, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToInt extends MapOfAToB {
    type A = Char
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Char, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToShort extends MapOfAToB {
    type A = Char
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Char, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToLong extends MapOfAToB {
    type A = Char
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Char, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToFloat extends MapOfAToB {
    type A = Char
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Char, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToDouble extends MapOfAToB {
    type A = Char
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Char, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToBoolean extends MapOfAToB {
    type A = Char
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Char, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToChar extends MapOfAToB {
    type A = Char
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Char, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfCharToByte extends MapOfAToB {
    type A = Char
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Char]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

  /** Map[Byte, String] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToString extends MapOfAToB {
    type A = Byte
    type B = String

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[String]
  }

  /** Map[Byte, Int] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToInt extends MapOfAToB {
    type A = Byte
    type B = Int

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Int]
  }

  /** Map[Byte, Short] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToShort extends MapOfAToB {
    type A = Byte
    type B = Short

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Short]
  }

  /** Map[Byte, Long] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToLong extends MapOfAToB {
    type A = Byte
    type B = Long

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Long]
  }

  /** Map[Byte, Float] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToFloat extends MapOfAToB {
    type A = Byte
    type B = Float

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Float]
  }

  /** Map[Byte, Double] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToDouble extends MapOfAToB {
    type A = Byte
    type B = Double

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Double]
  }

  /** Map[Byte, Boolean] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToBoolean extends MapOfAToB {
    type A = Byte
    type B = Boolean

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Boolean]
  }

  /** Map[Byte, Char] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToChar extends MapOfAToB {
    type A = Byte
    type B = Char

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Char]
  }

  /** Map[Byte, Byte] extractor
    *
    * See [[scalafy.types.extractorsMapOfAToB]]
    *
    * @author Mike Dreves
    */
  object MapOfByteToByte extends MapOfAToB {
    type A = Byte
    type B = Byte

    def isInstanceOfA(x: Any) = x.isInstanceOf[Byte]
    def isInstanceOfB(x: Any) = x.isInstanceOf[Byte]
  }

} // package object
