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
package scalafy

import collection.SortedMap
import collection.SortedSet

import scalafy.types.extractors.BasicExtractor
import scalafy.csv._

/** Contains basic object types.
  *
  * The following is a summary of features:
  * {{{
  * // Reifiable types
  * def createList(): List[_] = Reifiable(List[Int]())
  *
  * val x = createList()             // x is type List[_] (unknown list type)
  * x.isTypeOf[List[String]]         // false
  * x.isTypeOf[List[Int]]            // true 
  *  
  * def createMap(): Map[_,_] = Reifiable(Map[String, Boolean]())
  * val x: Any = createMap()      
  * x match {
  *   case xm if (xm.isTypeOf[Map[String, Boolean]]) => println("match")
  *   case _ => println("no match")
  * }
  *
  * // Map extractor
  * val x = Map("foo" -> "bar")
  * x match {
  *   case Map("foo" -> "bar") =>
  *     println("matched Map(foo -> bar)")
  * }
  *
  * // Scalar extractors
  * //   NOTE: If using multiple scala extractors at once make sure 
  * //         they are applied in this order or some will overide others 
  * val x = "test"
  * x match {
  *   case Short(s) => println("matched Short: " + s)
  *   case Int(i) => println("matched Int: " + i)
  *   case Long(l) => println("matched Long: " + l)
  *   case Float(f) => println("matched Float: " + f)
  *   case Double(d) => println("matched Double: " + d)
  *   case Boolean(b) => println("matched Boolean: " + b)
  *   case Char(c) => println("matched Char: " + c)
  *   case String(s) => println("matched String: " + s)
  *   case Byte(b) => println("matched Byte: " + b)
  * }
  * }}}
  */
package object types {

  ///////////////////////////////////////////////////////////////////////////
  // Reifiable Types
  ///////////////////////////////////////////////////////////////////////////

  /** Reifiable
    *
    * Stores information about the manifest associated with an object
    * This information can then used later during pattern matching.
    * This is intended to be used in cases where a method returns an
    * object that contains type information that would be lost due to 
    * type erasure. Prior to returning the object Reifiable() can be
    * called to store its manifest for later retrieval.
    *
    * Example: 
    * {{{
    * def createList(): List[_] = Reifiable(List[Int]())
    *
    * val x = createList()             // x is type List[_] (unknown list type)
    * x.isTypeOf[List[String]]         // false
    * x.isTypeOf[List[Int]]            // true 
    *  
    * def createMap(): Map[_,_] = Reifiable(Map[String, Boolean]())
    * val x = createMap()              // x is type Map[_,_]
    * x match {
    *   case xm if (xm.isTypeOf[Map[String, Boolean]]) => println("match")
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Reifiable {
    import collection.mutable.WeakHashMap

    val manifests = new WeakHashMap[Any, Manifest[_]]()

    def apply[A : Manifest](x: A): A = {
      manifests.put(x, manifest[A])
      x 
    }

    /** For cases when have a manifest already */
    def apply[A](m: Manifest[_], x: A): A = {
      manifests.put(x, m)
      x 
    }

    def isTypeOf(x: Any, m: Manifest[_]) = 
      manifests.contains(x) && m >:> manifests(x)
  }

  /** Settings for use with implicit params */
  case class ReifiableSettings(enabled: Boolean)

  class ReifiablePimp(val x: Any) {
    def isTypeOf[A : Manifest] = Reifiable.isTypeOf(x, manifest[A])
  }

  /** Allows us to use x.isTypeOf[...] */
  implicit def any2ReifiablePimp(x: Any) = new ReifiablePimp(x) 


  ///////////////////////////////////////////////////////////////////////////
  // Pimped Scala Types
  ///////////////////////////////////////////////////////////////////////////

  /** Arrow
    *
    * This is taken from a post by Daniel C. Sobral. Need to locate the
    * link...
    *
    * {{{
    * x match {
    *   case "foo" -> "bar" => println("matched (foo,bar)")
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Daniel C. Sobral
    */
  object -> {
    def unapply[A, B](x: (A, B)) = Some(x)
  }

  /** Map
    *
    * Overrides the scala.collection.immutable.Map companion object in order
    * to provide capabilities to extract a maps key/value pairs. It should
    * be used with caution as changes to the underlying Map implementation
    * over time will not be reflected in this object. This extractor should
    * be used in combination with the -> extractor.
    *
    * {{{
    * x match {
    *   case Map("foo" -> "bar") =>
    *     println("matched Map(foo -> bar)")
    *   case Map(a -> b, rest @ _*) =>
    *     println("starts with a=" + a + "b=" + b + " followed by: " + rest)
    *   case _ =>
    *     println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Map extends collection.generic.ImmutableMapFactory[Map] {
    implicit def canBuildFrom[A, B]: collection.generic.CanBuildFrom[
      Coll, (A, B), Map[A, B]
    ] = collection.immutable.Map.canBuildFrom

    def empty[A, B]: Map[A, B] = collection.immutable.Map.empty

    def unapplySeq[A, B](ms: Map[A, B]): Option[Seq[(A,B)]] = Some(ms.toSeq)
  }

  /** String
    *
    * {{{
    * x match {
    *   case String(s) => println("matched: " + s)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object String extends BasicExtractor {
    type A = String

    def isType(x: Any) = x.isInstanceOf[String] 
    def toType(s: String) = Some(s)
  }

  /** Int
    *
    * {{{
    * // x can be of type Int or String
    * x match {
    *   case Int(i) => println("matched: " + i)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Int extends BasicExtractor {
    type A = Int

    def isType(x: Any) = x.isInstanceOf[Int] 
    def toType(s: String) = Some(s.toInt)
  }

  /** Short
    *
    * {{{
    * // x can be of type Short or String
    * x match {
    *   case Short(s) => println("matched: " + s)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Short extends BasicExtractor {
    type A = Short

    def isType(x: Any) = x.isInstanceOf[Short] 
    def toType(s: String) = Some(s.toShort)
  }

  /** Long
    *
    * {{{
    * // x can be of type Long or String
    * x match {
    *   case Long(l) => println("matched: " + l)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Long extends BasicExtractor {
    type A = Long

    def isType(x: Any) = x.isInstanceOf[Long] 
    def toType(s: String) = Some(s.toLong)
  }

  /** Float
    *
    * {{{
    * // x can be of type Float or String
    * x match {
    *   case Float(f) => println("matched: " + f)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Float extends BasicExtractor {
    type A = Float

    def isType(x: Any) = x.isInstanceOf[Float] 
    def toType(s: String) = Some(s.toFloat)
  }

  /** Double
    *
    * {{{
    * // x can be of type Double or String
    * x match {
    *   case Double(d) => println("matched: " + d)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Double extends BasicExtractor {
    type A = Double

    def isType(x: Any) = x.isInstanceOf[Double] 
    def toType(s: String) = Some(s.toDouble)
  }

  /** Boolean
    *
    * {{{
    * // x can be of type Boolean or String
    * x match {
    *   case Boolean(b) => println("matched: " + b)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Boolean extends BasicExtractor {
    type A = Boolean

    def isType(x: Any) = x.isInstanceOf[Boolean] 
    def toType(s: String) = Some(s.toBoolean)
  }

  /** Char
    *
    * {{{
    * // x can be of type Char or String of length 1
    * x match {
    *   case Char(c) => println("matched: " + c)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Char extends BasicExtractor {
    type A = Char

    def isType(x: Any) = x.isInstanceOf[Char] 
    def toType(s: String) =
      if (s.length == 1) Some(s(0)) else None
  }

  /** Byte
    *
    * {{{
    * x match {
    *   case Byte(b) => println("matched: " + b)
    *   case _ => println("no match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object Byte extends BasicExtractor {
    type A = Byte
    
    def isType(x: Any) = x.isInstanceOf[Byte] 
    def toType(s: String) = None
  }


  ///////////////////////////////////////////////////////////////////////////
  // Misc Types
  ///////////////////////////////////////////////////////////////////////////
 
  /**
    * Contains utilities for dealing with name/value pairs.
    *
    * Name/value pairs when stored as a string have each name/value pair 
    * stored on a separate line. The line starts with the property name
    * followed by ":" and then the value. When stored in a map the map
    * just contains the name as a string and the value as a string. The
    * value itself can be made up of multiple values stored in CSV format.
    * 
    * The format is similar to HTTP headers and INI files.
    */ 
  object NameValuePair {
    /**
      * Converts a string of name/value pairs to a property map.
      *
      * @param value name/value pairs
      * @return propName -> value
      */
    def toMap(value: Iterable[Char]): SortedMap[String, String] = {
      if (value == null) return SortedMap[String, String]()
      var propMap = SortedMap[String, String]()
      fromNsv[Seq[String]](value).getOrElse(Seq()).foreach { line =>
        var name: String = null
        var value: String = null
        val pos = line.indexOf(":")
        if (pos == -1) {
          name = line
        } else {
          name = line.substring(0, pos)
          value = line.substring(pos + 1).trim
        }
        if (name != null) {
          if (propMap.get(name).isEmpty) {
            propMap += (name -> value)
          } else if (value != null) {
            propMap += (name -> (propMap.get(name).get + "," + value))
          }
        }
      }
      propMap
    }

    /** Converts a property map to a String of name/value pairs.
      *
      * @param propMap propName -> value
      * @return String of name/value pairs
      */
    def toString(propMap: Map[String, String]): String = {
      var nameValuePairs = ""
      for ((name, value) <- propMap) {
        if (nameValuePairs != "") {
          nameValuePairs = nameValuePairs + "\n"
        }
        nameValuePairs = nameValuePairs + 
          name.trim + ": " + value.trim
      }
      nameValuePairs
    }

    /** Adds a value to a name/value pair map.
      *
      * If replaceIfExists is false then the value will be merged
      * with the existing value using CSV format.
      *
      * @param nameValuePairs the name/value pairs map
      * @param name the new property name
      * @param value the new property value
      * @param replaceIfExists true if replace if exists, else merge
      * @return the new combined name/value pairs 
      */
    def addToMap(
      nameValuePairs: Map[String, String],
      name: String,
      value: String,
      replaceIfExists: Boolean = true
    ): SortedMap[String, String] = {
      if (nameValuePairs == null) return SortedMap(name -> value)

      var resultMap = SortedMap(nameValuePairs.toSeq : _*)
      if (name == null) return resultMap

      if (nameValuePairs.get(name).isEmpty) {
        resultMap += (name -> value)
      } else {
        if (replaceIfExists) {
          resultMap += (name -> value)
        } else {
          resultMap += (name -> mergeValues(resultMap.get(name).get, value))
        }
      }
      resultMap
    }

    /** Adds a value to a name/value pair string.
      *
      * If replaceIfExists is false then the value will be merged
      * with the existing value using CSV format.
      *
      * @param nameValuePairs the name/value pairs string
      * @param name the new property name
      * @param value the new property value
      * @param replaceIfExists true if replace if exists, else merge
      * @return the new combined name/value pairs 
      */
    def addToString(
      nameValuePairs: String,
      name: String,
      value: String,
      replaceIfExists: Boolean = true
    ): String = {
      if (name == null) nameValuePairs
      else if (nameValuePairs == null) name + ": " + value
      else if (nameValuePairs.indexOf(name) != -1) {
        var propMap = NameValuePair.toMap(nameValuePairs)
        if (replaceIfExists) {
          propMap += (name -> value)
        } else {
          propMap += (name -> mergeValues(propMap.get(name).get, value)) 
        }
        NameValuePair.toString(propMap.toMap)
      } else {
        var result = nameValuePairs
        if (result != "") {
          result += "\n"
        }
        result += name + ": " + value
        result
      }
    }

    /** Merges a name/value pair map with another name/value pair map.
      *
      * If the mergeIsComplement is set to true, then the values from B 
      * will be removed from A. Only the property names should be provided
      * in this case. If a value is also provided, then the value will only 
      * be removed if it matches the current value.
      *
      * @param valueA the first name/value pairs map 
      * @param valueB the second name/value pairs map 
      * @param complement true if merge is complement
      * @return the combined map 
      */
    def mergeMaps(
      mapA: Map[String, String],
      mapB: Map[String, String],
      complement: Boolean = false
    ): SortedMap[String, String] = {
      if (mapA == null) mapB
      if (mapB == null) mapA
      var resultMap = SortedMap[String, SortedSet[String]]()

      // Put all A props into result set
      for ((prop, value) <- mapA) {
        if (value == null) {
          resultMap += (prop -> SortedSet[String]())
        } else {
          val set = fromCsv[Seq[String]](value) match {
            case Some(seq) => SortedSet(seq : _*)
            case _ => SortedSet[String]()
          }
          resultMap += (prop -> set)
        }
      }

      // Add or remove B props from result set
      for ((prop, value) <- mapB) {
        // Create a set of out B's value
        val bSet = if (value == null) 
          Set[String]() 
        else { 
          fromCsv[Seq[String]](value) match {
            case Some(seq) => Set(seq : _*)
            case _ => Set[String]() 
          }
       }

       if (!resultMap.get(prop).isEmpty) {
          val aSet = resultMap.get(prop).get
          if (complement) {
            val resultSet = aSet.diff(bSet)
            if (value == null || resultSet.isEmpty) {
              resultMap -= prop
            } else {
              resultMap += (prop -> resultSet)
            }
          } else {
            resultMap += (prop -> aSet.union(bSet))
          }
        } else if (!complement) {
          resultMap += (prop -> SortedSet[String]().union(bSet))
        }
      }
      
      // Create new string from result map
      var result = SortedMap[String, String]()
      for ((prop, value) <- resultMap) {
        val buf = new StringBuilder()
        for (v <- value) {
          buf.append(v)
          buf.append(",")
        }
        buf.length -= 1 // remove last ',' added
        result += (prop -> buf.toString()) 
      }
      result
    } 

    /** Merges a name/value pair string with another name/value pair string.
      *
      * If the mergeIsComplement is set to true, then the values from B 
      * will be removed from A. Only the property names should be provided
      * in this case. If a value is also provided, then the value will only 
      * be removed if it matches the current value.
      *
      * @param valueA the first name/value pairs string
      * @param valueB the second name/value pairs string
      * @param complement true if merge is complement
      * @return the combined string 
      */
    def mergeStrings(
      valueA: String,
      valueB: String,
      complement: Boolean = false
    ): String = {
      var resultMap = mergeMaps(
          NameValuePair.toMap(valueA).toMap,
          NameValuePair.toMap(valueB).toMap, 
          complement )

      // Create new string from result map
      val result = new StringBuilder()
      for ((prop, value) <- resultMap) {
        result.append(prop)
        result.append(": ")
        result.append(value)
        result.append("\n")
      }
      result.length -= 1 // remove last '\n' added
      result.toString
    }

    /** merges two CSV strings into one */
    private def mergeValues(valueA: String, valueB: String): String = {
      var setA = fromCsv[Seq[String]](valueA) match {
        case Some(seq) => SortedSet(seq : _*)
        case _ => SortedSet[String]()
      }
      val setB = fromCsv[Seq[String]](valueB) match {
        case Some(seq) => SortedSet(seq : _*)
        case _ => SortedSet[String]()
      }
      val result = new StringBuilder()
      for (x <- setA.union(setB)) {
        result.append(x)
        result.append(",")
      }
      result.length -= 1 // remove trailing ','
      result.toString
    }
  }
}
