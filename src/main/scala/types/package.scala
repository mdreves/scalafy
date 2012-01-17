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
import scalafy.csv.Csv

/** Contains basic object types.
  *
  * The following is a summary of features:
  * {{{
  * // Reifiable types
  * val x = ReifiableList("foo", "bar")
  * x match {
  *   case listOfStr if (
  *     ReifiableList.isListOf(listOfStr, manifest[String])
  *   ) =>
  *     println("Matched: " + listOfStr)
  * }
  *
  * val x = ReifiableMap("foo" -> "bar")
  * x match {
  *   case mapOfStrToStr if (
  *     ReifiableMap.isMapOf(mapOfStrToStr, manifest[(String, String)])
  *   ) =>
  *     println("Matched: " + mapOfStrToStr)
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
  * val x = "test"
  * x match {
  *   case String(s) => println("matched: " + s)
  *   case Int(i) => println("matched: " + i)
  *   ...
  * }
  * }}}
  */
package object types {

  ///////////////////////////////////////////////////////////////////////////
  // Aliases
  ///////////////////////////////////////////////////////////////////////////

  type RList = ReifiableList
  type RMap = ReifiableMap


  ///////////////////////////////////////////////////////////////////////////
  // Reifiable Types
  ///////////////////////////////////////////////////////////////////////////

  /** Reifiable List
    *
    * Wraps the creation of a List such that it keeps track of manifest
    * information about what type of information that is contained in the
    * List. This information can then used later during pattern matching.
    *
    * A list of elements can be created as follows:
    * {{{
    * // type of x is List[String]
    * val x = ReifiableList("foo", "bar")
    *
    * // type of x is List[List[String]]
    * val x = ReifiableList(ReifiableList("foo", "bar"))
    *
    * // type of x is List[List[Int]]
    * val x = ReifiableList(ReifiableList(1, 2))
    * }}}
    *
    * Elements can be extracted from the list as follows:
    * {{{
    * x match {
    *   case s :: rest if (x.isListOf(manifest[String]) && s == "foo") =>
    *     println("Matched: " + s + " " + rest)
    * 
    *   case listOfStr if (x.isListOf(manifest[String])) =>
    *     println("Matched: " + listOfStr)
    *   
    *   case listOfListOfStr if (x.isListOf(manifest[List[String]])) =>
    *     println("Matched: " + listOfListOfStr)
    *
    *   case listOfListOfInt if (x.isListOf(manifest[List[Int]])) =>
    *     println("Matched: " + listOfListOfInt)
    *
    *   case _ =>
    *     println("No match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object ReifiableList {
    import collection.mutable.WeakHashMap

    val manifests = new WeakHashMap[List[_], Manifest[_]]()

    def apply[A : Manifest](xs: A*): List[A] = {
      val list = xs.toList
      manifests.put(list, manifest[A])
      list
    }

    def unapplySeq[A](xs: List[A]): Option[Seq[A]] =
      if (manifests.contains(xs)) Some(xs) else None

    def isListOf(x: Any, m: Manifest[_]) = {
      if (x.isInstanceOf[List[_]]) {
        val xs = x.asInstanceOf[List[_]] 
        manifests.contains(xs) && m == manifests(xs)
      } else false
    }
  }

  class ReifiableListHelper(val x: Any) {
    def isListOf(m: Manifest[_]) = ReifiableList.isListOf(x, m)
  }

  /** Allows us to use x.isListOf(manifiest[...]) */
  implicit def any2ReifiableListHelper(x: Any) = new ReifiableListHelper(x) 

  /** Not used
    *
    * This class is only created so we can type alias the companion object
    * [[scalafy.types.ReifiableList]]. Scala doesn't allow type aliases
    * without a companion class for some reason.
    */
  class ReifiableList

  /** Reifiable Map
    *
    * Wraps the creation of a Map such that it keeps track of manifest
    * information about what type of information is contained in the Map.
    * This information can then used later during pattern matching.
    *
    * A reifiable map of elements can be created as follows:
    * {{{
    * // type of x is Map[String, String]
    * val x = ReifiableMap("foo" -> "bar")
    *
    * // type of x is Map[Int, Map[String, String]]
    * val x = ReifiableMap(1 -> ReifiableMap("foo" -> "bar"))
    *
    * // type of x is Map[Int, Map[Int, Int]]
    * val x = ReifiableMap(1 -> ReifiableMap(2 -> 3))
    * }}}
    *
    * Elements can be extracted from the map as follows:
    * {{{
    * x match {
    *   case Map(a -> b, rest @ _*) if (
    *     x.isMapOf(manifest[(String, String)]) && a == "foo"
    *   ) =>
    *     println("Matched: " + a + " " + b + " " + rest)
    * 
    *   case mapOfStrToStr if (
    *     x.isMapOf(manifest[(String, String)])
    *   ) =>
    *     println("Matched: " + mapOfStrToStr)
    *
    *   case mapOfStrToMapOfStrToStr if (
    *     x.isMapOf(manifest[(String, Map[String, String])])
    *   ) =>
    *     println("Matched: " + mapOfStrToMapOfStrToStr)
    *
    *   case _ =>
    *     println("No match")
    * }
    * }}}
    *
    * @author Mike Dreves
    */
  object ReifiableMap {
    import collection.mutable.WeakHashMap

    val manifests = new WeakHashMap[Map[_,_], Manifest[_]]()

    def apply[A: Manifest, B: Manifest](kvs: (A, B)*): Map[A, B] = {
      val map = kvs.toMap
      manifests.put(map, manifest[(A, B)])
      map
    }

    def unapplySeq[A, B](xm: Map[A, B]): Option[Seq[(A, B)]] =
      if (manifests.contains(xm)) Some(xm.toSeq) else None
 
    def isMapOf(x: Any, m: Manifest[_]) = {
      if (x.isInstanceOf[Map[_,_]]) {
        val xm = x.asInstanceOf[Map[_,_]] 
        manifests.contains(xm) && m == manifests(xm)
      } else false
    }
  }

  class ReifiableMapHelper(val x: Any) {
    def isMapOf(m: Manifest[_]) = ReifiableMap.isMapOf(x, m)
  }

  /** Allows us to use x.isMapOf(manifiest[...]) */
  implicit def any2ReifiableMapHelper(x: Any) = new ReifiableMapHelper(x) 


  /** Not used
    *
    * This class is only created so we can type alias the companion object
    * [[scalafy.types.ReifiableMap]]. Scala doesn't allow type aliases
    * without a companion class for some reason.
    */
  class ReifiableMap


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
  }

  /** Int
    *
    * {{{
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
  }

  /** Short
    *
    * {{{
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
  }

  /** Long
    *
    * {{{
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
  }

  /** Float
    *
    * {{{
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
  }

  /** Double
    *
    * {{{
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
  }

  /** Boolean
    *
    * {{{
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
  }

  /** Char
    *
    * {{{
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
      val lines = Csv.fromNsv(value)
      var propMap = SortedMap[String, String]()
      for (line <- lines) {
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
          val set = Csv.fromCsvLine(value) match {
            case Left(l) => SortedSet(l)
            case Right(r) => SortedSet(r : _*)
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
          Csv.fromCsvLine(value) match {
            case Left(l) => Set(l)
            case Right(r) => Set(r : _*)
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
      var setA = Csv.fromCsvLine(valueA) match {
        case Left(l) => SortedSet(l)
        case Right(r) => SortedSet(r : _*)
      }
      val setB = Csv.fromCsvLine(valueB) match {
        case Left(l) => SortedSet(l)
        case Right(r) => SortedSet(r : _*)
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
