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
package scalafy.collection.immutable

import scala.collection.SortedMap
import scala.collection.SortedSet

import scalafy.util.csv._ 

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
