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
package scalafy.util.csv

import collection.immutable.Vector
import scalafy.types.reifiable.Reifiable

/** CSV parser */
private[csv] object CsvParser {
  /** Parser for toCsv package function 
    *
    * @return either String error msg or data of given type
    */
  def fromCsv[A](
    csv: Iterable[Char], 
    trimWhitespace: Boolean = true, 
    skipNonMatching: Boolean = false
  )(
    implicit m : Manifest[A], settings: CsvSettings
  ): Either[String, A] = {
    val errorPrefix = "CSV parse error :: "

    var result = 
      if (m == manifest[Any] || m == manifest[Nothing]) createSeq[Seq[String]]
      else if (m <:< manifest[Vector[_]]) createVector(m.typeArguments(0))
      else if (m <:< manifest[List[_]]) createList(m.typeArguments(0))
      // m <:< manifest[Seq[_]] doesn't work for Seq of Seq...
      // so we will assume Seq was used if inner type is a Seq
      else if (m <:< manifest[Seq[_]] || (!m.typeArguments.isEmpty && 
          m.typeArguments(0) <:< manifest[Seq[_]])) 
        createSeq(m.typeArguments(0))
      else return parsePrimitive(csv.toString, errorPrefix)(m)

    val flatList = (m != manifest[Any] && m != manifest[Nothing]) && 
      !(m.typeArguments(0) <:< manifest[Vector[_]] ||
        m.typeArguments(0) <:< manifest[List[_]] ||
        m.typeArguments(0) <:< manifest[Seq[_]])
    val lineType = 
      if (flatList || m == manifest[Any] || m == manifest[Nothing]) m 
      else m.typeArguments(0)
    val primitiveType = 
      if (m == manifest[Any] || m == manifest[Nothing]) m 
      else lineType.typeArguments(0)

    val iter = new CsvStoppingIterator(csv)
    while (iter.hasNextValue) {
      var line = 
        if (flatList) result
        else if (lineType == manifest[Any] || lineType == manifest[Nothing]) 
          createSeq[String]
        else if (lineType <:< manifest[Vector[_]])createVector(primitiveType) 
        else if (lineType <:< manifest[List[_]]) createList(primitiveType) 
        else if (lineType <:< manifest[Seq[_]]) createSeq(primitiveType) 
        else return Left(errorPrefix + "unsupported type: " + lineType) 

      while (iter.hasNextValue && !iter.isStoppedOnLineBreak) {
        while (iter.hasNext) iter.next()
        val v = if (trimWhitespace) iter.getValue().trim else iter.getValue()
        // Convert value to proper type
        parsePrimitive(v, errorPrefix)(primitiveType) match {
          case Right(r) => line :+= r
          case Left(l) => if (!skipNonMatching) return Left(l) 
        }
        if (!iter.isStoppedOnLineBreak) iter.nextValue()
      }

      if (!flatList) {
        if (settings.reifiableSettings.enabled) {
          if (m == manifest[Any] || m == manifest[Nothing]) {
            Reifiable(manifest[Seq[String]], line)(settings.reifiableSettings)
          } else {
            Reifiable(m, line)(settings.reifiableSettings)
          } 
        }
        result :+= line
      }
      else result = line

      iter.nextValue() // skip stop for line break
      iter.nextValue() // skip stop to read line break
    }

    if (settings.reifiableSettings.enabled) {
      if (m == manifest[Any] || m == manifest[Nothing]) {
        Reifiable(manifest[Seq[Seq[String]]], result)(
          settings.reifiableSettings)
      } else {
        Reifiable(m, result)(settings.reifiableSettings)
      }
    } 
    Right(result.asInstanceOf[A])
  }

  /* Parser for toCsv package function */
  def toCsv[A : Manifest](value: A, padding: String = ""): String = {
    if (value.isInstanceOf[Seq[_]]) {
      val lines = value.asInstanceOf[Seq[_]]
      if (lines.isEmpty) return ""
      if (lines(0).isInstanceOf[Seq[_]]) {
        lines.foldLeft("")((result, line) => {
          val lineData = toCsvLine(line.asInstanceOf[Seq[_]], padding)
          if (result.length == 0) result + lineData
          else result + LineBreak + lineData
        })
      } else toCsvLine(lines, padding) 
    } else {
      toEscapedCsv(value)
    }
  }

  /** Parser for toEscapedCsv package function */
  def toEscapedCsv[A](value: A): String = {
    val data = value.toString

    // If no commas or linebreaks then just return what was passed
    if (
      data.indexOf(Separator) == -1 &&
      data.indexOf(LineBreak) == -1 &&
      data.indexOf(Enclosure) == -1
    ) {
      data
    } else {
      toCsvLine(List(data), "").stripSuffix(",")
    }
  }

  /** Parser for fromNsv package function */
  def fromNsv[A](nsv: Iterable[Char], skipNonMatching: Boolean = false)(
    implicit m : Manifest[A], settings: CsvSettings
  ): Either[String, A] = {
    val errorPrefix = "NSV parse error :: "
    
    var result = 
      if (m == manifest[Any] || m == manifest[Nothing]) createSeq[String]
      else if (m <:< manifest[Vector[_]]) createVector(m.typeArguments(0))
      else if (m <:< manifest[List[_]]) createList(m.typeArguments(0))
      else if (m <:< manifest[Seq[_]]) createSeq(m.typeArguments(0))
      else return parsePrimitive(nsv.toString, errorPrefix)(m) 

    val primitiveType = 
      if (m == manifest[Any] || m == manifest[Nothing]) m
      else m.typeArguments(0)

    val iter = new NsvStoppingIterator(nsv)
    while (iter.hasNextLine) {
      var line = new StringBuilder
      while (iter.hasNext) line += iter.next()

      parsePrimitive(line.toString,  errorPrefix)(primitiveType) match {
        case Right(r) => result :+= r
        case Left(l) => if (!skipNonMatching) return Left(l)
      }
      iter.nextLine()
    }
  
    if (settings.reifiableSettings.enabled) {
      if (m == manifest[Any] || m == manifest[Nothing]) {
        Reifiable(manifest[Seq[String]], result)(settings.reifiableSettings)
      } else {
        Reifiable(m, result)(settings.reifiableSettings)
      }
    } 
    Right(result.asInstanceOf[A])
  }

  /** Parser for toNsv package function */
  def toNsv[A](value: A): String = {
    val data = 
      if (value.isInstanceOf[Seq[_]]) value.asInstanceOf[Seq[_]] 
      else Seq(value)

    var result = ""
    for (d <- data) {
      val line = d.toString
      var varLine = line
      // Check if newline exists and is not escaped
      val pos = line.indexOf(LineBreak)
      val firstQuote = line.indexOf('"')
      val lastQuote = line.lastIndexOf('"')
      if (
        pos != -1 &&
        !(
          firstQuote != -1 &&
          lastQuote != -1 &&
          firstQuote < pos &&
          lastQuote > pos
        )
      ) {
        // Need to escape newline
        if (line.indexOf(",") == -1) varLine = Enclosure + line + Enclosure
        else {
          // Search for insert position
          var lPos = pos
          var lChar = line.charAt(lPos)
          while (lPos > 0 && lChar != ',' && lChar != '"') {
            lChar = line.charAt(lPos)
            lPos -= 1
          }
          var rPos = pos
          var rChar = line.charAt(rPos)
          while (rPos < line.length && rChar != ',' && rChar != '"') {
            rChar = line.charAt(rPos)
            rPos += 1
          }
          varLine = line.substring(0, lPos) + Enclosure +
            line.substring(lPos, rPos) + Enclosure + line.substring(rPos)
        }
      }
      if (result == "") result = varLine
      else result = result + LineBreak + varLine
    }
    result
  }

  // Internal helper methods

  private def toCsvLine[A](data: Seq[A], padding: String): String = {
    // If empty array just return separator
    if (data.size == 0) return Separator.toString

    var result = Vector[String]()

    for (d <- data) {
      var item = d.toString
      if (item.indexOf(Enclosure) != -1) {
        // Replace " with ""
        item = item.replace(
          Enclosure.toString, Enclosure.toString + Enclosure)
      }

      if (
        item.indexOf(Separator) != -1 ||
        item.indexOf(Enclosure) != -1 ||
        item.indexOf(LineBreak) != -1
      ) {
        // Wrap with ""
        item = Enclosure + item + Enclosure
      }

      if (result.size == 0) result :+= item
      else result :+= padding + item
    }

    result.reduceLeft(_ + Separator + _)
  }

  // Seq creation methods using manifests
  private def createVector[A : Manifest] = Vector[A]()
  private def createList[A : Manifest] = List[A]()
  private def createSeq[A : Manifest] = Seq[A]()

  // Type conversion methods
  private def parsePrimitive[A : Manifest](
    s: String, errorPrefix: String
  ): Either[String, A] = try {
    if (manifest[A] == manifest[String] || manifest[A] == manifest[Any] ||
        manifest[A] ==  manifest[Nothing]) 
      Right(s.asInstanceOf[A])
    else if (manifest[A] == manifest[Short]) 
      Right(s.toShort.asInstanceOf[A])
    else if (manifest[A] == manifest[Int]) 
      Right(s.toInt.asInstanceOf[A])
    else if (manifest[A] == manifest[Long]) 
      Right(s.toLong.asInstanceOf[A])
    else if (manifest[A] == manifest[Float]) 
      Right(s.toFloat.asInstanceOf[A])
    else if (manifest[A] == manifest[Double]) 
      Right(s.toDouble.asInstanceOf[A])
    else if (manifest[A] == manifest[Boolean]) 
      Right(s.toBoolean.asInstanceOf[A])
    else if (manifest[A] == manifest[Char] && s.length == 1) 
      Right(s(0).asInstanceOf[A]) 
    else Left(errorPrefix + "unsupported type: " + manifest[A])
  } catch {
    case e: Exception => 
      Left(errorPrefix + e.getMessage() + " at " + s)
  }
}

/** Iterates sequences of comma separated values
  *
  * Any newlines encounted will be treated as values.
  */
private[csv] class CsvIterator(
  val xs: Iterable[Char],
  val trimWhitespace: Boolean = true
) extends Iterator[String] {
  private val iter = new CsvStoppingIterator(xs)

  def hasNext = iter.hasNextValue

  def next() = {
    while (iter.hasNext) iter.next()
    val next = iter.getValue()
    iter.nextValue()
    if (trimWhitespace && next != "\n") next.trim else next
  }
}

/** Iterates sequence of characters stopping on commas or line breaks
  *
  * The next() method should be called until the hasNext method
  * returns false. At this point the getValue() method can be
  * used to get the current value read. Once the value has been
  * read the nextValue() method must be called to move the
  * iterator on to the next value to be iterated. Linebreaks ('\n')
  * will be returned as values.
  */
private[csv] class CsvStoppingIterator(
  val xs: Iterable[Char]
) extends BufferedIterator[Char] {
  private val iter = new NsvStoppingIterator(xs)
  private var prev: Char = 0
  private var quoteCountOdd = false // even if content escaped
  private var firstQuote = true
  private var stopped = false // stopped because of comma (not newline)
  private var value = new StringBuilder

  def hasNext = {
    if (!stopped && iter.hasNext) {
      // If hit (unescaped) comma, then done with this iteration
      if (!quoteCountOdd && iter.head == Separator) {
        stopped = true
        false
      } else true
    } else false
  }

  def head = iter.head

  def next() = {
    prev = iter.next()
    if (prev == Separator && !quoteCountOdd) {
      throw new Exception("Invalid CSV")
    } else if (prev == Enclosure) {
      quoteCountOdd = !quoteCountOdd
      if (!value.isEmpty) {
        firstQuote = false
      }
      // Don't add to value if first quote or not second double quote
      if (!firstQuote && !quoteCountOdd) {
        value += prev
      }
      firstQuote = false
    } else {
      value += prev
    }
    prev
  }

  def hasNextValue = iter.hasNextLine

  def getValue() = value.stripSuffix("\"")

  def isStoppedOnLineBreak = iter.isStopped

  def isStopped = stopped || iter.isStopped

  def nextValue() {
    if (iter.isStopped) {
      // If stopped because of newline, then skip over newline
      iter.nextLine()
      // Now simulate stop because of comma so we can return the
      // newline as a value
      value.clear()
      prev = LineBreak
      value += LineBreak
      stopped = true
    } else if (stopped) {
      // If stopped because of comma, then skip over comma
      if (prev != LineBreak) iter.next()
      prev = 0
      value.clear()
      stopped = false
    }
    firstQuote = true
  }
}

/** Iterates sequence of characters stopping on line breaks
  *
  * The next() method should be called until the hasNext method
  * returns false. At this point the nextLine() method must be
  * called to move the iterator on to the next line to be iterated.
  */
private[csv] class NsvStoppingIterator(
  val xs: Iterable[Char]
) extends BufferedIterator[Char] {
  private val iter = xs.iterator.buffered
  private var prev: Char = 0
  private var quoteCountOdd = false // even if content escaped
  private var stopped = false // true if stopped on newline

  def hasNext = {
    if (!stopped && iter.hasNext) {
      // If hit (unescaped) newline, then done with this iteration
      if (
        !quoteCountOdd && prev != Escape && iter.head == LineBreak
      ) {
        stopped = true
        false
      } else true
    } else false
  }

  def head = iter.head

  def next() = {
    prev = iter.next()
    if (prev == Enclosure) quoteCountOdd = !quoteCountOdd
    prev
  }

  /** Returns true if has more lines */
  def hasNextLine = iter.hasNext

  /** Returns true if stopped on line break */
  def isStopped = stopped

  /** Unblocks buffer to move on to next line */
  def nextLine() {
    // Skip over current \n
    if (iter.hasNext && iter.head == LineBreak) iter.next()
    prev = 0
    stopped = false
  }
}
