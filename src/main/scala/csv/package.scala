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

import collection.immutable.Vector
import types.{Reifiable, ReifiableSettings}

/** Contains utilities for CSV matching and conversion.
  *
  * The following is a summary of features:
  * {{{
  * // Parsing (comma separated data - as per RFC4180)
  * val v = fromCsv[Seq[Seq[String]]]("a,b\nc,d")
  * toCsv(Seq(Seq("a","b"),Seq("c","d")))  // "a,b\nc,d"
  *
  * // Parsing (non-string primitive data)
  * val v = fromCsv[Seq[Seq[Int]]]("1,2\n3,4")
  * toCsv(Seq(Seq(1,2),Seq(3,4)))          // "1,2\n3,4"
  *
  * // Parsing (single line)
  * val v = fromCsv[Seq[Int]]("1,2")       // Seq(1,2)
  * toCsv(Seq(1,2)))                       // "1,2"
  *
  * // Parsing (skipping values that don't match type)
  * val v = fromCsv[Seq[Seq[Int]]](
  *   "1,2\nfoo,4", true, true)           // Seq(Seq(1,2),Seq(4))
  *
  * // Parsing (newline separated data)
  * //   NOTE: Handles escaped newlines as per RFC4180
  * fromNsv[Seq[String]]("a,b\nc,d")      // Seq("a,b","c,d")
  * fromNsv[Seq[Boolean]]("true\nfalse")  // Seq(true, false)
  * toNsv(Seq("foo", "bar"))              // "foo\nbar"
  *
  * // Escaping (NOTE: done automatically by toCsv/toNsv)
  * toEscapedCsv("a,b,c,\"d\",e")         // "a,b,c,""d"",e"
  *
  * // Extracting (csv for each line)
  * matchEachCsvLine("a,b\nc,d") {
  *   case Csv(x, y) =>
  *     println("matched: " + x + "," + y)  // prints: a b then  c d
  * }
  *
  * // Extracting (values in a single line)
  * "a,b" match {
  *   case Csv(a, b) =>
  *     println("matched: " + a + "," + b)
  * }
  *
  * // Extracting (values using variable wildcard pattern)
  * "a,b\nc,d" match {
  *   case Csv(a, rest @ _*) =>
  *     println("matched: " + a + "," + rest)
  * }
  *
  * // Extracting (values over multiple lines in one extractor)
  * "a,b\nc,d" match {
  *   case Csv(a, b, Eol, c, d) =>
  *     println("matched: " + a + "," + b + "," + c + "," + d)
  *   // Same as above
  *   case Csv(a, b, "\n", c, d) =>
  *     println("matched: " + a + "," + b + "," + c + "," + d)
  *   case Csv(a, _, _, _, d) =>
  *     println("matched: " + a + "," + d)
  * }
  *
  * // Extracting (only lines)
  * "a,b\nc,d" match {
  *   case Nsv(line1, line2) =>
  *     println("matched: " + line1 + " " + line2)
  * }
  *
  * // Extracting (lines using variable wildcard patterns)
  * "a,b\nc,d\ne,f" match {
  *   case Nsv(line1, rest @ _*) =>
  *     println("matched: " + line1 + " " + rest)
  * }
  * }}}
  */
package object csv {

  ///////////////////////////////////////////////////////////////////////////
  // vals and implicits 
  ///////////////////////////////////////////////////////////////////////////

  val Eol = "\n"

  implicit val reifiableSettings = ReifiableSettings(false)

  ///////////////////////////////////////////////////////////////////////////
  // CSV Package Functions
  ///////////////////////////////////////////////////////////////////////////

  /** Converts from RFC 4180 compatible CSV data to a given A 
    *
    * Any of the following types are supported for A:
    * <pre>
    *   Seq[Seq[String]],  List[List[String]],  Vector[Vector[String]]
    *   Seq[Seq[Short]],   List[List[Short]],   Vector[Vector[Short]]
    *   Seq[Seq[Int]],     List[List[Int]],     Vector[Vector[Int]]
    *   Seq[Seq[Long]],    List[List[Long]],    Vector[Vector[Long]]
    *   Seq[Seq[Float]],   List[List[Float]],   Vector[Vector[Float]]
    *   Seq[Seq[Double]],  List[List[Double]],  Vector[Vector[Double]]
    *   Seq[Seq[Boolean]], List[List[Boolean],  Vector[Vector[Boolean]]
    *   Seq[Seq[Char]],    List[List[Char]],    Vector[Vector[Char]]
    *   Seq[Seq[Any]],     List[List[Any]],     Vector[Vector[Any]]
    *   Any
    * </pre>
    * In addition, a single Seq/List/Vector type can be passed in order
    * to create a flat list of values (or if you know there is only one
    * line to be parsed). For example:
    * <pre>
    *   Seq[Int], List[Int], Vector[Int], ...
    * </pre>
    *
    * Examples:
    * {{{
    * // Type is Seq[Seq[Int]]
    * val v = fromCsv[Seq[Seq[Int]]]("1, 2, 3\n1, 2, 3")
    *
    * // Type is List[Int]
    * val v = fromCsv[List[Int]]("1, 2, 3")
    *
    * // Type is Any pointing to default Seq[Seq[Int]] 
    * val v = fromCsv[Any]("1, 2, 3")
    * }}}
    *
    * @return value to type A (usually Seq of Seq of String) 
    */
  def fromCsv[A](csv: Iterable[Char])(
    implicit m: Manifest[A], reifiableSettings: ReifiableSettings
  ): Option[A] = Csv.parser.fromCsv[A](csv)(m, reifiableSettings) match {
    case Right(v) => Some(v)
    case Left(x) => None
  }

  /** fromCsv with flag to trim whitespace */
  def fromCsv[A](csv: Iterable[Char], trimWhitespace: Boolean)(
    implicit m: Manifest[A], reifiableSettings: ReifiableSettings
  ): Option[A] = Csv.parser.fromCsv[A](
      csv, trimWhitespace)(m, reifiableSettings) match {
    case Right(v) => Some(v)
    case Left(x) => None
  }

  /** fromCsv with flag to trim whitespace and skip data not matching type */
  def fromCsv[A](
    csv: Iterable[Char], trimWhitespace: Boolean, skipNonMatchingData: Boolean
  )(
    implicit m: Manifest[A], reifiableSettings: ReifiableSettings
  ): Option[A] = Csv.parser.fromCsv[A](
      csv, trimWhitespace, skipNonMatchingData)(m, reifiableSettings) match {
    case Right(v) => Some(v)
    case Left(x) => None
  }

  /** Extracts on line by line basis. 
    *
    * {{{
    * matchEachCsvLine("a,b\nc,d") {
    *   case Csv(x, y) =>
    *     println("matched: " + x + "," + y)  // prints: a b then  c d
    * }
    * }}}
    */ 
  def matchEachCsvLine(csv: Iterable[Char])(pf: PartialFunction[String, _]) = {
    Csv.parser.fromNsv[Seq[String]](csv) match {
      case Right(r) => r foreach { line => pf.apply(line) }
      case _ => {}
    }
  }

  /** Converts given value to an RFC 4180 compatible CSV string
    *
    * Typically the value passed will be a seq of seq of values
    * (outer seq representing a line and the inner sequence representing
    * the fields of a given line). However, this method will also
    * support passing a single seq of values (to write out just one
    * line of CSV) or a primitive (to write out primitive as RFC 4180 
    * encoded CSV string)
    *
    * Types supported are same as for fromCsv
    *
    * @param value values (typically seq of seq of values (e.g. lines of csv))
    * @param padding optional padding between values
    * @return string CSV data
    */
  def toCsv[A : Manifest](x: A): String = Csv.parser.toCsv[A](x)

  /** Escapes a value so that it will not be interpreted as RFC 4180 CSV
    *
    * NOTE: Commas are escaped using RFC 4180 conventions.
    *
    * @param data value containing data needing encoding
    * @return encoded string
    */
  def toEscapedCsv[A : Manifest](x: A): String = Csv.parser.toEscapedCsv(x)


  ///////////////////////////////////////////////////////////////////////////
  // NSV Package Functions
  ///////////////////////////////////////////////////////////////////////////

  /** Converts RFC 4180 compatible new line separated data to given type A 
    *
    * The conversion takes into account any RFC4180 escaping of embedded
    * newline characters. However, it does not convert any CSV data that 
    * may be embedded in each line. To decode that as well use fromCsv.
    *
    * Any of the following types are supported for A:
    * <pre>
    *   Seq[String],  List[String],  Vector[String]
    *   Seq[Short],   List[Short],   Vector[Short]
    *   Seq[Int],     List[Int],     Vector[Int]
    *   Seq[Long],    List[Long],    Vector[Long]
    *   Seq[Float],   List[Float],   Vector[Float]
    *   Seq[Double],  List[Double],  Vector[Double]
    *   Seq[Boolean], List[Boolean], Vector[Boolean]
    *   Seq[Char],    List[Char],    Vector[Char]
    *   Seq[Any],     List[Any],     Vector[Any]
    *   Any
    * </pre>
    *
    * NOTE: This does NOT convert CSV data within lines, it only separates
    *       values per line. Use fromCsv if that is required.
    *
    * Example:
    * {{{
    * // Type is Seq[Int]
    * val v = fromNsv[Seq[Int]]("1\n2\n3")
    *
    * // Type is Int
    * val i = fromNsv[Int]("1")
    *
    * // Type is Any pointing to default Seq[String]
    * val v = fromNsv[Any]("1\n2\n3")
    * }}}
    *
    * @param nsv data in RFC4180 compatible format
    * @return value of type A (usually Seq of String) 
    */
  def fromNsv[A](nsv: Iterable[Char])(
    implicit m: Manifest[A], reifiableSettings: ReifiableSettings
  ): Option[A] = {
    Nsv.parser.fromNsv(nsv) match {
      case Right(r) => Some(r)
      case _ => None
    }
  }

  def fromNsv[A](nsv: Iterable[Char], skipNonMatching: Boolean)(
    implicit m: Manifest[A], reifiableSettings: ReifiableSettings
  ): Option[A] = {
    Nsv.parser.fromNsv(nsv, skipNonMatching) match {
      case Right(r) => Some(r)
      case _ => None
    }
  }

  /** Converts seq of values to NSV (newline separated values)
    *
    * The conversion takes into account RFC4180 escaping of embedded
    * newline characters. However, it does not convert the CSV data stored
    * in each line. To encode that as well, use toCsv.
    *
    * @param data the seq of lines
    * @return the NSV string data
    */
  def toNsv[A : Manifest](x: A): String = Csv.parser.toNsv[A](x)


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  object Csv {
    private[csv] val parser = new CsvParser() {}

    def unapplySeq(s: String): Option[Seq[String]] = {
      IterableCsv.unapplySeq(s)
    }
  }

  object IterableCsv {
    /** Extractor for CSV for when data input as iterable.
      *
      * NOTE: Because of type erasure this will not match on _* patterns.
      */
    def unapplySeq(iterable: Iterable[Char]): Option[Seq[String]] = {
      val iter = new CsvIterator(iterable)
      var result = Vector[String]()
      while (iter.hasNext) {
        result :+= iter.next()
      }
      Some(result)
    }
  }

  object Nsv {
    private[csv] val parser = new CsvParser() {}

    def unapplySeq(s: String): Option[Seq[String]] = {
      IterableNsv.unapplySeq(s)
    }
  }

  object IterableNsv {
    /** Extractor for NSV for when data input as iterable.
      *
      * NOTE: Because of type erasure this will not match on _* patterns.
      */
    def unapplySeq(iterable: Iterable[Char]): Option[Seq[String]] = {
      Nsv.parser.fromNsv(iterable) match {
        case Right(r) => Some(r)
        case _ => None
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  implicit def any2CsvPimp[A](x: A)(implicit 
    m: Manifest[A], reifiableSettings: ReifiableSettings
  ) = new CsvPimp[A](x, m, reifiableSettings)

  class CsvPimp[A](
    val value: A,
    val m: Manifest[A], 
    val reifiableSettings: ReifiableSettings
  ) {
    /** Applies operator to existing type and type extracted from CSV 
      * 
      * @param csv csv data
      * @param op operation taking current type and type converted from CSV 
      * @return type from apply operation or current type if conversion fails
      */
    def withCsv(csv: Iterable[Char])(op: (A, A) => A): A = {
      fromCsv[A](csv, true, false)(m, reifiableSettings) match {
        case Some(csvValue) => op(value, csvValue) 
        case _ => value 
      }
    }

    def toCsv(): String = scalafy.csv.toCsv(value)(m)

    def toEscapedCsv(): String = scalafy.csv.toEscapedCsv(value)(m)

    def toNsv(): String = scalafy.csv.toNsv(value)(m)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  private val Separator = ','
  private val Enclosure = '"'
  private val Escape = '\\'
  private val LineBreak = '\n'

  /** CSV parser */
  trait CsvParser {
    /** Parser for toCsv package function 
      *
      * @return either String error msg or data of given type
      */
    def fromCsv[A](
      csv: Iterable[Char], 
      trimWhitespace: Boolean = true, 
      skipNonMatching: Boolean = false
    )(
      implicit m : Manifest[A], reifiableSettings: ReifiableSettings
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
          if (reifiableSettings.enabled) {
            if (m == manifest[Any] || m == manifest[Nothing]) {
              Reifiable(manifest[Seq[String]], line)
            } else {
              Reifiable(m, line)
            } 
          }
          result :+= line
        }
        else result = line

        iter.nextValue() // skip stop for line break
        iter.nextValue() // skip stop to read line break
      }

      if (reifiableSettings.enabled) {
        if (m == manifest[Any] || m == manifest[Nothing]) {
          Reifiable(manifest[Seq[Seq[String]]], result)
        } else {
          Reifiable(m, result)
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
      implicit m : Manifest[A], reifiableSettings: ReifiableSettings
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
    
      if (reifiableSettings.enabled) {
        if (m == manifest[Any] || m == manifest[Nothing]) {
          Reifiable(manifest[Seq[String]], result)
        } else {
          Reifiable(m, result)
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
  private class CsvIterator(
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
  private class CsvStoppingIterator(
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
  private class NsvStoppingIterator(
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

} // end package object
