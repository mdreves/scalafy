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

/** Contains objects used for CSV matching and conversion.
  *
  * The following is a summary of features:
  * {{{
  * // Extracting (csv for each line)
  * "a,b\nc,d" matchEach {
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
  *
  * // Parsing (comma separated data - as per RFC4180)
  * Csv.fromCsv("a,b\nc,d")  // Seq(Seq("a","b"),Seq("c","d"))
  * Csv.toCsv(Seq(Seq("a","b"),Seq("c","d")))  // "a,b\nc,d"
  *
  * // Parsing (newline separated data)
  * //   NOTE: Handles escaped newlines as per RFC4180
  * Csv.fromNsv("a,b\nc,d")  // Seq("a,b","c,d")
  * Csv.toNsv(Seq("a,b", "c,d"))  // "a,b\nc,d"
  * }}}
  */
package object csv {

  val Eol = "\n"

  object IterableCsv {
    /** Extractor for CSV for data input as iterable.
      *
      * NOTE: Because of type erasure this will not match on _* patterns.
      */
    def unapplySeq(iterable: Iterable[Char]): Option[Seq[String]] = {
      val iter = new Csv.CsvIterator(iterable)
      var result = Vector[String]()
      while (iter.hasNext) {
        result :+= iter.next()
      }
      Some(result)
    }
  }

  class Csv(val xs: Iterable[Char]) {
    def matchEach(pf: PartialFunction[String, _]) = {
      Csv.fromNsv(xs) foreach { line =>
        pf.apply(line)
      }
    }
  }

  object Csv extends CsvHelpers {
    def apply(iterable: Iterable[Char]) = new Csv(iterable)

    def unapplySeq(s: String): Option[Seq[String]] = {
      IterableCsv.unapplySeq(s)
    }
  }

  object IterableNsv {
    def unapplySeq(iterable: Iterable[Char]): Option[Seq[String]] = {
      Some(Csv.fromNsv(iterable))
    }
  }

  object Nsv {
    def unapplySeq(s: String): Option[Seq[String]] = {
      IterableNsv.unapplySeq(s)
    }
  }

  implicit def string2Csv(s: String) = new Csv(s)

  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  /** Casing helper methods */
  trait CsvHelpers {
    val Separator = ','
    val Enclosure = '"'
    val Escape = '\\'
    val LineBreak = '\n'

    /** Converts RFC 4180 compatible CSV data to a seq of seq of strings
      *
      * @param csv CSV string
      * @return seq of seq of strings (e.g. lines of csv data)
      */
    def fromCsv(
      csv: Iterable[Char], trimWhitespace: Boolean = true
    ): Seq[Seq[String]] = {
      var result = Vector[Vector[String]]()
      val iter = new CsvStoppingIterator(csv)
      while (iter.hasNextValue) {
        var line = Vector[String]()
        while (iter.hasNextValue && !iter.isStoppedOnLineBreak) {
          while (iter.hasNext) iter.next()
          if (trimWhitespace) {
            line :+= iter.getValue().trim
          } else {
            line :+= iter.getValue()
          }
          if (!iter.isStoppedOnLineBreak) iter.nextValue()
        }
        result :+= line
        iter.nextValue() // skip stop for line break
        iter.nextValue() // skip stop to read line break
      }
      result
    }

    /** Converts seq of seq of strings to an RFC 4180 compatible CSV string
      *
      * @param value seq of seq of strings (e.g. lines of csv data)
      * @return string CSV data
      */
    def toCsv(value: Seq[Seq[String]]): String = {
      value.foldLeft("")((result, line) => {
        val lineData = toCsvLine(line)
        if (result.length == 0) result + lineData
        else result + LineBreak + lineData
      })
    }

    /** Converts RFC 4180 compatible CSV data to a string or seq of values
      *
      * This method works on a single line of CSV data. If multiple lines of
      * CSV are used then fromNsv and/or fromCsv are more appropriate.
      *
      * If the input contains at least one (unescaped) comma, then a seq
      * will be returned (i.e. to ensure a seq is returned the input must
      * have at least one comma in it). An input sequence without any
      * commas (or with an encoded comma) will be returned as a single
      * decoded string. This allows this method to be used to decode data
      * that was not intended to be used as a CSV list but had to have the
      * commas encoded (using 4180 escaping conventions) to prevent
      * interpretation.
      *
      * @param csv data in RFC4180 compatible format
      * @param trimWhitespace true to trim whitespace around values
      * @return string (Left) or list of CSV data (Right)
      */
    def fromCsvLine(
      csv: Iterable[Char],
      trimWhitespace: Boolean = true
    ): Either[String, Seq[String]] = {
      if (csv == "") return Right(Vector[String]())

      var result = Vector[String]()
      val iter = new CsvStoppingIterator(csv)
      while (iter.hasNextValue && !iter.isStoppedOnLineBreak) {
        while (iter.hasNext) iter.next()
        if (trimWhitespace) {
          result :+= iter.getValue().trim
        } else {
          result :+= iter.getValue()
        }
        if (!iter.isStoppedOnLineBreak) iter.nextValue()
      }

      // If single item don't return list. Note that a list ending
      // in a comma will count as two items: "a," has "a" and ""
      if (result.length == 1) Left(result(0)) else Right(result)
    }

    /** Converts a string representing line of data to an RFC 4180 CSV string
      *
      * This method works on a single line of CSV data. If multiple lines of
      * CSV are used then first call toCsv and then toNsv
      *
      * Normally a list of strings that make up the values in the CSV list
      * are used but this overloaded method takes only a single string. It is
      * used to encode a string so that it will NOT be interpreted as a CSV
      * list (i.e. commas are escaped using RFC 4180 conventions).
      *
      * @param data string containing commas needing encoding
      * @param padding padding to add between values
      * @return encoded string
      */
    def toCsvLine(data: String): String = {
      // If no commas or linebreaks then just return what was passed
      if (
        data.indexOf(Separator) == -1 &&
        data.indexOf(LineBreak) == -1 &&
        data.indexOf(Enclosure) == -1
      ) {
        data
      } else {
        toCsvLine(List(data)).stripSuffix(",")
      }
    }

    /** Converts a list of values to a RFC 4180 CSV string.
      *
      * This method works on a single line of CSV data. If multiple lines of
      * CSV are used then first call toCsv and then toNsv
      *
      * If a list is passed with no values or with a single value then an
      * ending comma will be added to ensure that the list is decoded as a
      * list instead of a single string during decoding.
      *
      * @param data the list of strings
      * @param padding padding to add between values
      * @return a CSV string
      */
    def toCsvLine(data: Seq[String], padding: String = ""): String = {
      // If empty array just return separator
      if (data.size == 0) return Separator.toString

      var result = Vector[String]()

      for (d <- data) {
        var item = d
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

      // If list passed in and only one item, add comma to make sure it is
      // parsed as an list during decoding.
      if (result.size == 1)
        result.reduceLeft(_ + Separator + _) + Separator
      else
        result.reduceLeft(_ + Separator + _)
    }

    /** Converts string data from NSV (newline separated values) to a seq.
      *
      * The conversion takes into account any RFC4180 escaping of embedded
      * newline characters. However, it does not convert the CSV data stored
      * in each line. To decode that as well, call fromCsv on each item
      * returned
      *
      * @param nsv data in RFC4180 compatible format
      * @return the sequence of lines
      */
    def fromNsv(nsv: Iterable[Char]): Seq[String] = {
      var result = Vector[String]()
      val iter = new NsvStoppingIterator(nsv)
      while (iter.hasNextLine) {
        var line = new StringBuilder
        while (iter.hasNext) line += iter.next()
        result :+= line.toString
        iter.nextLine()
      }
      result
    }

    /** Converts seq of strings to NSV (newline separated values)
      *
      * The conversion takes into account RFC4180 escaping of embedded
      * newline characters. However, it does not convert the CSV data stored
      * in each line. To encode that as well, use toCsv.
      *
      * @param data the seq of lines
      * @return the NSV string data
      */
    def toNsv(data: Seq[String]): String = {
      var result = ""
      for (line <- data) {
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


    /** Iterates sequences of comma separated values
      *
      * Any newlines encounted will be treated as values.
      */
    class CsvIterator(
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
    class CsvStoppingIterator(
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
    class NsvStoppingIterator(
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
  }

} // end package object
