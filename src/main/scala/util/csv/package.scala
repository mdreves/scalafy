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
package scalafy.util

import scalafy.types.reifiable.ReifiableSettings

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
  *
  * // Configuring default settings
  * implicit val csvSettings = CsvSettings(
  *   ReifiableSettings(false)      // disable reifiable types (default)
  * )
  * }}}
  */
package object csv {

  ///////////////////////////////////////////////////////////////////////////
  // vals and implicits 
  ///////////////////////////////////////////////////////////////////////////

  val Eol = "\n"

  case class CsvSettings(reifiableSettings: ReifiableSettings)

  implicit val csvSettings = CsvSettings(ReifiableSettings(false))


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
    implicit m: Manifest[A], settings: CsvSettings
  ): Option[A] = CsvParser.fromCsv[A](csv)(m, settings) match {
    case Right(v) => Some(v)
    case Left(x) => None
  }

  /** fromCsv with flag to trim whitespace */
  def fromCsv[A](csv: Iterable[Char], trimWhitespace: Boolean)(
    implicit m: Manifest[A], settings: CsvSettings
  ): Option[A] = CsvParser.fromCsv[A](
      csv, trimWhitespace)(m, settings) match {
    case Right(v) => Some(v)
    case Left(x) => None
  }

  /** fromCsv with flag to trim whitespace and skip data not matching type */
  def fromCsv[A](
    csv: Iterable[Char], trimWhitespace: Boolean, skipNonMatchingData: Boolean
  )(
    implicit m: Manifest[A], settings: CsvSettings
  ): Option[A] = CsvParser.fromCsv[A](
      csv, trimWhitespace, skipNonMatchingData)(m, settings) match {
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
    CsvParser.fromNsv[Seq[String]](csv) match {
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
  def toCsv[A : Manifest](x: A): String = CsvParser.toCsv[A](x)

  /** Escapes a value so that it will not be interpreted as RFC 4180 CSV
    *
    * NOTE: Commas are escaped using RFC 4180 conventions.
    *
    * @param data value containing data needing encoding
    * @return encoded string
    */
  def toEscapedCsv[A : Manifest](x: A): String = CsvParser.toEscapedCsv(x)


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
    implicit m: Manifest[A], settings: CsvSettings
  ): Option[A] = {
    CsvParser.fromNsv(nsv) match {
      case Right(r) => Some(r)
      case _ => None
    }
  }

  def fromNsv[A](nsv: Iterable[Char], skipNonMatching: Boolean)(
    implicit m: Manifest[A], settings: CsvSettings
  ): Option[A] = {
    CsvParser.fromNsv(nsv, skipNonMatching) match {
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
  def toNsv[A : Manifest](x: A): String = CsvParser.toNsv[A](x)


  ///////////////////////////////////////////////////////////////////////////
  // Package Objects 
  ///////////////////////////////////////////////////////////////////////////

  object Csv {
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
      CsvParser.fromNsv(iterable) match {
        case Right(r) => Some(r)
        case _ => None
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Pimps 
  ///////////////////////////////////////////////////////////////////////////

  implicit def any2CsvPimp[A](x: A)(implicit 
    m: Manifest[A], settings: CsvSettings
  ) = new CsvPimp[A](x, m, settings)

  final class CsvPimp[A](
    val value: A,
    val m: Manifest[A], 
    val settings: CsvSettings
  ) {
    /** Applies operator to existing type and type extracted from CSV 
      * 
      * @param csv csv data
      * @param op operation taking current type and type converted from CSV 
      * @return type from apply operation or current type if conversion fails
      */
    def withCsv(csv: Iterable[Char])(op: (A, A) => A): A = {
      fromCsv[A](csv, true, false)(m, settings) match {
        case Some(csvValue) => op(value, csvValue) 
        case _ => value 
      }
    }

    def toCsv(): String = scalafy.util.csv.toCsv(value)(m)

    def toEscapedCsv(): String = scalafy.util.csv.toEscapedCsv(value)(m)

    def toNsv(): String = scalafy.util.csv.toNsv(value)(m)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Helpers
  ///////////////////////////////////////////////////////////////////////////

  private[csv] val Separator = ','
  private[csv] val Enclosure = '"'
  private[csv] val Escape = '\\'
  private[csv] val LineBreak = '\n'

} // end package object
