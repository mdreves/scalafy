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
package test.scalafy.csv

import org.specs2.mutable.Specification

import scalafy.csv._

/** Test specification for csv package */
object CsvSpec extends Specification {
  "The Csv object" should {
    "support extracting csv on a line by line basis" in {
      {
          var s = new StringBuilder
          "a,b\nc,d" matchEach {
            case Csv(x, y) => s.append(x + "," + y + "\n")
            case _ => s.append("")
          }
          s.toString
      } .mustEqual("a,b\nc,d\n")
    }

    "support extracting values in a single line" in {
      {
          "a,b" match {
            case Csv(a, b) => a + "," + b
            case _=> ""
          }
      } .mustEqual("a,b")
    }

    "support extracting values using variable wildcard pattern" in {
      {
          "a,b\nc,d" match {
            case Csv(rest @ _*) => rest
            case _ => ""
          }
      } .mustEqual(Seq("a","b","\n","c","d"))
    }

    "support extracting values over multiple lines in one extractor" in {
      {
          "a,b\nc,d" match {
            case Csv(a, b, Eol, c, d) => a + "," + b + "," + c + "," + d
            case _ => ""
          }
      } .mustEqual("a,b,c,d")

      {
          "a,b\nc,d" match {
            case Csv(a, b, "\n", c, d) => a + "," + b + "," + c + "," + d
            case _ => ""
          }
      } .mustEqual("a,b,c,d")

      {
          "a,b\nc,d" match {
            case Csv(a, _, _, _, d) => a + "," + d
            case _ => ""
          }
      } .mustEqual("a,d")
    }
  }

  "The Nsv object" should {
    "support extracting only lines" in {
      {
          "a,b\nc,d" match {
            case Nsv(line1, line2) => line1 + " " + line2
            case _ => ""
          }
      } .mustEqual("a,b c,d")
    }

    "support extracting lines using variable wildcard patterns" in {
      {
          "a,b\nc,d\ne,f" match {
            case Nsv(rest @ _*) => rest
          }
      } .mustEqual(Seq("a,b","c,d", "e,f"))
    }
  }

  "The Csv.fromCsvLine function" should {
    // Examples from: http://en.wikipedia.org/wiki/Comma-separated_values
    "support converting from RFC4180 compatible strings" in {
      Csv.fromCsvLine("1997,Ford,E350")
        .mustEqual(Right(Seq("1997", "Ford", "E350")))

      Csv.fromCsvLine("1997, Ford , E350", false)
        .mustEqual(Right(Seq("1997", " Ford "," E350")))

      Csv.fromCsvLine("1997,Ford,E350,\"Super, luxurious truck\"")
        .mustEqual(Right(Seq("1997","Ford","E350","Super, luxurious truck")))

      Csv.fromCsvLine("1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"")
        .mustEqual(
          Right(Seq("1997","Ford","E350","Super, \"luxurious\" truck")) )

      Csv.fromCsvLine(
          "1997,Ford,E350,\"Go get one now\nthey are going fast\"" )
        .mustEqual(
          Right(Seq("1997","Ford","E350","Go get one now\nthey are going fast")) )

      Csv.fromCsvLine(
          "1996,Jeep,Grand Cherokee,\"MUST SELL!\nair, moon roof, loaded\",4799.00" )
        .mustEqual(
          Right(Seq("1996","Jeep","Grand Cherokee", "MUST SELL!\nair, moon roof, loaded", "4799.00")) )

      Csv.fromCsvLine("1997,Ford,E350,\" Super luxurious truck \"", false)
        .mustEqual(
          Right(Seq("1997","Ford","E350"," Super luxurious truck ")))

      Csv.fromCsvLine("\"1997\",\"Ford\",\"E350\"")
        .mustEqual(Right(Seq("1997", "Ford","E350")))

      Csv.fromCsvLine("\"\"\"1997\",\"Ford\",\"E350\"")
        .mustEqual(Right(Seq("\"1997", "Ford","E350")))
    }

    "support returning single string if no commas used" in {
      Csv.fromCsvLine("No commas used").mustEqual(Left("No commas used"))
      Csv.fromCsvLine("\"Still no \"\" commas used\"")
        .mustEqual(Left("Still no \" commas used"))
    }
  }

  "The Csv.toCsvLine function" should {
    "support converting Seqs of fields to RFC4180 compatible strings" in {
      Csv.toCsvLine(Seq("1997", "Ford", "E350"))
        .mustEqual("1997,Ford,E350")

      Csv.toCsvLine(Seq("1997", " Ford "," E350"))
        .mustEqual("1997, Ford , E350")

      Csv.toCsvLine(Seq("1997","Ford","E350","Super, luxurious truck"))
        .mustEqual("1997,Ford,E350,\"Super, luxurious truck\"")

      Csv.toCsvLine(Seq("1997","Ford","E350","Super, \"luxurious\" truck"))
        .mustEqual("1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"")

      Csv.toCsvLine(
          Seq("1997","Ford","E350","Go get one now\nthey are going fast"))
        .mustEqual("1997,Ford,E350,\"Go get one now\nthey are going fast\"")

      Csv.toCsvLine(Seq("1997","Ford","E350"," Super luxurious truck "))
        .mustEqual("1997,Ford,E350, Super luxurious truck ")

      Csv.toCsvLine(Seq("\"1997\"","\"Ford\"","\"E350\""))
        .mustEqual("\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"")

      Csv.toCsvLine(Seq("\"1997", "Ford"," E350"))
        .mustEqual("\"\"\"1997\",Ford, E350")
    }

    "support converting a string to an RFC4180 compatible string" in {
      Csv.toCsvLine("No commas used")
        .mustEqual("No commas used")
      Csv.toCsvLine("\"Still no \"\" commas used\"")
        .mustEqual("\"\"\"Still no \"\"\"\" commas used\"\"\"")
    }
  }

  "The Csv.fromNsv function" should {
    "support convert NSV data to seq of strings" in {
      Csv.fromNsv("line1\nline2\nline3")
        .mustEqual(Seq("line1","line2","line3"))
      Csv.fromNsv("a,b\nc,d").mustEqual(Seq("a,b","c,d"))
    }

    "handle newlines escaped by quotes (RFC4180 format)" in {
      Csv.fromNsv("\"line1\nline2\"\nline3")
        .mustEqual(Seq("\"line1\nline2\"","line3"))

      Csv.fromNsv(
          "1997,Ford,E350,\"ac, abs, moon\",3000.00\n1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00" )
        .mustEqual(
          Seq("1997,Ford,E350,\"ac, abs, moon\",3000.00","1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00") )
    }

    "not interfer with RFC4180 encoded string data" in {
      Csv.fromNsv(
          "1997, Ford , E350\n1997,Ford,E350,\"Super, luxurious truck\"\n1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"\n1997,Ford,E350,\"Go get one now\nthey are going fast\"\n1997,Ford,E350, Super luxurious truck \n\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"\n\"\"\"1997\",Ford, E350" )
        .mustEqual(
          Seq("1997, Ford , E350", "1997,Ford,E350,\"Super, luxurious truck\"","1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"","1997,Ford,E350,\"Go get one now\nthey are going fast\"", "1997,Ford,E350, Super luxurious truck ", "\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"", "\"\"\"1997\",Ford, E350") )
    }
  }

  "The Csv.toNsv function" should {
    "support convert seq of strings to NSV data" in {
      Csv.toNsv(Seq("line1","line2","line3"))
        .mustEqual("line1\nline2\nline3")
      Csv.toNsv(Seq("a,b", "c,d")).mustEqual("a,b\nc,d")
    }

    "handle newlines escaped by quotes (RFC4180 format)" in {
      Csv.toNsv(Seq("\"line1\nline2\"","line3"))
        .mustEqual("\"line1\nline2\"\nline3")
    }

    "not interfer with RFC4180 encoded string data" in {
      Csv.toNsv(
          Seq("1997, Ford , E350", "1997,Ford,E350,\"Super, luxurious truck\"","1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"","1997,Ford,E350,\"Go get one now\nthey are going fast\"", "1997,Ford,E350, Super luxurious truck ", "\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"", "\"\"\"1997\",Ford, E350") )
        .mustEqual(
          "1997, Ford , E350\n1997,Ford,E350,\"Super, luxurious truck\"\n1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"\n1997,Ford,E350,\"Go get one now\nthey are going fast\"\n1997,Ford,E350, Super luxurious truck \n\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"\n\"\"\"1997\",Ford, E350" )
    }
  }

  "The Csv.fromCsv function" should {
    // Example from: http://en.wikipedia.org/wiki/Comma-separated_values
    "support conversion from RFC4180 string to seq of seq of strings" in {
      Csv.fromCsv("a,b\nc,d").mustEqual(Seq(Seq("a","b"),Seq("c","d")))

      Csv.fromCsv(
          "1997,Ford,E350,\"ac, abs, moon\",3000.00\n1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00\n1999,Chevy,\"Venture \"\"Extended Edition, Very Large\"\"\",\"\",5000.00\n1996,Jeep,Grand Cherokee,\"MUST SELL!\nair, moon roof, loaded\",4799.00")
        .mustEqual(
          Seq(Seq("1997","Ford","E350","ac, abs, moon", "3000.00"),Seq("1999","Chevy", "Venture \"Extended Edition\"","","4900.00"),Seq("1999","Chevy", "Venture \"Extended Edition, Very Large\"","","5000.00"),Seq("1996","Jeep","Grand Cherokee","MUST SELL!\nair, moon roof, loaded","4799.00")) )
    }
  }

  "The Csv.toCsv function" should {
    // Example from: http://en.wikipedia.org/wiki/Comma-separated_values
    "support conversion to RFC4180 string from seq of seq of strings" in {
      Csv.toCsv(Seq(Seq("a","b"),Seq("c","d"))).mustEqual("a,b\nc,d")

      Csv.toCsv(
          Seq(Seq("1997","Ford","E350","ac, abs, moon", "3000.00"),Seq("1999","Chevy", "Venture \"Extended Edition\"","","4900.00"),Seq("1999","Chevy", "Venture \"Extended Edition, Very Large\"","","5000.00"),Seq("1996","Jeep","Grand Cherokee","MUST SELL!\nair, moon roof, loaded","4799.00")))
        .mustEqual(
          "1997,Ford,E350,\"ac, abs, moon\",3000.00\n1999,Chevy,\"Venture \"\"Extended Edition\"\"\",,4900.00\n1999,Chevy,\"Venture \"\"Extended Edition, Very Large\"\"\",,5000.00\n1996,Jeep,Grand Cherokee,\"MUST SELL!\nair, moon roof, loaded\",4799.00" )
    }
  }
}
