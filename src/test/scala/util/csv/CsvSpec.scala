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
package test.scalafy.util.csv

import org.specs2.mutable.Specification

import scalafy.util.csv._

/** Test specification for csv package */
object CsvSpec extends Specification {

  "The fromCsv function" should {
    // Examples from: http://en.wikipedia.org/wiki/Comma-separated_values
    "support converting from RFC4180 compatible strings to Seq of String" in {
      fromCsv[Seq[String]]("1997,Ford,E350")
        .mustEqual(Some(Seq("1997", "Ford", "E350")))

      fromCsv[Seq[String]]("1997, Ford , E350", false)
        .mustEqual(Some(Seq("1997", " Ford "," E350")))

      fromCsv[Seq[String]]("1997,Ford,E350,\"Super, luxurious truck\"")
        .mustEqual(Some(Seq("1997","Ford","E350","Super, luxurious truck")))

      fromCsv[Seq[String]]("1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"")
        .mustEqual(
          Some(Seq("1997","Ford","E350","Super, \"luxurious\" truck")) )

      fromCsv[Seq[String]](
          "1997,Ford,E350,\"Go get one now\nthey are going fast\"" )
        .mustEqual(
          Some(Seq("1997","Ford","E350","Go get one now\nthey are going fast")) )

      fromCsv[Seq[String]](
          "1996,Jeep,Grand Cherokee,\"MUST SELL!\nair, moon roof, loaded\",4799.00" )
        .mustEqual(
          Some(Seq("1996","Jeep","Grand Cherokee", "MUST SELL!\nair, moon roof, loaded", "4799.00")) )

      fromCsv[Seq[String]]("1997,Ford,E350,\" Super luxurious truck \"", false)
        .mustEqual(
          Some(Seq("1997","Ford","E350"," Super luxurious truck ")))

      fromCsv[Seq[String]]("\"1997\",\"Ford\",\"E350\"")
        .mustEqual(Some(Seq("1997", "Ford","E350")))

      fromCsv[Seq[String]]("\"\"\"1997\",\"Ford\",\"E350\"")
        .mustEqual(Some(Seq("\"1997", "Ford","E350")))
    }

    // Example from: http://en.wikipedia.org/wiki/Comma-separated_values
    "support conversion from RFC4180 string to seq of seq of strings" in {
      fromCsv[Seq[Seq[String]]]("a,b\nc,d")
        .mustEqual(Some(Seq(Seq("a","b"),Seq("c","d"))))

      fromCsv[Seq[Seq[String]]]("1997,Ford,E350,\"ac, abs, moon\",3000.00\n1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00\n1999,Chevy,\"Venture \"\"Extended Edition, Very Large\"\"\",\"\",5000.00\n1996,Jeep,Grand Cherokee,\"MUST SELL!\nair, moon roof, loaded\",4799.00")
        .mustEqual(
          Some(Seq(Seq("1997","Ford","E350","ac, abs, moon", "3000.00"),Seq("1999","Chevy", "Venture \"Extended Edition\"","","4900.00"),Seq("1999","Chevy", "Venture \"Extended Edition, Very Large\"","","5000.00"),Seq("1996","Jeep","Grand Cherokee","MUST SELL!\nair, moon roof, loaded","4799.00") )) )
    }
    
    "support conversion from RFC4180 string to other sequence types" in {
      fromCsv[List[List[String]]]("a,b\nc,d")
        .mustEqual(Some(List(List("a","b"),List("c","d"))))
      fromCsv[List[String]]("a,b")
        .mustEqual(Some(List("a","b")))
      fromCsv[Vector[Vector[String]]]("a,b\nc,d")
        .mustEqual(Some(Vector(Vector("a","b"),Vector("c","d"))))
      fromCsv[Vector[String]]("a,b")
        .mustEqual(Some(Vector("a","b")))
    }
    
    "support conversion from RFC4180 string to other primitive types" in {
      fromCsv[List[List[Int]]]("1,2\n3,4")
        .mustEqual(Some(List(List(1,2),List(3,4))))
      fromCsv[List[Int]]("1,2")
        .mustEqual(Some(List(1,2)))
      fromCsv[List[List[Short]]]("1,2\n3,4")
        .mustEqual(Some(List(List[Short](1,2),List[Short](3,4))))
      fromCsv[List[List[Long]]]("1,2\n3,4")
        .mustEqual(Some(List(List[Long](1,2),List[Long](3,4))))
      fromCsv[List[List[Float]]]("1.0,2.0\n3.1,4.2")
        .mustEqual(Some(List(List(1.0f,2.0f),List(3.1f,4.2f))))
      fromCsv[List[List[Double]]]("1.0,2.0\n3.1,4.2")
        .mustEqual(Some(List(List(1.0,2.0),List(3.1,4.2))))
      fromCsv[List[List[Boolean]]]("true,true\ntrue,false")
        .mustEqual(Some(List(List(true,true),List(true,false))))
      fromCsv[List[List[Char]]]("a,b\nc,d")
        .mustEqual(Some(List(List('a','b'),List('c','d'))))
    }

    "support conversion from RFC4180 string to using Any" in {
      fromCsv[Any]("1,2\n3,4")
        .mustEqual(Some(List(List("1","2"),List("3","4"))))
      fromCsv[Nothing]("1,2\n3,4")
        .mustEqual(Some(List(List("1","2"),List("3","4"))))
    }
    
    "support skipping values not matching type" in {
      fromCsv[List[List[Int]]]("1,2\nfoo,4", true, true)
        .mustEqual(Some(List(List(1,2),List(4))))
    }
  }

  "The toCsv function" should {
    "support converting Seqs of fields to RFC4180 compatible strings" in {
      toCsv(Seq("1997", "Ford", "E350")).mustEqual("1997,Ford,E350")

      toCsv(Seq("1997", " Ford "," E350")).mustEqual("1997, Ford , E350")

      toCsv(Seq("1997","Ford","E350","Super, luxurious truck"))
        .mustEqual("1997,Ford,E350,\"Super, luxurious truck\"")

      toCsv(Seq("1997","Ford","E350","Super, \"luxurious\" truck"))
        .mustEqual("1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"")

      toCsv(Seq("1997","Ford","E350","Go get one now\nthey are going fast"))
        .mustEqual("1997,Ford,E350,\"Go get one now\nthey are going fast\"")

      toCsv(Seq("1997","Ford","E350"," Super luxurious truck "))
        .mustEqual("1997,Ford,E350, Super luxurious truck ")

      toCsv(Seq("\"1997\"","\"Ford\"","\"E350\""))
        .mustEqual("\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"")

      toCsv(Seq("\"1997", "Ford"," E350"))
        .mustEqual("\"\"\"1997\",Ford, E350")
    }
    
    // Example from: http://en.wikipedia.org/wiki/Comma-separated_values
    "support conversion to RFC4180 string from seq of seq of strings" in {
      toCsv(Seq(Seq("a","b"),Seq("c","d"))).mustEqual("a,b\nc,d")

      toCsv(Seq(Seq("1997","Ford","E350","ac, abs, moon", "3000.00"),Seq("1999","Chevy", "Venture \"Extended Edition\"","","4900.00"),Seq("1999","Chevy", "Venture \"Extended Edition, Very Large\"","","5000.00"),Seq("1996","Jeep","Grand Cherokee","MUST SELL!\nair, moon roof, loaded","4799.00")))
        .mustEqual(
          "1997,Ford,E350,\"ac, abs, moon\",3000.00\n1999,Chevy,\"Venture \"\"Extended Edition\"\"\",,4900.00\n1999,Chevy,\"Venture \"\"Extended Edition, Very Large\"\"\",,5000.00\n1996,Jeep,Grand Cherokee,\"MUST SELL!\nair, moon roof, loaded\",4799.00" )
    }

    "support converting a string to an RFC4180 compatible string" in {
      toCsv("No commas used").mustEqual("No commas used")
      toCsv("\"Still no \"\" commas used\"")
        .mustEqual("\"\"\"Still no \"\"\"\" commas used\"\"\"")
    }

    "support conversion to CSV from other sequence types" in {
      toCsv(List(List("a","b"),List("c","d"))).mustEqual("a,b\nc,d")
      toCsv(Vector(Vector("a","b"),Vector("c","d"))).mustEqual("a,b\nc,d")
    }

    "support conversion to CSV from other primitive types" in {
      toCsv(List(List(1,2),List(3,4))).mustEqual("1,2\n3,4")
      toCsv(List(List[Short](1,2),List[Short](3,4))).mustEqual("1,2\n3,4")
      toCsv(List(List[Long](1,2),List[Long](3,4))).mustEqual("1,2\n3,4")
      toCsv(List(List[Float](1.0f,2.0f),List[Float](3.1f,4.2f)))
        .mustEqual("1.0,2.0\n3.1,4.2")
      toCsv(List(List(1.0,2.0),List(3.1,4.2))).mustEqual("1.0,2.0\n3.1,4.2")
      toCsv(List(List(true,false),List(true,true)))
        .mustEqual("true,false\ntrue,true")
      toCsv(List(List('a','b'),List('c','d'))).mustEqual("a,b\nc,d")
      toCsv(List(List("a","b"),List("c","d")): Any).mustEqual("a,b\nc,d")
    }
  }
  
  "The toEscapedCsv function" should {
    "support property escapped CSV data" in {
      toEscapedCsv[String]("a,b,c,d\"e\"").mustEqual("\"a,b,c,d\"\"e\"\"\"")
    }
  }

  "The matchEachCsvLine function" should {
    "support extracting csv on a line by line basis" in {
      {
          var s = new StringBuilder
          matchEachCsvLine("a,b\nc,d") {
            case Csv(x, y) => s.append(x + "," + y + "\n")
            case _ => s.append("")
          }
          s.toString
      } .mustEqual("a,b\nc,d\n")
    }
  }

  "The fromNsv function" should {
    "support convert NSV data to seq of strings" in {
      fromNsv[Seq[String]]("line1\nline2\nline3")
        .mustEqual(Some(Seq("line1","line2","line3")))
      fromNsv[Seq[String]]("a,b\nc,d").mustEqual(Some(Seq("a,b","c,d")))
    }

    "handle newlines escaped by quotes (RFC4180 format)" in {
      fromNsv[Seq[String]]("\"line1\nline2\"\nline3")
        .mustEqual(Some(Seq("\"line1\nline2\"","line3")))

      fromNsv[Seq[String]]("1997,Ford,E350,\"ac, abs, moon\",3000.00\n1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00" )
        .mustEqual(
          Some(Seq("1997,Ford,E350,\"ac, abs, moon\",3000.00","1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00") ))
    }

    "not interfer with RFC4180 encoded string data" in {
      fromNsv[Seq[String]]("1997, Ford , E350\n1997,Ford,E350,\"Super, luxurious truck\"\n1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"\n1997,Ford,E350,\"Go get one now\nthey are going fast\"\n1997,Ford,E350, Super luxurious truck \n\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"\n\"\"\"1997\",Ford, E350" )
        .mustEqual(
          Some(Seq("1997, Ford , E350", "1997,Ford,E350,\"Super, luxurious truck\"","1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"","1997,Ford,E350,\"Go get one now\nthey are going fast\"", "1997,Ford,E350, Super luxurious truck ", "\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"", "\"\"\"1997\",Ford, E350") ))
    }

    "support conversion from RFC4180 string to other sequence types" in {
      fromNsv[List[String]]("a\nb").mustEqual(Some(List("a","b")))
      fromNsv[Vector[String]]("a\nb").mustEqual(Some(Vector("a","b")))
    }
    
    "support conversion from RFC4180 string to other primitive types" in {
      fromNsv[List[Int]]("1\n2").mustEqual(Some(List(1,2)))
      fromNsv[List[Short]]("1\n2").mustEqual(Some(List[Short](1,2)))
      fromNsv[List[Long]]("1\n2").mustEqual(Some(List[Long](1,2)))
      fromNsv[List[Float]]("1.0\n2.0").mustEqual(Some(List(1.0f,2.0f)))
      fromNsv[List[Double]]("1.0\n2.0").mustEqual(Some(List(1.0,2.0)))
      fromNsv[List[Boolean]]("true\ntrue").mustEqual(Some(List(true,true)))
      fromNsv[List[Char]]("a\nb").mustEqual(Some(List('a','b')))
    }

    "support conversion from RFC4180 string to using Any" in {
      fromNsv[Any]("1\n2").mustEqual(Some(List("1","2")))
      fromNsv[Nothing]("1\n2").mustEqual(Some(List("1","2")))
    }

    "support skipping values not matching type" in {
      fromNsv[List[Int]]("1\nfoo\n4", true)
        .mustEqual(Some(List(1,4)))
    }
  }

  "The toNsv function" should {
    "support converting seq of strings to NSV data" in {
      toNsv(Seq("line1","line2","line3"))
        .mustEqual("line1\nline2\nline3")
      toNsv(Seq("a,b", "c,d")).mustEqual("a,b\nc,d")
    }

    "handle newlines escaped by quotes (RFC4180 format)" in {
      toNsv(Seq("\"line1\nline2\"","line3"))
        .mustEqual("\"line1\nline2\"\nline3")
    }

    "not interfer with RFC4180 encoded string data" in {
      toNsv(Seq("1997, Ford , E350", "1997,Ford,E350,\"Super, luxurious truck\"","1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"","1997,Ford,E350,\"Go get one now\nthey are going fast\"", "1997,Ford,E350, Super luxurious truck ", "\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"", "\"\"\"1997\",Ford, E350"))
        .mustEqual(
          "1997, Ford , E350\n1997,Ford,E350,\"Super, luxurious truck\"\n1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"\n1997,Ford,E350,\"Go get one now\nthey are going fast\"\n1997,Ford,E350, Super luxurious truck \n\"\"\"1997\"\"\",\"\"\"Ford\"\"\",\"\"\"E350\"\"\"\n\"\"\"1997\",Ford, E350" )
    }

    "support conversion to NSV from other sequence types" in {
      toNsv(List("line1","line2","line3")).mustEqual("line1\nline2\nline3")
      toNsv(Vector("line1","line2","line3")).mustEqual("line1\nline2\nline3")
    }

    "support conversion to NSV from other primitive types" in {
      toNsv(List(1,2,3)).mustEqual("1\n2\n3")
      toNsv(List[Short](1,2,3)).mustEqual("1\n2\n3")
      toNsv(List[Long](1,2,3)).mustEqual("1\n2\n3")
      toNsv(List(1.0f,2.0f,3.1f)).mustEqual("1.0\n2.0\n3.1")
      toNsv(List(1.0,2.0,3.1)).mustEqual("1.0\n2.0\n3.1")
      toNsv(List(true,false,true)).mustEqual("true\nfalse\ntrue")
      toNsv(List('a','b','c')).mustEqual("a\nb\nc")
      toNsv(List("line1","line2","line3"): Any).mustEqual("line1\nline2\nline3")
    }
  }

  "The Csv object" should {
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
}
