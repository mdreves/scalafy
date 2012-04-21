package test.scalafy

import java.io.BufferedReader
import java.io.Reader
import java.io.File
import java.io.FileReader
import java.io.PrintWriter

import scala.reflect.BeanProperty
import scala.collection.mutable.ListBuffer

import scalafy.util.json._ 

/**** need to include libs in project
import sjson.json.Serializer._
import sjson.json._
import sjson.json.DefaultProtocol._
import sjson.json.JsonSerialization._
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
****/

/**
  * Test results (Mac 1.7 Ghz i5, 4GB)
  *
  *   TestClass2 @ 1,000,000
  *
  *     scalafy:
  *       to      : 0m23.485s
  *       to/from : 0m56.860s
  *
  *     lift-json (default):
  *       to      : 0m45.550s
  *       to/from : 1m23.312s
  *
  *     lift-json (with serializer):
  *       to      : 0m23.067s
  *       to/from : 0m44.577s
  *
  *     sjson (default)
  *       to      : 0m29.658s
  *       to/from : 30m31.673s
  *
  *     sjson (with typeclasses)
  *       to      : 0m49.739s
  *       to/from : 29m0.887s
  *
  *  List[TestClass2] @ 10,000 items
  * 
  *     scalafy:
  *       to file   : 0m0.961s 
  *       from file : 0m2.097s
  *
  *     lift-json
  *       to file   : 0m1.748s
  *       from file : 0m1.835s
  *
  *  List[TestClass2] @ 100,000 items
  * 
  *     scalafy:
  *       to file   : 0m3.595s 
  *       from file : 2m53.819s
  *
  *     lift-json 
  *       to file   : (out of memory issues)
  *
  *  Stream[TestClass2] @ 100,000 items
  * 
  *     scalafy:
  *       to file   : 0m3.755s
  *       from file : 0m5.167s     (skipping over until last)
  *
  *  NOTE: Stream has memory issues at 1,000,000 items even if drop used
  *
  *  Iterator[TestClass2] @ 1,000,000 items
  *   
  *     scalafy: 
  *       to file   : 0m27.874s
  *       from file : 0m32.013s
  */
object PerfTests {

  var testObjs: List[TestClass2] = List[TestClass2]() 
  val testSize = 1000000

  def main(args: Array[String]): Unit = {
    println("\n--------- Starting Tests -------\n")

    println("\n--------- Creating test objects -------\n")
    printTime(createTestObjects)

    println("\n--------- Converting to JSON -------\n")
    printTime(toFileTest)

    println("\n--------- Converting to and from JSON -------\n")
    printTime(fromFileTest)

    println("\n--------- Done -------\n")
  }

  def time(f: => Unit): Long = {
    val start = System.currentTimeMillis()
    f
    System.currentTimeMillis() - start
  }

  def printTime(f: => Unit): Unit = {
    val total = time(f)
    printf("\nTotal time: %dm%d.%03ds\n", 
      total / 1000 / 60, (total / 1000) % 60, total % 1000)
  }

  def createTestObjects: Unit = {
    testObjs = List[TestClass2]()
    for (_ <- 0 until testSize) {
      testObjs ::= new TestClass2(
        new TestClass1(
          "this is some test data", 10000, 1234, 123123123L, 1.23412f, 1222.2,
          'z', false, 12),
        new TestClass1(
          "this is some test data", 10000, 1234, 123123123L, 1.23412f, 1222.2,
          'z', false, 12)
      )
    }
  }

  def toTest {
    for (x <- testObjs) toJson(x)
  }

  def toFromTest {
    for (x <- testObjs) fromJson[TestClass2](toJson(x))
  }

  def toFileTest {
    val p = new PrintWriter(new File("test.json"))
    toJson(p, testObjs)
    testObjs = null
    p.close()
  }

  def fromFileTest {
    val r = new BufferedReader(new FileReader("test.json"))
    //println(fromJson[List[TestClass2]](r).get.head)
    //var s = fromJson[Stream[TestClass2]](r).get
    //while (!s.isEmpty) s = s.drop(1)
    val i = fromJson[Iterator[TestClass2]](r).get
    while (i.hasNext) i.next()
    r.close()
  }

/**** need to include libs in project

  // SJSON (default)

  def sjsonDefault_toTest {
    for (x <- testObjs) SJSON.out(x).toString
  }

  def sjsonDefault_toFromTest {
    for (x <- testObjs) 
      SJSON.in[TestClass2](JsValue.fromString(SJSON.out(x).toString))
  }

  // SJSON (typeclasses)

  def sjsonTypeclass_toTest {
    val fmt = TestClass2Protocol.TestClass2Format
    for (x <- testObjs) tojson(x)(fmt).toString
  }

  def sjsonTypeclass_toFromTest {
    val fmt = TestClass2Protocol.TestClass2Format
    for (x <- testObjs) fromjson[TestClass2](
      JsValue.fromString(tojson(x)(fmt).toString))(fmt)
  }

  // lift-json (reflection)

  def liftDefault_toTest {
    implicit val formats = Serialization.formats(NoTypeHints)

    for (x <- testObjs) write(x)
  }

  def liftDefault_toFromTest {
    implicit val formats = Serialization.formats(NoTypeHints)

    for (x <- testObjs) read[TestClass2](write(x))
  }

  def liftDefault_toFileTest {
    implicit val formats = Serialization.formats(NoTypeHints)
    val p = new PrintWriter(new File("test.json"))
    write(testObjs, p)
    p.close()
    testObjs = null
  }

  def liftDefault_fromFileTest {
    implicit val formats = Serialization.formats(NoTypeHints)

    val r = new BufferedReader(new FileReader("test.json"))
    println(read[List[TestClass2]](r).head)
    r.close()
  }

  // lift-json (seralizer)

  def liftSerializer_toTest {
    implicit val formats = (Serialization.formats(NoTypeHints) + 
      new TestClass2Serializer)

    for (x <- testObjs) write(x)
  }

  def liftSerializer_toFromTest {
    implicit val formats = (Serialization.formats(NoTypeHints) + 
      new TestClass2Serializer)

    for (x <- testObjs) read[TestClass2](write(x))
  }

****/

}

// Test Classes 
//  Note: bean properties not necessary, only for performance comparisons
//        with other libraries that require them
class TestClass1 {
  @BeanProperty
  var str: String = _
  @BeanProperty
  var num: Int = _
  @BeanProperty
  var sht: Int = _    // SJSON doesn't support Short, using Int
  @BeanProperty
  var lng: Long = _
  @BeanProperty
  var flt: Float = _ 
  @BeanProperty
  var dbl: Double = _
  @BeanProperty
  var ch: Int = _     // SJSON doesn't support Char, using Int
  @BeanProperty
  var bool: Boolean = _
  @BeanProperty
  var byt: Int = _    // SJSON doesn't support Byte, using Int

  def this(
    str: String, num: Int, sht: Int, lng: Long, flt: Float, 
    dbl: Double, ch: Int, bool: Boolean, byt: Int
  ) = {
    this()
    this.str = str
    this.num = num
    this.sht = sht
    this.lng = lng
    this.flt = flt
    this.dbl = dbl
    this.ch = ch
    this.bool = bool
    this.byt = byt
  }
  
  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[TestClass1]) return false
    val o = that.asInstanceOf[TestClass1]
    o.str == str && o.num == num && o.sht == sht && o.lng == lng &&
    o.flt == flt && o.dbl == dbl && o.ch == ch && o.bool == bool && o.byt == byt
  }

  override def toString = "TestClass1(" + 
    List(str, num, sht, lng, flt, dbl, ch, bool, byt).mkString(",") + ")"
}

class TestClass2 {
  @BeanProperty
  var t1: TestClass1 = _
  @BeanProperty
  var t2: TestClass1 = _

  def this(t1: TestClass1, t2: TestClass1) = {
    this()
    this.t1 = t1
    this.t2 = t2
  }

  override def equals(that: Any): Boolean = {
    if (that == null || !that.isInstanceOf[TestClass2]) return false
    val o = that.asInstanceOf[TestClass2]
    o.t1 == t1 && o.t2 == t2 
  }

  override def toString = "TestClass2(" + List(t1, t2).mkString(",") + ")"
}


/**** need to include libs in project

// SJSON Protocols
object TestClass1Protocol extends DefaultProtocol {
  import dispatch.json._
  import JsonSerialization._

  implicit object TestClass1Format extends Format[TestClass1] {
    def reads(json: JsValue): TestClass1 = json match {
      case JsObject(o) =>
        new TestClass1(
          fromjson[String](o(JsString("str"))), 
          fromjson[Int](o(JsString("num"))), 
          fromjson[Int](o(JsString("sht"))), 
          fromjson[Long](o(JsString("lng"))), 
          fromjson[Float](o(JsString("flt"))), 
          fromjson[Double](o(JsString("dbl"))), 
          fromjson[Int](o(JsString("ch"))),
          fromjson[Boolean](o(JsString("bool"))), 
          fromjson[Int](o(JsString("byt"))))
      case _ => throw new RuntimeException("JsObject expected")
    }

    def writes(o: TestClass1): JsValue =
      JsObject(List(
        (tojson("str").asInstanceOf[JsString], tojson(o.str)),
        (tojson("num").asInstanceOf[JsString], tojson(o.num)),
        (tojson("sht").asInstanceOf[JsString], tojson(o.sht)),
        (tojson("lng").asInstanceOf[JsString], tojson(o.lng)),
        (tojson("flt").asInstanceOf[JsString], tojson(o.flt)),
        (tojson("dbl").asInstanceOf[JsString], tojson(o.dbl)),
        (tojson("ch").asInstanceOf[JsString], tojson(o.ch)),
        (tojson("bool").asInstanceOf[JsString], tojson(o.bool)),
        (tojson("byt").asInstanceOf[JsString], tojson(o.byt)) ))
  }
}


object TestClass2Protocol extends DefaultProtocol {
  import dispatch.json._
  import JsonSerialization._

  implicit object TestClass2Format extends Format[TestClass2] {
    def reads(json: JsValue): TestClass2 = json match {
      case JsObject(o) =>
        new TestClass2(
          fromjson[TestClass1](o(JsString("t1")))(
            TestClass1Protocol.TestClass1Format),
          fromjson[TestClass1](o(JsString("t2")))(
            TestClass1Protocol.TestClass1Format))
      case _ => throw new RuntimeException("JsObject expected")
    }

    def writes(o: TestClass2): JsValue =
      JsObject(List(
        (tojson("t1").asInstanceOf[JsString], 
          tojson(o.t1)(TestClass1Protocol.TestClass1Format)),
        (tojson("t2").asInstanceOf[JsString], 
          tojson(o.t2)(TestClass1Protocol.TestClass1Format)) ))
  }
}

// lift-json Serializers
class TestClass1Serializer extends Serializer[TestClass1] {
  private val TestClass1Class = classOf[TestClass1]

  def deserialize(
    implicit format: Formats
  ): PartialFunction[(TypeInfo, JValue), TestClass1] = {
    case (TypeInfo(TestClass1Class, _), json) => json match {
      case JObject(
          JField("str", JString(str)) :: JField("num", JInt(num)) :: 
          JField("sht", JInt(sht)) :: JField("lng", JInt(lng)) ::
          JField("flt", JDouble(flt)) :: JField("dbl", JDouble(dbl)) ::
          JField("ch", JInt(ch)) :: JField("bool", JBool(bool)) ::
          JField("byt", JInt(byt)) :: Nil) =>
            new TestClass1(str, num.intValue, sht.intValue, lng.longValue, 
              flt.floatValue, dbl, ch.intValue, bool, byt.intValue)
      case x => 
        throw new MappingException("Can't convert " + x + " to TestClass1")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: TestClass1 =>
      JObject(
        JField("str", JString(x.str)) :: JField("num", JInt(BigInt(x.num))) :: 
        JField("sht", JInt(BigInt(x.sht))) :: 
        JField("lng", JInt(BigInt(x.lng))) :: 
        JField("flt", JDouble(x.flt)) :: JField("dbl", JDouble(x.dbl)) :: 
        JField("ch", JInt(BigInt(x.ch))) :: JField("bool", JBool(x.bool)) :: 
        JField("byt", JInt(BigInt(x.byt))) :: Nil)
  }
}

class TestClass2Serializer extends Serializer[TestClass2] {
  private val typeInfo = TypeInfo(classOf[TestClass1], None)
  private val testClass2Class = classOf[TestClass2]
  private val testClass1Serializer = new TestClass1Serializer
  private val testClass1Format = (Serialization.formats(NoTypeHints) + 
      testClass1Serializer)

  def deserialize(
    implicit format: Formats
  ): PartialFunction[(TypeInfo, JValue), TestClass2] = {
    case (TypeInfo(testClass2Class, _), json) => json match {
      case JObject(
          JField("t1", t1) :: JField("t2", t2) :: Nil) =>
            new TestClass2(
              testClass1Serializer.deserialize(
                testClass1Format)(typeInfo -> t1),
              testClass1Serializer.deserialize(
                testClass1Format)(typeInfo -> t2))
      case x => 
        throw new MappingException("Can't convert " + x + " to TestClass2")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: TestClass2 =>
      JObject(
        JField("t1", testClass1Serializer.serialize(testClass1Format)(x.t1)) :: 
        JField("t2", testClass1Serializer.serialize(testClass1Format)(x.t2)) :: 
        Nil)
  }
}

******/
