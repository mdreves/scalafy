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
package scalafy.types

import scalafy.types.extractors.BasicExtractor

/** Basic object types.
  *
  * The following is a summary of features:
  * {{{
  * // Extraction using ->
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
package object basic {

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

    // Scala Int
    val MaxValue = scala.Int.MaxValue
    val MinValue = scala.Int.MinValue

    def box(x: Int) = scala.Int.box(x)
    override def toString() = scala.Int.toString()
    def unbox(x: AnyRef) = scala.Int.unbox(x)
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

    // Scala Short 
    val MaxValue = scala.Short.MaxValue
    val MinValue = scala.Short.MinValue

    def box(x: Short) = scala.Short.box(x)
    override def toString() = scala.Short.toString()
    def unbox(x: AnyRef) = scala.Short.unbox(x)
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

    // Scala Long 
    val MaxValue = scala.Long.MaxValue
    val MinValue = scala.Long.MinValue

    def box(x: Long) = scala.Long.box(x)
    override def toString() = scala.Long.toString()
    def unbox(x: AnyRef) = scala.Long.unbox(x)
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

    // Scala Float 
    val MaxValue = scala.Float.MaxValue
    val MinPositiveValue = scala.Float.MinPositiveValue
    val MinValue = scala.Float.MinValue
    val NaN = scala.Float.NaN
    val NegativeInfinity = scala.Float.NegativeInfinity
    val PositiveInfinity = scala.Float.PositiveInfinity

    def box(x: Float) = scala.Float.box(x)
    override def toString() = scala.Float.toString()
    def unbox(x: AnyRef) = scala.Float.unbox(x)
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

    // Scala Double 
    val MaxValue = scala.Double.MaxValue
    val MinPositiveValue = scala.Double.MinPositiveValue
    val MinValue = scala.Double.MinValue
    val NaN = scala.Double.NaN
    val NegativeInfinity = scala.Double.NegativeInfinity
    val PositiveInfinity = scala.Double.PositiveInfinity

    def box(x: Double) = scala.Double.box(x)
    override def toString() = scala.Double.toString()
    def unbox(x: AnyRef) = scala.Double.unbox(x)
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

    // Scala Boolean 
    def box(x: Boolean) = scala.Boolean.box(x)
    override def toString() = scala.Boolean.toString()
    def unbox(x: AnyRef) = scala.Boolean.unbox(x)

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

    // Scala Char 
    val MaxValue = scala.Char.MaxValue
    val MinValue = scala.Char.MinValue

    def box(x: Char) = scala.Char.box(x)
    override def toString() = scala.Char.toString()
    def unbox(x: AnyRef) = scala.Char.unbox(x)
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

    // Scala Byte 
    val MaxValue = scala.Byte.MaxValue
    val MinValue = scala.Byte.MinValue

    def box(x: Byte) = scala.Byte.box(x)
    override def toString() = scala.Byte.toString()
    def unbox(x: AnyRef) = scala.Byte.unbox(x)
  }

} // end package object
