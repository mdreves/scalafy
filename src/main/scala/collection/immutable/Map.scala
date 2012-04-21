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
