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
package scalafy.collection.uniform

/** Poor mans uniform list buffer */
trait UniformListBuffer[A] {

  import scala.collection.GenTraversableOnce
  import scala.collection.mutable.ListBuffer

  // Wrapped container
  private[uniform] val buffer: ListBuffer[A]


  def ++ [A1 : UniformType](
    xs: GenTraversableOnce[A1]
  ): UniformListBuffer[Any] = 
    UniformListBuffer.createListBuffer(buffer.++(normalize(xs.toList : _*)))

  def ++: [A1 : UniformType](
    xs: Traversable[A1]
  ): UniformListBuffer[Any] = 
    UniformListBuffer.createListBuffer(buffer.++:(normalize(xs.toSeq : _*)))

  def +: [A1 : UniformType](elem: A1): UniformListBuffer[Any] = 
    UniformListBuffer.createListBuffer(buffer.+:(normalize(elem)(0)))

  def :+ [A1 : UniformType](elem: A1): UniformListBuffer[Any] = 
    UniformListBuffer.createListBuffer(buffer.:+(normalize(elem)(0)))

  def toUniformList = new UniformList[A] {
    val value = buffer.toList
  }

  private[uniform] def normalize[A](xs: A*): Seq[A] = {
    var result = Vector[A]()
    for (x <- xs) {
      if (x.isInstanceOf[UniformList[_]])
        result :+= x.asInstanceOf[UniformList[_]].value.asInstanceOf[A]
      else if (x.isInstanceOf[UniformMap[_]])
        result :+= x.asInstanceOf[UniformMap[_]].value.asInstanceOf[A]
      else result :+= x 
    }
    result
  }
}

object UniformListBuffer {

  import scala.collection.mutable.ListBuffer

  def apply[A](): UniformListBuffer[A] = createListBuffer(ListBuffer[A]())

  // Helpers

  private[uniform] def createListBuffer[A1](
    xs: ListBuffer[A1]
  ): UniformListBuffer[A1] =
    new UniformListBuffer[A1] { 
      val value = null 
      val buffer = xs.asInstanceOf[ListBuffer[A1]] 
    }
}  
