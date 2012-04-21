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

import java.text.DateFormat

import scalafy.types.meta.OpaqueDataSettings
import scalafy.types.reifiable.ReifiableSettings
import scalafy.util.casing.Casing

/** Utils for general parsing. */
package object parser {

  ///////////////////////////////////////////////////////////////////////////
  // Settings 
  ///////////////////////////////////////////////////////////////////////////

  trait ParserSettings {
    val fieldCasing: Casing
    val prettyPrintSettings: PrettyPrintSettings
    val dateFormatter: DateFormat
    val typeHintSettings: TypeHintSettings
    val classLoader: ClassLoader 
    val opaqueDataSettings: OpaqueDataSettings
    val reifiableSettings: ReifiableSettings
  }

} // end package object
