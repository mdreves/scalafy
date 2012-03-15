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
package test.scalafy.casing

import org.specs2.mutable.Specification

import scalafy.casing._

/** Test specification for casing package */
object CasingSpec extends Specification {

  "The casing package" should {
    "support implicit casing conversions for strings" in {
      "my_string".toLowerCamelCase.mustEqual("myString")
      "my_string".toUpperCamelCase.mustEqual("MyString")
      "my_string".toLowerDashCase.mustEqual("my-string")
      "my_string".toUpperDashCase.mustEqual("My-String")
      "My-String".toLowerSnakeCase.mustEqual("my_string")
      "my_string".toUpperSnakeCase.mustEqual("MY_STRING")
    }

    "support implicit casing conversions for seqs of strings" in {
      List("str_foo", "str_bar").toLowerCamelCase
        .mustEqual(List("strFoo", "strBar"))
    }

    "support implicit casing conversions for maps of strings" in {
      Map("str_foo" -> 1, "str_bar" -> 2).toLowerCamelCase
        .mustEqual(Map("strFoo" -> 1, "strBar" -> 2))
    }

    "support extactors for matching on case" in {
      ("value" match { case LowerCase(x) => x; case _ => "" })
        .mustEqual("value")
      ("VALUE" match { case UpperCase(x) => x; case _ => "" })
        .mustEqual("VALUE")
      ("someValue" match { case LowerCamelCase(x) => x; case _ => ""})
        .mustEqual("someValue")
      ("SomeValue" match { case UpperCamelCase(x) => x; case _ => ""})
        .mustEqual("SomeValue")
      ("some_value" match { case LowerSnakeCase(x) => x; case _ => "" })
        .mustEqual("some_value")
      ("SOME_VALUE" match { case UpperSnakeCase(x) => x; case _ => "" })
        .mustEqual("SOME_VALUE")
      ("some-value" match { case LowerDashCase(x) => x; case _ => "" })
        .mustEqual("some-value")
      ("Some-Value" match { case UpperDashCase(x) => x; case _ => "" })
        .mustEqual("Some-Value")
      ("FooBar" match { case LowerCamelCase(x) => x; case _ => "" })
        .mustNotEqual("someValue")
    }
  }

  "The Casing.toLowerCamelCase function" should {
    "not change lowerCamelCase strings" in {
      Casing.toLowerCamelCase("myString").mustEqual("myString")
      Casing.toLowerCamelCase("myLongerString").mustEqual("myLongerString")
    }

    "convert UpperCamelCase to lowerCamelCase" in {
      Casing.toLowerCamelCase("MyString").mustEqual("myString")
      Casing.toLowerCamelCase("MyLongerString").mustEqual("myLongerString")
    }

    "convert lower_snake_case to lowerCamelCase" in {
      Casing.toLowerCamelCase("my_string").mustEqual("myString")
      Casing.toLowerCamelCase("my_longer_string").mustEqual("myLongerString")
    }

    "convert UPPER_UNDERSCORE to lowerCamelCase" in {
      Casing.toLowerCamelCase("MY_STRING").mustEqual("myString")
      Casing.toLowerCamelCase("MY_LONGER_STRING").mustEqual("myLongerString")
    }

    "convert Upper-Dash-Case to lowerCamelCase" in {
      Casing.toLowerCamelCase("My-String").mustEqual("myString")
      Casing.toLowerCamelCase("My-Longer-String").mustEqual("myLongerString")
    }

    "convert UPPER to lowerCamelCase" in {
      Casing.toLowerCamelCase("UPPER").mustEqual("upper")
    }

    "convert lower to lowerCamelCase" in {
      Casing.toLowerCamelCase("lower").mustEqual("lower")
    }

    "work with special chars (. , =)" in {
      Casing.toLowerCamelCase("myString=Test").mustEqual("myString=test")
      Casing.toLowerCamelCase("myString.Test=Test2")
         .mustEqual("myString.test=test2")
      Casing.toLowerCamelCase("Test1,test2,Test3")
         .mustEqual("test1,test2,test3")
    }

    "support being escapped with \" or '" in {
      Casing.toLowerCamelCase("\"myString=Test\"")
        .mustEqual("\"myString=Test\"")
      Casing.toLowerCamelCase("\"lower_snake_case\"")
        .mustEqual("\"lower_snake_case\"")
      Casing.toLowerCamelCase("\"Upper-Dash-Case\"")
        .mustEqual("\"Upper-Dash-Case\"")
      Casing.toLowerCamelCase("'UpperCamelCase'")
        .mustEqual("'UpperCamelCase'")
    }
  }

  "The Casing.toUpperCamelCase function" should {
    "not change UpperCamelCase strings" in {
      Casing.toUpperCamelCase("MyString").mustEqual("MyString")
      Casing.toUpperCamelCase("MyLongerString").mustEqual("MyLongerString")
    }

    "convert lowerCamelCase to UpperCamelCase" in {
      Casing.toUpperCamelCase("myString").mustEqual("MyString")
      Casing.toUpperCamelCase("myLongerString").mustEqual("MyLongerString")
    }

    "convert lower_snake_case to UpperCamelCase" in {
      Casing.toUpperCamelCase("my_string").mustEqual("MyString")
      Casing.toUpperCamelCase("my_longer_string").mustEqual("MyLongerString")
    }

    "convert UPPER_UNDERSCORE to UpperCamelCase" in {
      Casing.toUpperCamelCase("MY_STRING").mustEqual("MyString")
      Casing.toUpperCamelCase("MY_LONGER_STRING").mustEqual("MyLongerString")
    }

    "convert Upper-Dash-Case to UpperCamelCase" in {
      Casing.toUpperCamelCase("My-String").mustEqual("MyString")
      Casing.toUpperCamelCase("My-Longer-String").mustEqual("MyLongerString")
    }

    "convert UPPER to UpperCamelCase" in {
      Casing.toUpperCamelCase("UPPER").mustEqual("Upper")
    }

    "convert lower to UpperCamelCase" in {
      Casing.toUpperCamelCase("lower").mustEqual("Lower")
    }

    "work with special chars (. , =)" in {
      Casing.toUpperCamelCase("myString=Test").mustEqual("MyString=Test")
      Casing.toUpperCamelCase("myString.Test=Test2")
        .mustEqual("MyString.Test=Test2")
      Casing.toUpperCamelCase("test1,Test2,test3")
        .mustEqual("Test1,Test2,Test3")
    }
  }

  "The Casing.toLowerSnakeCase function" should {
    "not change lower_snake_case strings" in {
      Casing.toLowerSnakeCase("my_string").mustEqual("my_string")
      Casing.toLowerSnakeCase("my_longer_string")
        .mustEqual("my_longer_string")
    }

    "convert lowerCamelCase to lower_snake_case" in {
      Casing.toLowerSnakeCase("myString").mustEqual("my_string")
      Casing.toLowerSnakeCase("myLongerString").mustEqual("my_longer_string")
    }

    "convert UpperCamelCase to lower_snake_case" in {
      Casing.toLowerSnakeCase("MyString").mustEqual("my_string")
      Casing.toLowerSnakeCase("MyLonger-String")
        .mustEqual("my_longer_string")
    }

    "convert Upper-Dash-Case to lower_snake_case" in {
      Casing.toLowerSnakeCase("My-String").mustEqual("my_string")
      Casing.toLowerSnakeCase("My-Longer-String")
        .mustEqual("my_longer_string")
    }

    "convert UPPER_UNDERSCORE to lower_snake_case" in {
      Casing.toLowerSnakeCase("MY_STRING").mustEqual("my_string")
      Casing.toLowerSnakeCase("MY_LONGER_STRING")
        .mustEqual("my_longer_string")
    }

    "convert UPPER to lower_snake_case" in {
      Casing.toLowerSnakeCase("UPPER").mustEqual("upper")
    }

    "convert lower to lower_snake_case" in {
      Casing.toLowerSnakeCase("lower").mustEqual("lower")
    }

    "separate numbers with snake_case" in {
      Casing.toLowerSnakeCase("MyString2").mustEqual("my_string_2")
      Casing.toLowerSnakeCase("My13String2").mustEqual("my_13_string_2")
    }

    "work with special chars (. , =)" in {
      Casing.toLowerSnakeCase("myString=Test").mustEqual("my_string=test")
      Casing.toLowerSnakeCase("myString.Test=Test2")
        .mustEqual("my_string.test=test_2")
      Casing.toLowerSnakeCase("test1Test,Test2,test3")
        .mustEqual("test_1_test,test_2,test_3")
    }
  }

  "The Casing.toUpperSnakeCase function" should {
    "not change UPPER_UNDERSCORE strings" in {
      Casing.toUpperSnakeCase("MY_STRING").mustEqual("MY_STRING")
      Casing.toUpperSnakeCase("MY_LONGER_STRING")
        .mustEqual("MY_LONGER_STRING")
    }

    "convert lowerCamelCase to UPPER_UNDERSCORE" in {
      Casing.toUpperSnakeCase("myString").mustEqual("MY_STRING")
      Casing.toUpperSnakeCase("myLongerString")
        .mustEqual("MY_LONGER_STRING")
    }

    "convert UpperCamelCase to UPPER_UNDERSCORE" in {
      Casing.toUpperSnakeCase("MyString").mustEqual("MY_STRING")
      Casing.toUpperSnakeCase("MyLongerString")
        .mustEqual("MY_LONGER_STRING")
    }

    "convert Upper-Dash-Case to UPPER_UNDERSCORE" in {
      Casing.toUpperSnakeCase("My-String").mustEqual("MY_STRING")
      Casing.toUpperSnakeCase("My-Longer-String")
        .mustEqual("MY_LONGER_STRING")
    }

    "convert lower_snake_case to UPPER_UNDERSCORE" in {
      Casing.toUpperSnakeCase("my_string").mustEqual("MY_STRING")
      Casing.toUpperSnakeCase("my_longer_string")
        .mustEqual("MY_LONGER_STRING")
    }

    "convert UPPER to UPPER_UNDERSCORE" in {
      Casing.toUpperSnakeCase("UPPER").mustEqual("UPPER")
    }

    "convert lower to UPPER_UNDERSCORE" in {
      Casing.toUpperSnakeCase("lower").mustEqual("LOWER")
    }

    "separate numbers with snake_case" in {
      Casing.toUpperSnakeCase("MyString2").mustEqual("MY_STRING_2")
      Casing.toUpperSnakeCase("My13String2").mustEqual("MY_13_STRING_2")
    }

    "work with special chars (. , =)" in {
      Casing.toUpperSnakeCase("myString=Test").mustEqual("MY_STRING=TEST")
      Casing.toUpperSnakeCase("myString.Test=Test2")
        .mustEqual("MY_STRING.TEST=TEST_2")
      Casing.toUpperSnakeCase("test1Test,Test2,test3")
        .mustEqual("TEST_1_TEST,TEST_2,TEST_3")
    }
  }

  "The Casing.toLowerDashCase function" should {
    "not change lower-dash-case strings" in {
      Casing.toLowerDashCase("my-string").mustEqual("my-string")
      Casing.toLowerDashCase("my-longer-string")
        .mustEqual("my-longer-string")
    }

    "convert Upper-Dash-Case to lower-dash-case" in {
      Casing.toLowerDashCase("My-String").mustEqual("my-string")
      Casing.toLowerDashCase("My-Longer-String")
        .mustEqual("my-longer-string")
    }

    "convert lowerCamelCase to lower-dash-case" in {
      Casing.toLowerDashCase("myString").mustEqual("my-string")
      Casing.toLowerDashCase("myLongerString").mustEqual("my-longer-string")
    }

    "convert UpperCamelCase to lower-dash-case" in {
      Casing.toLowerDashCase("MyString").mustEqual("my-string")
      Casing.toLowerDashCase("MyLonger-String").mustEqual("my-longer-string")
    }

    "convert snake_case_separated to lower-dash-case" in {
      Casing.toLowerDashCase("my_string").mustEqual("my-string")
      Casing.toLowerDashCase("my_longer_string")
        .mustEqual("my-longer-string")
    }

    "convert UPPER to lower-dash-case" in {
      Casing.toLowerDashCase("UPPER").mustEqual("upper")
    }

    "convert lower to lower-dash-case" in {
      Casing.toLowerDashCase("lower").mustEqual("lower")
    }

    "separate numbers with snake_case" in {
      Casing.toLowerDashCase("MyString2").mustEqual("my-string-2")
      Casing.toLowerDashCase("My13String2").mustEqual("my-13-string-2")
    }

    "work with special chars (. , =)" in {
      Casing.toLowerDashCase("myString=test").mustEqual("my-string=test")
      Casing.toLowerDashCase("myString.test=Test2")
        .mustEqual("my-string.test=test-2")
      Casing.toLowerDashCase("test1Test,Test2,test3")
        .mustEqual("test-1-test,test-2,test-3")
    }
  }

  "The Casing.toUpperDashCase function" should {
    "not change Upper-Dash-Case strings" in {
      Casing.toUpperDashCase("My-String").mustEqual("My-String")
      Casing.toUpperDashCase("My-Longer-String")
        .mustEqual("My-Longer-String")
    }

    "convert lower-dash-case to Upper-Dash-Case" in {
      Casing.toUpperDashCase("my-string").mustEqual("My-String")
      Casing.toUpperDashCase("my-longer-string").mustEqual("My-Longer-String")
    }

    "convert lowerCamelCase to Upper-Dash-Case" in {
      Casing.toUpperDashCase("myString").mustEqual("My-String")
      Casing.toUpperDashCase("myLongerString").mustEqual("My-Longer-String")
    }

    "convert UpperCamelCase to Upper-Dash-Case" in {
      Casing.toUpperDashCase("MyString").mustEqual("My-String")
      Casing.toUpperDashCase("MyLonger-String").mustEqual("My-Longer-String")
    }

    "convert snake_case_separated to Upper-Dash-Case" in {
      Casing.toUpperDashCase("my_string").mustEqual("My-String")
      Casing.toUpperDashCase("my_longer_string")
        .mustEqual("My-Longer-String")
    }

    "convert UPPER to Upper-Dash-Case" in {
      Casing.toUpperDashCase("UPPER").mustEqual("Upper")
    }

    "convert lower to Upper-Dash-Case" in {
      Casing.toUpperDashCase("lower").mustEqual("Lower")
    }

    "separate numbers with snake_case" in {
      Casing.toUpperDashCase("MyString2").mustEqual("My-String-2")
      Casing.toUpperDashCase("My13String2").mustEqual("My-13-String-2")
    }

    "work with special chars (. , =)" in {
      Casing.toUpperDashCase("myString=Test").mustEqual("My-String=Test")
      Casing.toUpperDashCase("myString.Test=Test2")
        .mustEqual("My-String.Test=Test-2")
      Casing.toUpperDashCase("test1Test,Test2,test3")
        .mustEqual("Test-1-Test,Test-2,Test-3")
    }
  }

  "The Casing.toCase function" should {
    "support all the Casing enumerations" in {
      Casing.toCase("My-String", LowerCamelCase, UnknownCasing)
        .mustEqual("myString")
      Casing.toCase("My-String", UpperCamelCase).mustEqual("MyString")
      Casing.toCase("myString", LowerDashCase).mustEqual("my-string")
      Casing.toCase("myString", UpperDashCase).mustEqual("My-String")
      Casing.toCase("myString", LowerSnakeCase).mustEqual("my_string")
      Casing.toCase("myString", UpperSnakeCase).mustEqual("MY_STRING")
      Casing.toCase("test", UpperCase).mustEqual("TEST")
      Casing.toCase("Test", LowerCase).mustEqual("test")
    }

    "support the ignore list" in {
      Casing.toCase(
          "My-String", LowerCamelCase, UnknownCasing, "My-String" :: Nil)
        .mustEqual("My-String")
    }
  }

  "The Casing.toCaseOnMap function" should {
    "support all the Casing enumerations" in {
      Casing.toCaseOnMap(
          Map("My-String" -> "My-Value", "MyString2" -> "MyValue2"),
          LowerCamelCase, null )
        .mustEqual(Map("myString" -> "My-Value", "myString2" -> "MyValue2"))

      Casing.toCaseOnMap(
          Map("My-String" -> "My-Value", "myString2" -> "myValue2"),
          UpperCamelCase )
        .mustEqual(Map("MyString" -> "My-Value", "MyString2" -> "myValue2"))

      Casing.toCaseOnMap(
          Map("My-String" -> "My-Value", "myString2" -> "myValue2"),
          LowerSnakeCase )
        .mustEqual(
          Map("my_string" -> "My-Value", "my_string_2" -> "myValue2") )

      Casing.toCaseOnMap(
          Map("My-String" -> "My-Value", "myString2" -> "myValue2"),
          UpperSnakeCase )
        .mustEqual(
          Map("MY_STRING" -> "My-Value", "MY_STRING_2" -> "myValue2") )

      Casing.toCaseOnMap(
          Map("Test" -> "My-Value", "test2" -> "myValue2"), UpperCase )
        .mustEqual(Map("TEST" -> "My-Value", "TEST2" -> "myValue2"))

      Casing.toCaseOnMap(
          Map("Test" -> "My-Value", "TEST2" -> "myValue2"), LowerCase )
        .mustEqual(Map("test" -> "My-Value", "test2" -> "myValue2"))
    }

    "support the ignore list" in {
      Casing.toCaseOnMap(
          Map("My-String" -> "My-Value", "MyString2" -> "MyValue2"),
          LowerCamelCase, null, "MyString2" :: Nil )
        .mustEqual(Map("myString" -> "My-Value", "MyString2" -> "MyValue2"))
    }

    "support converting values" in {
      Casing.toCaseOnMap(
          Map("My-String" -> "My-Value", "MyString2" -> "MyValue2"),
          LowerCamelCase, null, Nil, true )
        .mustEqual(Map("myString" -> "myValue", "myString2" -> "myValue2"))
    }
  }

  "The Casing.toCaseOnSeq function" should {
    "support all the Casing enumerations" in {
      Casing.toCaseOnSeq(
          List("MyString", "MyString2"), LowerCamelCase, null)
        .mustEqual(List("myString", "myString2"))

      Casing.toCaseOnSeq(List("My-String", "myString2"),UpperCamelCase)
        .mustEqual(List("MyString", "MyString2"))

      Casing.toCaseOnSeq(List("My-String", "myString2"), LowerSnakeCase)
        .mustEqual(List("my_string", "my_string_2"))

      Casing.toCaseOnSeq(List("My-String", "myString2"), UpperSnakeCase)
        .mustEqual(List("MY_STRING", "MY_STRING_2"))

      Casing.toCaseOnSeq(List("Test", "test2"), UpperCase)
        .mustEqual(List("TEST", "TEST2"))

      Casing.toCaseOnSeq(List("Test", "TEST2"), LowerCase)
        .mustEqual(List("test", "test2"))
    }

    "support the ignore list" in {
      Casing.toCaseOnSeq(
          List("My-String", "MyString2"),
          LowerCamelCase, null, "MyString2" :: Nil )
        .mustEqual(List("myString", "MyString2"))
    }

    "support converting values" in {
      Casing.toCaseOnSeq(
          List("My-String", "MyString2"), LowerCamelCase, null, Nil)
        .mustEqual(List("myString", "myString2"))
    }
  }
}
