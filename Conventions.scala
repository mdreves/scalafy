//////////////////////////////////////////////////////////////////////////
// SCALAFY CODING CONVENTIONS
//
// The scala style guide (http://docs.scala-lang.org/style/) is followed
// with some deviations to aid readability/maintainability of code.
//
// Overal Goals:
//   * Limiting the changes required when one part of the code is updated
//     - If a variable name changes we shouldn't have to update much
//       indentation around it to re-indent. For example, we avoid aligning
//       method params, etc.
//     - If a single 'if' statement turns into multiple lines requiring
//       braces be added, we shouldn't have to update all the other if/else
//       blocks.
//   * Limit horizontal space to make diffing easier
//   * Conserve vertical space
//   * Consistency
//     - Class fields, function params, if/else conditionals, and case
//       match/guard statements should all follow the same rules for
//       wrapping and identation
//     - No special cases (e.g. abbreviate IO as 'Io' not 'IO')
//   * User friendly and style checker friendly
//     - A style checker program should be able to follow a logical set of
//       rules without special cases.
//////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
// Naming Conventions
//
//  * http://docs.scala-lang.org/style/naming-conventions.html
//
//  General:
//    * UpperCamelCase: classes, type aliases
//    * lowerCamelCase: fields, methods, annotations
//    * Omit () on zero arg methods if state not modified, use () if side
//      effects
//    * Use A,B,.. for parameter type names
//
//    NOTE: Acyronyms MUST be treated as normal words for casing.
//          (e.g. IoStream, HttpServer  -- NOT IOStream, HTTPServer)
//
//  Special Names:
//    * 'xs' means sequence of some value 'x'
//    * 'kvs' means sequence of key/value pairs
//    * 'kvm' means map of key/value pairs
//
//////////////////////////////////////////////////////////////////////////

// RIGHT
def name = firstName  // accessor


// RIGHT
def increment() = innerVar + 1  // side effect, modifies state


/////////////////////////////////////////////////////////////////////////
// General Indentation and Wrapping
//
//  * http://docs.scala-lang.org/style/indentation.html
//  * http://docs.scala-lang.org/style/declarations.html
//  * http://docs.scala-lang.org/style/method-invocation.html
//  * http://docs.scala-lang.org/style/control-structures.html
//  * http://docs.scala-lang.org/style/types.html
//
//  Spaces:
//    * Default unit of indentation is 2 spaces
//    * 80 chars per line for code
//        NOTE: Try to target 76 chars for outer block comments so can
//          easily move in 2 units of indentation without restructuring
//    * If wrapping class fields / function params indent 4 spaces unless
//      the line includes a closing brace ')' or ']' then indent 2.
//
//  Wrapping:
//    * Everything on one line unless wrapping needed.
//    * First line wrap breaks after opening '('
//    * Related items (type params, function/method params, fields, extends
//      classes, return, etc) must wrap as a group.
//    * If a previous group has spanned multiple lines, then the next group
//      must start on a different line. The line break must be BEFORE the
//      previous groups closing ')' or ']' and indented 2 spaces. Subsequent
//      wrapping within the next group is indented 4.
//    * If a starting ']' or ')' is followed by a '(' put the '](' and ')('
//      on their own lines to provide visual separation between groups. If the
//      group contains a keyword, put it on the line with the ') ('
//      e.g. ')(implicit'
//    * Don't wrap subsequent groups just because previous groups wrapped
//      (e.g. conserve vertical space)
//    * The final closing ')', ') {',  and '=>' are never on their own lines
//      (always with last statement). Exception is if the rare case where the
//      ') {' themselves need to wrap (e.g. hit the dreadful 81 chars...).
//    * When possible, right hand side of an assignment starts on same line
//      as '=' (e.g. ': ReturnType = one-liner-expression', 'val x = foo(...)'
//      'val x = if (...)', etc)
//    * When wrapping function/method invocations apply the 4 space indentation
//      rule to the params and params within params (if function call used to
//      set a param value).
//    * Chained method/function invocations should wrap BEFORE the '.' and
//      should be indented 2 spaces.
//    * If need to wrap an expression, put () around the expression.
//    * If need to wrap an chain of functions calls separated by spaces,
//      put () around them.
//    * When wrapping param comments, indent 2.
//
//  Blank lines:
//    * Two empty lines between each class/function in file
//    * One empty line between each method in a class
//    * Judgement should be used as to whether a blank line should be placed
//      before start of code block (e.g. single method classes might not
//      need a blank line between the declaration and method, but in other
//      cases the visual separation is waranted).
//
//  Braces (Single Expression Rule):
//    * Don't use {} braces around single expressions. No special cases. This
//      differs from Scala style guide which prefers braces for 'if'
//      statements with side effects. There is no way to make that rule
//      consistent since blocks of multiple expressions require braces.
//
//  Comments:
//    * Inline comments must be 2 spaces from text and should NOT be
//      aligned with other inline comments (or will have to change spacing
//      if variable names changed)
//////////////////////////////////////////////////////////////////////////

/** Class Declarations.
  *
  * From general rules:
  *  - Wrapping "groups": 'val/var' fields, 'extends/with' classes/traits
  *  - If line wrap, fields go on their own lines (indented 4) and
  *    ') extends/with' goes on its own line indented 2 with subsequent
  *    wrapping indented 4.
  *  - Params and extends types on same line unless need to wrap.
  *  - Open bracket ({) must go on last line of definition (not its own)
  *
  * Additional rules:
  *  - Declared vals/vars should be grouped together when it makes sense
  *  - Order of keywords: override, protected/private, final, lazy
  */

// RIGHT
class Foo(val f1: Type1) extends Object {  // all fits on one line
  def method1()  // Use judgement, single method so no empty line before needed
}


// RIGHT
class Foo(
    val f1: Type1, val f2: Type2) extends Object {  // all fit (no groups wrap)
  def method1()
}


// RIGHT
class Foo(
    val f1: Type1, val f2: Type2,  // fields on own lines (indent 4)
    val f2: Type3, val f3: Type4
  ) extends Object {  // Previous group wrapped, extends must start on new line

  val f4: Type4  // first definition
  var f5: Type5  // val/var grouped as makes sense

  def method1()
}


// RIGHT
class Foo(
    val f1: Type1, val f2: Type2,  // fields on own lines (indent 4)
    val f2: Type3, val f3: Type4
  ) extends Object with Trait1  // extends/with wrap onto own lines (indent 2)
    with Trait2 with Trait3 {  // indent 4

  val f4: Type4  // first definition
  var f5: Type5  // val/var grouped as makes sense

  def method1()
}


// RIGHT
class Foo[A, B, C](
    val f1: Type1, val f2: Type2
  ) extends Object with Trait1 {  // extends/with wrap onto own lines (indent 2)
  ...
}


// RIGHT
class Foo[
    A, B, C, D, E, F, G, H]() extends Object {  // all groups fit on line
  ...
}


// RIGHT
class Foo[
    A, B, C, D, E, F, G, H  // Wrapping type params on own line (indent 4)
  ](  // starting ']' followed by '(', always on own line
    val f1: Type1) extends Object {  // all remaining "groups" fit on line
  ...
}


// RIGHT
class Foo[
    A, B, C, D, E, F, G, H  // Wrapping type params on own line (indent 4)
  ](  // starting ']' followed by '(', always on own line
    val f1: Type1, val f2: Type2
  ) extends Object with Trait1 {  // extends/with wrap onto own lines (indent 2)
  ...
}


// RIGHT
class Foo[
    A, B, C, D, E, F, G, H  // Wrapping type params on own line (indent 4)
  ](
    val f1: Type1, val f2: Type2,  // fields wrap on own lines (indent 4)
    val f2: Type3
  ) extends Object {  // prev group wrapped must start new line
  ...
}


// WRONG (Scala guide version)
class Foo(
    val f1: Type1,
    var f2: Type2,
    val f3: Type3)
  extends Object
  with Trait1
  with Trait2 {
  def method1()
}


// WRONG
class Foo(
    val f1: Type1, val f2: Type2,
    val f3: Type3) extends Object {  // prev group wrapped, must start new line

  def method1()
}


// WRONG
class Foo[
    A, B, C, D, E, F, G, H]  // must break before closing ']'
  (val f1: Type1, val f2: Type2) extends Object {
  ...
}


// WRONG
class Foo[
    A, B, C, D, E, F, G, H
  ](val f1: Type1) extends Object {  // ']' followed by '(' always on own line
  ...
}


/** Methods/Function Declarations
  *
  * From general rules:
  *  - Wrapping "groups": params (normal, curried, implicit), return,
  *    return + match (form a single group)
  *  - Everything on one line unless need to wrap
  *  - Follow line wrap rules. Params wrap in groups. Wrap BEFORE closing
  *    ')', if ')' followed by '(' put on own line. Don't wrap if don't
  *    have to (even if previously wrapped), etc. When wrapping occurs,
  *    keywords such as 'implicit' go with the opening '(' for the group.
  *  - Open bracket ({) must go on last line of definition (not its own)
  *  - Don't use {..} unless required, just '=' is ok (single expression
  *    rule).
  *
  * Additional rules:
  *  - Always specify return type unless Unit or everything fits on one
  *    line (including params, etc)
  *  - If 'match' used, it must go one same line as return
  *  - Use judgement for when to add spacing at the start of (and within
  *    a funtion (e.g. small function can have no empty lines between
  *    declaration and code block, etc
  *  - Function passed as parameters using { ... } syntax should place their
  *    parameter on the same line as the opening '{'
  */

// RIGHT
def sum(a: Int, b: Int) = a + b  // small fits on one line


// RIGHT
def foo() {  // returns Unit, omit return type and =
  ..side-effects..
}


// RIGHT
def foo(param1: Type1): ReturnType =  // params/return fits on one line
  one-liner-expression  // single expression (no braces needed)


// RIGHT
def foo(  // params/ret don't fit on first line
    param1: Type1, param2: Type2): ReturnType =  // all fit on next line
  one-liner-expression  // single expression (no braces needed)


// RIGHT
def foo(
    param1: Type1, param2: Type2, param3: Type3
  ): ReturnType =  // return "group" needed to wrap

  one-liner-expression  // leave blank line for readibility


// RIGHT
def foo(param1: Type1)(param2: Type2): ReturnType = {  // All fits on one line
  ...
  multi-line expression  // multi-line expression (braces needed)
}


// RIGHT
def foo(
    param1: Type1, param2: Type2  // Param group didn't fit on first line
  )(   // ')' followed by '(' must go on own line
    param3: Type3, param4: Type4): ReturnType = {

  ...  // multiple expessions (braces needed)
}


// RIGHT
def foo(param1: Type1)(  // First param group fits on first line
    implicit param2: Type2, param3: Type3): ReturnType = {  // group/ret fits

  ...
}


// RIGHT
def foo(
    param1: Type1, param2: Type2  // Param group didn't fit on first line
  )(implicit   // implicit goes with '(', for when implicit group itself wraps
    param3: Type3, param4: Type4): ReturnType = {

  ...  // multiple expessions (braces needed)
}


// RIGHT
def foo[A, B, C](
    param1: Type1, param2: Type2): ReturnType = {
  ...
}


// RIGHT
def foo[
    A, B, C, D,
    E, F
  ](   // ']' followed by '(' must go on own line
    param1: Type1, param2: Type2,  // normal parameters in their section
    param3: Type3  // Can't put return here because this group has wrapped
  ): ReturnType = {

  ...
}


// RIGHT
def foo[A,B](
    param1: Type1, param2: Type2
  ): ReturnType = param1 match {  // match goes with return

  case ...
}


// WRONG
def foo(
    param1: Type1,  // put multiple params on one line
    param2: Type2,
    param3: Type3
  ): ReturnType = {

  ...
}


// WRONG
def foo(param1: Type1, param2: Type2  // wrapping, must go on next line
    param3: Type3
  ): ReturnType = one-line-expression


// WRONG
def foo(
    param1: Type1, param2: Type2,
    param3: Type3): ReturnType =  // prev group wrapped, return needs own line
  one-liner-expression  // onliner must go with return


// WRONG
def foo(param1: Type1): ReturnType = {
  one-liner-expression  // single expression, don't use '{', '}'
}


/** Function Values
  *
  * From general rules:
  *   - When {} used, put function params on same line as '{'
  */

// ACCEPTABLE (for small function values)
val f = (Int, Int) => Int { _ + _ }


// PREFERRED
val f = { (a: Int, b: Int) =>  // params on one line with {
  ...multi-line-expresion...
}


/** Expressions
  *
  * From general rules:
  *  - If need to wrap, put () around expression
  */

// RIGHT
val foo =  1 + 2 + 3  // fits on one line


// RIGHT
val foo = (1 + 2 + 3 +  // end with operator
    4 + 5)


// WRONG (Scala guide version)
val foo = 1 + 2 + 3 + 4 +
  5 + 6 + 7


/** Method/Function Invocation
  *
  * From general rules:
  *   - Wrapping "groups": function/method calls
  *   - If need to wrap params, indent 4. This is for visual separation
  *     between other chained function calls when they need to be wrapped.
  *   - If embedded function call has wrapping params, put it on its own
  *     line and indent its params another 4 spaces on subsequent lines.
  *     Don't put any params on the same line after a wrapping fn call.
  *   - Each group should start with a function call including the '.' to
  *     indicate that it is applied to a value returned from previous line.
  *
  * Additional rules:
  *   - Use {} when a function or block of code is being passed as a param
  *     otherwise use (). If {} used then closing '}' goes on its own line,
  *     if () used, then closing ')' goes on same line as last param.
  *   - Avoid parens in functional calls whenever possible. If spaces may be
  *     used, use them. If necessary, wrap in entire chain of calls in a set
  *     of parens '(...)' so that the inner expression that makes use of
  *     spaces can span multiple lines.
  *   - If a chain of calls needs to wrap, then logically group each
  *     sub-chain in the sequence onto its own line. If a logical grouping
  *     itself needs wrapping then wrap and indent the group wrap another 2.
  *   - When wrapping is used in chained calls separated by spaces (e.g.
  *     not '.'), then put a set of parens '(...)' around the entire chain.
  *   - Don't mix chain wrapping with parameter wrapping. If a chained fn
  *     call needs to wrap put the next fun in chain on its own line.
  *   - If the result is being assigned to a variable, then the first
  *     function call must go after the '='. This may look odd (especially
  *     for 'if'), but it makes it stick out that an assignment is made.
  *   - If {} used in a chain, it is ok to place next function on same
  *     line as closing '}', but it's body must be one a separate line.
  */

// RIGHT
val x = foo(param1, param2)  // everything fits on one line


// RIGHT
val x = foo(
    param1, param2, param3)  // wrap and indent 4


// RIGHT
val x = foo(
    param1, param2, param3,
    param4)


// RIGHT
val x = foo(
    param1, param2, bar(param3),
    param4)


// RIGHT
val x = foo(
    param1, param2,
    bar(  // fn call where the function params wrap, put fn on own line
        param3, param4, // indent additional 4
        param5))


// RIGHT
val x = foo(
    param1, param2,
    bar(
        param3, param4,
        param5),
    param6)  // param6 on next line, not after param5 on wrapping fn call


// WRONG
val x = foo(param1, param2,  // start params on next line
    param3)


// WRONG
val x =
  foo(param1, param2, param3)  // foo must go with '='


// WRONG (Scala guide version)
foo(
  param1,
  param2,
  param3)


//** Chained calls **//

// RIGHT
foo(param1).foo2(param2)  // ok fits on one line


// RIGHT
foo(param1, param2)
  .foo2(param3)  // start with . and indent 2


// RIGHT
foo(
    param1, param2, param3)
  .foo2(param3)


// RIGHT
foo(param1)
  .foo2(param2)  // first chained call
  .foo3(param3)  // second chained call


// OK
foo(param1).foo2(param2)  // assumes foo2 more logically with foo, but not foo3
  .foo3(param3)


// RIGHT
myList map { _.toUpperCase } filter { _.length > 5 } // fits on one line


// RIGHT
(myList  // won't fit on one line, separate lines, surround all by ( )
  map { _.toUpperCase }
  filter { _.length > 5 }
  toStream)


// RIGHT
(myList
  map {
    ...
  } filter {
    ...
  } toStream)


// WRONG
foo(  // don't mix chained/param wrapping
    param1, param2, param3).foo2(param3);


// WRONG
foo(param1, param2).foo2(
    param2)  // chained call, must start line with .fn


// WRONG
(obj.toList
  map {
    .. really long expression ...
  } filter { _.length > 5 })  // put body of filter on own line


/** Infix, Postfix, Functional vs Imperative
  *
  * Additional Rules:
  * - Omit () on method calls whenever the method definition omits it
  *   (e.g. purely functional calls)
  * - Suffix notation (myArray toList vs myArray.toList) should be avoided.
  *   It is only allowed at the end of a chain functional calls using infix
  * - Infix notation used with arity-1 functions (myArray mkString ",")
  *   should be used with standard functional methods (mkString, foreach,
  *   map, filter, getOrElse etc). It should only be used with methods that
  *   are purely functional or take functions as arguments.
  * - If a parameter is a function and its the last arg, use {} instead of ()
  *   (e.g. map {...}, filter {...}, foreach {...}, etc)
  */

// WRONG
println                        // has side-effect, use ()

// RIGHT
println()


// WRONG
queue.size()                   // functional, no side effects

// RIGHT
queue.size                     // omit ()


// WRONG
myArray toList                 // avoid suffix notation

// RIGHT
myArray.toList

// RIGHT (ok to use suffix notation, toStream called after infix used)
myArray map { _.toUpperCase } toStream


// WRONG
println "foo"                  // has side effects, don't use infix notation

// RIGHT
println("foo")


// RIGHT
myArray foreach { ... }        // foreach takes function, use infix and {}
myArray mkString ","           // mkString has no side effects, use infix
myMap getOrElse  "foo"         // getOrElse has no side effects, use infix
myArray map {..} filter {..}   // map/filter functions, use infix and {}


/** Control structures (if/else)
  *
  * From general rules:
  *  - Omit braces if if/else fits on one line, else always use {} (single
  *    expression rule)
  *  - If need to wrap conditionals indent additional 4 and put
  *    ') {' on line with last conditional.
  *  - Use own judgement as to whether a blank line before the code
  *    block starts would add readibility.
  */

// RIGHT
val x = if (someCondition) a else b  // fits on one line


// RIGHT
val x = if (someReallyLongCondition) {
    a  // indention of additional 2 because of assignment
  } else {
    b
  }


// RIGHT
val x = if (  // wrapped conditionals on their own lines
      someCondition1 && someCondition2 &&  // conditional expr indent +4
      someCondition3) {
    ... do something ...
  }


// RIGHT
val x = if (someCondition) {  // put comments in block, not outside
    // This is specific to the if-part
    ... do something ...
  } else {
    // This is specific to the else-part
    ... do something else ...
  }


// RIGHT
if (someCondition)
  single-expression
else {
  multi-expression
  ...
}


// WRONG (Scala guide version)
val x = if (someReallyLongCondition)
  a
else
  b


// WRONG
val x =  // put if here, not below
  // This is specific to the if-part
  if (someCondition) {
    ... do something ...  // put comment here not above...
  }
  // This is specific to the else-part
  else {
    ... do something else ...  // put comment here not above...
  }


// WRONG
if (someCondition) {
  single-expression  // Single expression used, no '{', '}' needed
} else{
  multi-expression
  ...
}


/** Control structures (match)
  *
  * Additional rules:
  *  - Indent code blocks associated with case 2 spaces. Don't use {}
  *    around block even if multi-line or wrapping used.
  *  - If case statement all fits on one line then put all on one line
  *  - If need to wrap, then wrap the block after the =>
  *  - If need to wrap before the => then follow the same wrapping rules
  *    as for functions and classes (e.g. break at (, indent values
  *    4 spaces and end with ) => on last wrapped line
  *  - Always put the 'if (' portion of a pattern guard on the same line
  *    as a match statement. It becomes difficult to be able to easily
  *    distinguish pattern gaurd if statements from a blocks if statements
  *  - Treat each case block independently. Just because one wraps doesn't
  *    mean all the others need to wrap (otherwise have to update all lines
  *    when one changes)
  *  - Use own judgement as to whether a blank line before the code
  *    block starts would add readibility.
  */

// RIGHT
exp match {
  case "test" => one-line-expression
  case "test2" =>
    one-line-expression  // doesn't fit above but fits below
  case "test3" =>
    multi-line-expression  // multiple lines ok
    multi-line-expression
  case "test4" => one-line-expression
}


// RIGHT
exp match {
  case RealyLongPatternMatchingExpr if (
      somePatternGuard) =>
    expression
}


// WRONG
exp match {
  case RealyLongPatternMatchingExpr
    if (somePatternGuard) =>  // bad - dont' wrap pattern guard
      one-line-expression
}


/** for comprehensions
  *
  * Additional rules:
  *  - If yield expression and multiple generators, use {}, else use ()
  */

// RIGHT
for (x <- list) yield (x + 1)  // only one generator, use ()


// RIGHT
for {
  x <- list1
  y <- list2
} yeild (x, y)


// RIGHT
for (x <- list1; y <- list2) {             // yield not used, use ()
  println(x + y)
}


// WRONG
for (x <- list1; y <- list2) yield (x, y)  // multi and yield used, use {}


// WRONG
for {
  x <- list1
  y <- list2                               // yeild not used, use ()
} {
  println(x + y)
}


/** Type definitions
  *
  * Additional rules
  * - Use inference and target typing (only annotate types if needed)
  * - Omit braces for single param functions
  * - Structural types on one line if fits else define alias (avoid whenever
  *   possible)
  */

// RIGHT
val f: Int => String  // only one param to fn


// RIGHT
val f: (Int, Int) => String  // more than one param to fn


// RIGHT
def foo(a: { def bar(a: Int, b: Int): String }): Int  // fits on one line


// WRONG (too long)
def foo(a: { def bar(a: Int, b: Int): String; def baz(): String }): Int


// RIGHT
private type FooParam = {
  def bar(a: Int, b: Int): String
  def baz(): String
}
def foo(a: FooParam): Int


// WRONG
val f: (Int) => String          // only one param to fn, omit ()


/////////////////////////////////////////////////////////////////////////////
// Scaladoc
//
// * http://docs.scala-lang.org/style/scaladoc.html
//
// Guidelines:
//   * Get to point and keep it short (e.g. 'Returns xxx' not 'This method
//     returns xxx'). Same for classes (e.g. 'Does xxx' not 'This class...')
//   * Scala library links using [[scala.List]], etc
//   * Don't document params/return if obvious. If the description is short
//     and includes information about what is returned, don't add @return
//   * Say 'this class' and 'this object' not 'the class' and 'the object'
//   * If implicits used, show examples of usage
/////////////////////////////////////////////////////////////////////////////

// package1/package2/package.scala

package package1
/** Put a short summary here for package
  *
  * Add more detailed comments here. You can use these wiki format options:
  *
  * [[package1.package2.SomeClass]] - linkes to a class
  *
  * ==Overview==                    - heading
  *
  * {{{                             - example code
  * scala> val s = "test"
  * s: java.lang.String = test
  * }}}
  */


// package1/package2/Foo.scala
package package2

/** Example class.
 *
 * @constructor comment on construction
 * @param param1 constructor param1 comment
 * @param param2 constructor param2 comment that wraps to the next
 *   line (use 2 space indent, don't want to re-indent if rename param)
 */
class Foo (param1: Type1, param2: Type2) {
  /** Comment on method.
    *
    * {{{
    *  example...
    * }}}
    *
    * @param param1 param1 comment
    * @return bar
    */
  def bar(param1: Type1): ReturnType = {
    ...
  }
}
