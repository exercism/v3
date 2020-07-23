## Key Takeaways from this Exercise

**Variables**

- Immutable variables (also called values) can be defined with `val`.
- Mutable variables can be defined with `var`.
- The variable's type can often be detected automatically by the Scala compiler (this is known as [type interference][scala-type-inference]), so that it can be omitted.

```scala
val numberTen = 10 // vals can not be changed anymore
var number = 1 // vars can be changed
number += 1 // number is now 2
var index : Int = 0 // variable with explicit type declaration
```

**Comments**

Scala has single line comments and block comments.

```scala
// this is a single line comment

/*
block comments can span
across multiple lines
*/
```

**Classes**

- Classes are defined with keyword `class` and are instantiated with `new`.

```scala
class Calculator {
  // here comes the class implementation
}

val myCalculator = new Calculator()
```

**Methods**

- Methods are defined within classes.
- Methods can have multiple or no parameters.
- The return type can often be inferred by the Scala compiler like for variables.
- The last expression within a method is being returned (no `return` keyword required).
- Methods in Scala can be very short and curly braces are then omitted, if the implementation fits into one line. Of course there can also be multiline methods (consisting of multiple expressions).


```scala
class Calculator {
  def add(x: Int, y: Int) = x + y

  def sumOfDigits(x: Int) = {
    val digits = x.toString.map(_.asDigit)
    digits.sum
  }
}

//invoking a method:
val calculator = new Calculator()
println(calculator.sumOfDigits(123))
// => prints 6
```

## Additional information

**Naming conventions**

Scala has following [naming conventions]:

- Class names are written in upper camel case, e.g. `Calculator` or `LasagnaTest`
- Variable names (mutable and immutable) and method names are in lower camel case, e.g. `def toString()`, `val digits`, `var number`
- Immutable variables used as a constant value are in upper camel case.

```scala
class FourWheelVehicle {
  val NumberOfWheels = 4
}
```

**Visibility**

- Variables and methods defined in classes are per default **public** and can be accessed from outside the class.
- Add access modifier `private` to hide variables or methods to the outside of the class and to restrict their access to class-internal access.

```scala
class MyClass {
  val publicValue = 42
  private val internalValue = publicValue * 2

  def publiclyCallableMethod() = ...

  private def internallyCallableMethod() = ...
}
```

- More about [access modifiers on tutorialspoint.com].

## Further links

- [Tour of Scala - Introduction]
- [Tour of Scala - Basics]

[scala-type-inference]: https://docs.scala-lang.org/tour/type-inference.html
[tour of scala - introduction]: https://docs.scala-lang.org/tour/tour-of-scala.html
[tour of scala - basics]: https://docs.scala-lang.org/tour/basics.html
[naming conventions]: https://docs.scala-lang.org/style/naming-conventions.html
[access modifiers on tutorialspoint.com]: https://www.tutorialspoint.com/scala/scala_access_modifiers.htm
