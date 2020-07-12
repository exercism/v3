## Defining Variables

Defining a variable means assigning a value to a name. In Scala there are two kinds of variables, vals and vars.

A `val` is [immutable][immutability], so it's value cannot be changed anymore after the first assignment:

```scala
val numberTen = 10

numberTen = 11
// --> raises a compile error "reassignment to val"
```

A `var`, in contrast, can be reassigned to a new value of same type:

```scala
var someNumber = 10

someNumber = 11
// now someNumber has been reassigned
```

Scala is a statically-typed language, which means that everything has a type at compile-time. A variable can be defined either by explicitly specifying its type, or by letting the Scala compiler infer its type based on the assigned value (known as [type inference][type-inference]). Therefore, the following two variable definitions are equivalent:

```scala
val explicitNumber: Int = 10; // Explicitly typed
val implicitNumber = 10; // Implicitly typed
```

Once defined, a variable's type can never change.

```scala
var count = 1; // Assigning an initial value of type Int
count = 2;     // Updating to new value is possible, because it is defined as var

count = false;
// --> raises a compile error "type mismatch"
```

## Defining Functions within Classes

Scala is an _object-oriented language_ and requires all functions to be defined in a _class_. The `class` keyword is used to define a class.

```scala
class Calculator {
  // ...
}
```

Inside a class, functions can be defined starting with keyword `def` followed by a name. Each method can have zero or more parameters, which are listed comma-separated within round brackets. All parameters must be explicitly typed, there is no type inference for parameters. After the list of parameters, the function's return type is defined separated by another colon.

The method definition is followed by an `=` sign to start with the method body. In the following example the method body consists of two statements, which are surrounded by curly braces.

The function will return the value of the last expression, in this case the `add` function returns the value of variable `sum`:

```scala
class Calculator {
  // long form
  def add(x: Int, y: Int): Int = {
    val sum = x + y
    sum
  }
}
```

There are a few improvements, which can be done to shorten that function in order to improve readability:

- since the variable `sum` is accessed only once to return the value, it can be omitted
- methods consisting of only one statement do not need curly braces
- the function's return type can be inferred the same as for variable types

This leads to the following one-line function with same functionality:

```scala
class Calculator {
  // shortened form
  def add(x: Int, y: Int) = x + y
}
```

## Instantiating classes and calling methods

Invoking a method is done by first creating an instance of a class and second to call the method on that instance with arguments:

```scala
val calculator = new Calculator()
val sum = calculator.add(5, 7)
// sum is of type Int with assigned value of 12
```

## Commenting

Scala supports two types of comments, single line comments and multiline comments:

```scala
// this is a single line comment

/*
This comment spans across
multiple lines.
*/
```

[immutability]: ../../../../../../reference/concepts/immutability.md
[type-inference]: ../../../../../../reference/concepts/type_inference.md
