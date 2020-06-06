Swift is a statically-typed language, which means that every value has a known, fixed type at compile time.

Values can be associated with names by defining a variable or a constant and assiging a value to the constant or variable. That name may then be used to refer to that value throughout the program. As the name implies, a constant is _immutable_ which means that the value cannot be changed. A variable, on the other hand, is _mutable_, which means that the value can be changed at any time.

Variables are defined using the `var` keyword, while constants are defined using the `let` keyword.

```swift
var variableName = 10 // varible
let constantName = 10 // constant
```

The type associated with a variable can be either explicitly specified, or it can be inferred by the compiler based on the assigned value. Therefore, the following two variable definitions are equivalent:

```swift
var explicitVar: Int = 10 // Explicitly typed
var implicitVar = 10 // Implicitly typed
```

Updating a variable's value is done using the `=` operator. Note that the type of that variable, however, is fixed once it is initially defined and like the value associated with a constant, cannot be changed.

```swift
variableName = 13 // update to new value

// compiler error when assigning different type
variableName = "Hello, world!" // Cannot assign value of type 'String' to type 'Int'
```

Constants and variables may be declared without assigning a value by specifying the name and type, but they may not be used before a value is assigned.

```swift
var someInt: Int
let someString: String

// This would trigger a compiler error
// print(someString) // Constant 'someString' used before being initialized
// print(someInt) // Variable 'someInt' used before being initialized

// Assign a value to the names
someInt = 169
someString = "Finally! A value."

print(someString) // prints 'Finally! A value.'
print(someInt) // prints '169'
```

Functions in Swift are also values that are associated with a name, though they are defined using a different syntax from constants and variables. They are defined using the `func` keyword followed by a pair of parentheses enclosing a comma-separated list of parameter names along with their explicit types and an external name to be used by the caller of the function. The parameter list is followed by `->` and the type of the values returned by the function. Finally, this is followed by the body of the function enclosed in a pair of curly braces. For example:

```swift
func add10(to x: Int) -> Int {
  return x + 10
}
```

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters using the external parameter name.

```swift
let fifteen = add10(to: 5)
```

Swift supports two types of comments. Single line comments start with `//` and continue to the following newline. Multiline comments ar inserted between `/*` and `*/`.
