There are three primary conditional statements that are used in Swift, `if` statements, `switch` statements, and `guard` statements. The `if` and `switch` statements are similar to those seen in a number of other languages, and `guard` should feel familiar to users of other languages with early exit statements.

## If statement

The basic format of the `if` statement is as follows:

```swift
if Boolean-expression {
  block
  of
  code 1
} else {
  block
  of
  code 2
}
rest-of-code
```

With this structure, if the Boolean expression evaluates to `true`, the first block of code is run, the second block of code is skipped, and the program resumes running at _rest-of-code_. And if the Boolean expression evaluates to `false`, the first block of code is skipped, the second block of code is run, and the program resumes running at _rest-of-code_.

There are two variants of this pattern that are available in Swift, the else-if and the no-else variants.

### else-if

In cases where the second block of code would just be another `if` statement, the else-if allows us to clean up the code and remove some indentation from our code by moving the `if` up next to the previous `else` and getting rid of a layer of parentheses.

So if we needed to perform different actions when a string is equal to “apple”, “lemon”, “peach”, or any other string, rather than writing:

```swift
if str == "apple" {
    print("Let's bake an apple crumble")
} else {
    if str == "lemon" {
        print("Let's bake a lemon meringue pie!")
    } else {
        if str == "peach" {
            print("Let's bake a peach pie!")
        } else {
            print("Let's buy ice cream.")
        }
    }
}
```

Else-if allows us to write:

```swift
if str == "apple" {
    print("Let's bake an apple crumble")
} else if str == "lemon" {
    print("Let's bake a lemon meringue pie!")
} else if str == "peach" {
    print("Let's bake a peach pie!")
} else {
    print("Let's buy ice cream.")
}
```

### no-else

And if any if-statement only needs to perform code for one of the cases, the else branch can be left out entirely. So for example, if you are writing software that diagnoses patients and you need to log certain symptoms, like elevated heart rate, instead of writing:

```swift
if heartRate > 100 {
  print(“elevated heart rate”)
} else {
  ()
}
```

You can drop the else and just write

```swift
if heartRate > 100 {
  print(“elevated heart rate”)
}
```

## switch statements

While the else-if variant of `if` statements cleans things up considerably, there is still a lot of noise in the code from all of the curly braces. This is where the `switch` statement comes into play. In conditional statements with many possible branches, the switch statement shines. Note, however, that `switch` statements do work a bit differently from `if` statements.

Rather than evaluating a Boolean expression and using the value of that expression to choose the code branch that is run, a simple switch statement takes an input value (or expression which it evaluates to obtain the input value) of some type and compares against one or more values of the same type. If a case is found that matches the input value, the corresponding block of code is run. Note that if multiple cases of a switch statement match the input value, only the first matching case is used. An underscore (`_`) can be used to match all values.

The structure of a switch looks like this:

```swift
switch input-value {
case value1:
    Code block for value1
case value2,
     value3:
    Code block for value2 or value3
case value4:
    Code block for value4
default:
    Code block for any other case
```

So the “apple”, “lemon”, “peach” example from above can be written:

```swift
switch str {
case "apple":
    print("Let's bake an apple crumble")
case "lemon":
    print("Let's bake a lemon meringue pie!")
case "peach":
    print("Let's bake a peach pie!")
default:
    print("Let's buy ice cream.")
}
```

Note that all possible cases must be covered in a `switch` statement. In cases like the above where all possible strings _cannot_ be enumerated, a `default` case can be used to match all remaining cases.

Unlike in some other languages, `switch` cases in Swift do not fall through to the next case unless that behavior is explicitly called for with the `fallthrough` keyword. this is the opposite behavior from C which requires explicit `break` statements to prevent fallthrough.

### Switches with multiple values

Switches may be performed over multiple values at once by placing the expressions to be matched and the matching cases in tuples.

```swift
switch(a+b, a==b) {
case (6, true):
  return "a and b must both be 3"
case (6, false):
  return "neither a nor b can be 3"
case (_, true):
  return "a nd b are equal, but they are not 3"
case (_, false):
  return "a and b can be pretty much anything"
}
```

### Binding and where statements

The values being matched in `switch` statements can also be bound to names which can be used in the body of the case. They can also be used in `where` clauses, which are additional boolean expressions that must evaluate to `true` for the case to match.

```swift
switch (sumOfDivisors(of: x), x) {
case (let total, _) where total == x:
  print(total, "is a perfect number")
default:
  print(x, "is not a perfect number")
}
```

## guard statements

The `guard` statement in swift is used for early returns from Swift functions when a necessary condition which needs to be met for further processing to continue is not met. The form of a `guard` statement is:

```swift
guard Boolean-expression else { exit-actions }
rest-of-code
```

Here, the `guard` checks if _Boolean-expression_ evaluates to true. If it does, then processing continues with _rest-of-code_, otherwise it will execute the code in the else clause. Unlike an `if` statement, a `guard` statement _must_ have an else clause, and unlike the else clause of an if-else, the else clause of a guard _must_ exit the scope of the guard statement. I.e. it must use a control transfer statement she as return, continue, break, or it must throw an error or exit the program.

An example of its use is the sinc function, which is equal to sin(x)/x with sinc(0) defined to be 1, avoiding issues with division by 0. This function can be written in Swift, using a `guard` as:

```swift
func sinc(_ x: Double) -> Double {
    guard x != 0 else { return 1 }
    return sin(x) / x
}

sinc(0)              // returns 1
sinc(Double.pi / 2)  // returns 0.6366197723675814
```

## The Ternary Operator

Swift also supports a fourth type od conditional, [the ternary operator][ternaryoperator]. The ternary operator is an operator that takes, as the name implies, three inputs. The first input is a Boolean expression, and the other two inputs are expressions of the same type. The structure of the ternary operator is:

```swift
Boolean-expression ? expression1 : expression2
```

The ternary operator evaluates _Boolean-expression_ then, if the expression evaluates to `true`, evaluates _expression1_, returning its value, otherwise it evaluates _expression2_ and returns its value.

-

To learn more about these topics it is recommended that you read [A Tour of Swift: Conditionals][conditionals]

[conditionals]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID127
[ternaryoperator]: https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID71
