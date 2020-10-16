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

While an else-if variant of `if` statements, like the one above, cleans things up considerably compared to a series of `if` statements without `else` clauses, there is still a lot of noise in the code from all of the curly braces. This is where the `switch` statement comes into play. In conditional statements with many possible branches, the switch statement shines. Note, however, that `switch` statements do work a bit differently from `if` statements.

Rather than evaluating a Boolean expression and using the value of that expression to choose the code branch that is run, a simple switch statement takes an input value (or expression which it evaluates to obtain the input value) of some type and compares against one or more values of the same type. If a case is found that matches the input value, the corresponding block of code is run.

Note that all possible cases must be covered in a `switch` statement. In cases like the above where all possible strings _cannot_ be enumerated, a `default` case can be used to match all remaining cases.

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

Additionally, note that if multiple cases of a switch statement match the input value, only the first matching case is used. An underscore (`_`) can be used to match all values.

Unlike in some other languages, `switch` cases in Swift do not fall through to the next case unless that behavior is explicitly called for with the `fallthrough` keyword. this is the opposite behavior from C which requires explicit `break` statements to prevent fallthrough.

### Binding and where statements

The values being matched in `switch` statements can also be bound to names which can be used in the body of the case. They can also be used in `where` clauses, which are additional boolean expressions that must evaluate as `true` for the case to match.

```swift
let x = 1337
switch sumOfDivisors(of: x) {
case let total where total == x:
  print(total, "is a perfect number")
default:
  print(x, "is not a perfect number")
}
```
