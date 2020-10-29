If statements in Swift are similar to those seen in other languages. E.g.:

```swift
if myValue > 0 {
  print("myValue is positive")
} else {
  print("myValue is not positive")
}
```

With this structure, if the Boolean expression following the `if` evaluates as `true`, the first block of code is run, the second block of code is skipped. And if the Boolean expression evaluates to `false`, the first block of code is skipped, the second block of code is run. In either case, the program continues running at the first line of code following the `if` statement.

There are two variants of this pattern that are available in Swift, the else-if and the no-else variants.

### else-if

In cases where the second block of code would just be another `if` statement, the else-if allows us to clean up the code and remove some indentation from our code by moving the `if` up next to the previous `else` and getting rid of a layer of parentheses.

So if we needed to perform different actions when a string is equal to “apple”, “lemon”, “peach”, or any other string, instead of nesting additional `if` statements inside the else blocks, one can write:

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
