## Loops

There are three primary types of loop constructs in Swift: while loops, repeat-while loops, and for-in loops.

### while loops

While loops in Swift have the following structure:

```swift
while boolean-expression {
  loop-body
}
```

The loops begin by first evaluating a Boolean expression. If the expression evaluates to `false`, then the body of the loop is skipped and execution begins on the first line following the loop. If the condition evaluates to `true`, then the body of the loop is executed, after which the Boolean expression is evaluated again, repeating this way until the Boolean expression evaluates to false.

```swift
var count = 3
while count > 0 {
  print("\(count)…")
  count -= 1
}
print("Liftoff!")

// prints:
// 3…
// 2…
// 1…
// Liftoff!
```

### repeat-while loops

Repeat-while loops are similar to while loops, however, these loops differ in that the Boolean expression appears, and is evaluated, _after_ the body of the loop is executed. As a result, these loops always execute at least once.

```swift
repeat {
  print("This runs at least once")
} while false
print("Loop done")

// prints:
// This runs at least once
// Loop done
```

### for-in loops

For-in loops are used to iterate over a sequence of values, taking each element in turn, binding it to a variable or constant name of the developer's choosing, then executes a block of code that may refer to the element. When every element of the sequence has been iterated over, the loop exits and execution begins with the first line following the body of the loop.

```swift
let numbers = [3, 10, 7, 11]

for number in numbers {
  print("\(number) / 2 = \(number / 2)")
}
print("Done with numbers")

// prints:
// 3 / 2 = 1
// 10 / 2 = 5
// 7 / 2 = 3
// 11 / 2 = 5
// Done with numbers
```

## Control transfer

Sometimes it is necessary to skip to the next iteration of a loop early, without completing the rest of the statements in the current iteration of the loop. The `continue` keyword can be used for this. When `continue` is executed, the loop jump to the next check to see if the next iteration of the loop can be run, i.e. the `while` in while and repeat-while loops or the check if there's another element in the sequence in for-in loops.

```swift
count = 1
repeat {
  count += 1
  if count == 4 { continue }
  print(count)
} while count !=4 && count < 6
```

prints:

```
2
3
```

rather than:

```
2
3
5
6
```

Similarly, the `break` keyword can be used to exit a loop early, without executing the rest of the current iteration. When a `break` is executed, the loop is exited and execution picks up with the first line of code following the loop.

```swift
for fruit in ["banana", "grapes", "apple", "strawberry", "kiwi", "lemon"] {
  if !fruit.count.isMultiple(of: 2) { break }
  print(fruit)
}

// prints:
// banana
// grapes
```
