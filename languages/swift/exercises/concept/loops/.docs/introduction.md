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
let word = "Supercalifragilisticexpialidocious"

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


for char in word {
  if "aeiou".contains(char) {
    print(char, terminator: "")
  }
}
print(" - those are all the vowels")

// prints:
// ueaiaiiieiaioiou - those are all the vowels

```

If one wants to execute a loop a specified number of times, a for-in loop can be used with a range supplied for the sequence to iterate over:

```swift
for i in 1...3 {
  print("i: \(i)")
}

// prints:
// i: 1
// i: 2
// i: 3
```

## Control transfer

Sometimes it is necessary to skip to the next iteration of a loop early, without completing the rest of the statements in the currunt iteration of the loop. The `continue` keyword can be used for this. When `continue` is executed, the loop jump to the next check to see if the next itertion of the loop can be run, i.e. the `while` in while and repeat-while loops or the check if there's another element in the sequence in for-in loops.

Similarly, the `break` keyword can be used to exit a loop early, without execution the rest of the current iteration. When a `break` is executed, the loop is exited and execution picks up with the first line of code following the loop.
