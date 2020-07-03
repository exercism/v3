## Loops

There are three primary types of loop constructs in Swift: while loops, repeat-while loops, and for-in loops.

### while loops

[While loops][while-loops] in Swift have the following structure:

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

[Repeat-while loops][repeat-loops] are similar to while loops, however, these loops differ in that the Boolean expression appears, and is evaluated, _after_ the body of the loop is executed. As a result, these loops always execute at least once.

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

[For-in loops][for-in-loops] are used to iterate over a sequence of values, taking each element in turn, binding it to a variable or constant name of the developer's choosing, then executes a block of code that may refer to the element. When every element of the sequence has been iterated over, the loop exits and execution begins with the first line following the body of the loop.

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

let word = "Supercalifragilisticexpialidocious"

for char in word {
  if "aeiou".contains(char) {
    print(char, terminator: "")
  }
}
print(" - those are all the vowels")

// prints:
// ueaiaiiieiaioiou - those are all the vowels

```

If one needs to mutate the current element of the iteration, it can be declared as a variable in the for-in loop:

```swift
let numbers = [3, 10, 7, 11]
for var number in numbers {
  number *= 2
  print("Doubled number: \(number)")
}

// prints:
// Doubled number: 6
// Doubled number: 20
// Doubled number: 7
// Doubled number: 11
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

If the body of the loop doesn't refer to the current element of the sequence, an underscore (`_`) can be supplied for the name:

```swift
for _ in 1...3 {
  print("Hello")
}

// prints:
// Hello
// Hello
// Hello
```

## Control transfer

The normal control flow of loops in Swift can be altered using [Swift's control transfer keywords][control-transfer]. One of these keywords, `return` has been seen before in the [basics][basics-concept] concept exercise. With the use of `return`, if the loop is inside a function, the function wil exit at that point, returning whatever value is specified, just as it would at any other point in a function. Two more control transfer keywords that are often used with loops are `continue` and `break`.

### continue

When the `continue` keyword is executed inside a loop, it tells the loop to skip immediately to the next iteration of the loop, skipping any lines of code that may lie between it and the end of the body of the loop. Note that execution resumes with the next evaluation of the Boolean expression for while and repeat-while loops, and not directly to the start of the loop body. So this loop:

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

### break

When the `break` keyword is executed inside a loop, it tells the loop to jump immediately out of the loop and to resume execution with the first statement following the body of the loop, skipping any lines of code that may lie between it and the end of the body of the loop and not attempting to start another iteration of the loop.

```swift
for fruit in ["banana", "grapes", "apple", "strawberry", "kiwi", "lemon"] {
  if !fruit.count.isMultiple(of: 2) { break }
  print(fruit)
}

// prints:
// banana
// grapes
```

### labels

When loops are nested, there are times when one may want to use `break` or `continue` to exit or restart the outer loops that contain the loop in which the `break` or `continue` are used. In cases like these, [labels may be used][labeled-statements] to specify the loop to be exited or restarted. A loop can be labeled by putting a name followed by a colon before the `while`, `repeat`, or `for` that starts the loop.

To see the effect of using labels in this way, consider the following loop.

```swift
for fruit in ["banana", "grapes", "apple", "strawberry", "kiwi", "lemon"] {
  print("\n--- \(fruit) ---")
  for letter in fruit {
    guard letter != "e" else { continue }
    if "aiou".contains(letter) {
      continue
    } else {
      print(letter, terminator: "")
    }
  }
  print("\n***")
}
```

This loop takes each word and prints a header containing the full word followed by all of the non-vowel characters as, when the current `letter` is a vowel, the `continue` starts the next iteration of the inner loop with the next character in the word. After all of the non-vowel characters are printed, a line of asterisks and a newline are printed and the next word is processed.

The output of this loop is:

```swift

--- banana ---
bnn
***

--- grapes ---
grps
***

--- apple ---
ppl
***

--- strawberry ---
strwbrry
***

--- kiwi ---
kw
***

--- lemon ---
lmn
***
```

However, if we label the outer loop, calling it `mainLoop`, and specify continuing _that_ loop when the current `letter` is "e" then any non-vowel letters following the "e" are not printed, nor is the trailing line of asterisks and newline.

```swift
mainLoop: for fruit in ["banana", "grapes", "apple", "strawberry", "kiwi", "lemon"] {
  print("\n--- \(fruit) ---")
  for letter in fruit {
    guard letter != "e" else { continue mainLoop }
    if "aiou".contains(letter) {
      continue
    } else {
      print(letter, terminator: "")
    }
  }
}
```

The output of this version of the loop is:

```swift

--- banana ---
bnn
***

--- grapes ---
grp
--- apple ---
ppl
--- strawberry ---
strwb
--- kiwi ---
kw
***

--- lemon ---
l
```

Note the difference in the "grapes", "apple", "strawberry", and "lemon" lines.

[for-in-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID121
[while-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID124
[repeat-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID126
[control-transfer]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID135
[labeled-statements]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID141
[basics-concept]: https://../../basics/.docs/after.md
