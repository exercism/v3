## Loops

Excellent work! Even though loops are not as commonly seen in Swift as they are in other programming languages, they are still foundational blocks of programming, and it is important to understand them well.

## Key Takeaways from this Exercise

- There are three primary types of loop constructs in Swift: while loops, repeat-while loops, and for-in loops.
- [While loops][while-loops] begin by first evaluating a Boolean expression, then executes the body of the loop if the expression evaluates to `true` before going back to evaluate the boolean expression.
- [Repeat-while loops][repeat-loops] are similar to while loops, however, these loops differ in that the Boolean expression appears, and is evaluated, _after_ the body of the loop is executed. As a result, these loops always execute at least once.
- For-in loops][for-in-loops] are used to iterate over a sequence of values, taking each element in turn, binding it to a variable or constant name of the developer's choosing, then executes a block of code that may refer to the element. When every element of the sequence has been iterated over, the loop exits and execution begins with the first line following the body of the loop.
- If one needs to mutate the current element of the iteration, it can be declared as a variable in the for-in loop: `for var x in xs { … }`.
- If one wants to execute a loop a specified number of times, a for-in loop can be used with a range supplied for the sequence to iterate over: `for i in 1...3 { … }`.
- If the body of the loop doesn't refer to the current element of the sequence, an underscore (`_`) can be supplied for the name: `for _ in 1...3 { … }`.
  }

## Control Flow

The normal control flow of loops in Swift can be altered using [Swift's control transfer keywords][control-transfer]. One of these keywords, `return` has been seen before in the [basics][basics-concept] concept exercise. With the use of `return`, if the loop is inside a function, the function will exit at that point, returning whatever value is specified, just as it would at any other point in a function. Two more control transfer keywords that are often used with loops are `continue` and `break`.

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
