The normal control flow of loops in Swift can be altered using [Swift's control transfer keywords][control-transfer]. One of these keywords, `return` has been seen before in the basics concept exercise. With the use of `return`, if the loop is inside a function, the function will exit at that point, returning whatever value is specified, just as it would at any other point in a function. Two more control transfer keywords that are often used with loops are `continue` and `break`.

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

[control-transfer]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID135
[labeled-statements]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID141
