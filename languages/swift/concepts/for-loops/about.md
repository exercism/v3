[For-in loops][for-in-loops] are used to iterate over a sequence of values, taking each element in turn, binding it to a variable or constant name of the developer's choosing, then executes a block of code that may refer to the element. When every element of the sequence has been iterated over, the loop exits and execution begins with the first line following the body of the loop.

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

If one needs to mutate the current element of the iteration, it can be declared as a variable in the for-in loop:

```swift
for var x in [123, 900, 7] {
  while x > 0 {
    print(x % 10)
    x /= 10
  }
  print()
}

// prints:
// 3
// 2
// 1
//
// 0
// 0
// 9
//
// 7
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
	  print("Perhaps.")
}

// prints:
// Perhaps.
// Perhaps.
// Perhaps.

```

[for-in-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID121
