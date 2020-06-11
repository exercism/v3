## General

Read about [optionals][optionals] in Apple's _A Tour of Swift_ and about conditionals in the [conditionals exercise][conditionals].

## 1. Write a function to compute slice sizes which returns nil for invalid input.

1. You can convert an `Int` to a `Double` by calling `Double(intVar)`.
2. You can use a guard-style conditional to exit early if either parameter is invalid.

## 2. Process input from your web application to determine the larger slice.

1. You can use `if let` optional binding to decide whetheer to call `sliceSize` for a given slice.
2. You can use `guard let` to exit thee function early if you know the what the return value should be without further processing.
3. Remember, you cannot comparee `nil` with non-nil values.

[optionals]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID330
[conditionals]: ../../conditionals/.docs/introduction.md
