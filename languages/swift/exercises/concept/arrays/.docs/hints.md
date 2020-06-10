## General

- Arrays in Swift use zero-based indices. The first index in an Array is `0`.

## 1. Retrieve a card from a stack

- To get the `n`th item of a slice [use a subscript][array-access].
- An array always starts with index `0`

## 2. Exchange a card in the stack

- To set the `n`th item in an array [use a subscript][array-access] and asign a new value to it.
- An array always starts with index `0`

## 3. Insert a card at the top of the stack

- There is an [array method][array-append-docs] to add a new value to the end of the array.

## 4. Remove a card from the stack

- There is an [array method][array-removeat-docs] that can be used to remove elements at a specified index.

## 5. Remove the top card from the stack

- There is an [array method][array-removelast-docs] to remove the last element from the array.

## 6. Insert a card at the bottom of the stack

- There is an [array method][array-insert-docs] to add a new value a specified index of the array.
- An array always starts with index `0`

## 7. Remove a card from the bottom of the stack

- There is an [array method][array-removefirst-docs] method to remove the first element from the array.

## 8. Check size of the stack

- Arrays have a [property][array-count-docs] to retrieve their length.


## 9. Count the number of even cards in the stack

- You can step through the elements of an array using a [for-in loop][for-in-loops].
- Integer types in swift have the [`isMultiple(of:)`][int-isMultiple-docs] method to test if they are the multiple of another integer.

[array-access]: https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#ID110
[array-append-docs]: https://developer.apple.com/documentation/swift/array/3126937-append
[array-removeat-docs]: https://developer.apple.com/documentation/swift/array/1641390-remove
[array-removelast-docs]: https://developer.apple.com/documentation/swift/array/2885350-removelast
[array-insert-docs]: https://developer.apple.com/documentation/swift/array/3126951-insert
[array-removefirst-docs]: https://developer.apple.com/documentation/swift/array/2884646-removefirst
[array-count-docs]: https://developer.apple.com/documentation/swift/array/2943906-count
[for-in-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID121
[int-isMultiple-docs]: https://developer.apple.com/documentation/swift/int/3127688-ismultiple