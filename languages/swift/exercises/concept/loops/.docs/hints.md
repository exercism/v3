## 1. Hash the IDs from the database

- A [for-in loop][for-in-loops] can be used to work on each ID one at a time.
- If you are on a 32-bit machine, you will need to convert the IDs to `Int64` to compute the hash, then convert the result back to `Int`.

## 2. Compute the digital sum of a number

- You can get the rightmost digit of a number, _n_, by computing `n % 10`.
- You can remove the rightmost digit of a number _n_, by computing `n / 10`.
- If we know a number is not 0, we know that we will have at least one digit to add into the sum of digits. This is a good indication that a [repeat-while loop][repeat-loops] would be a good choice to use to compute the digital sum.

## 3. Compute the ranking level for a hashed ID

- We can use the `digitalSum(_:)` function to determine if a number is a Harshad number and thus if we need to increase our ranking level.
- We need to increase the ranking level and compute the next number in the sequence while the current value is a Harshad number. This is a good indication that a [while loop][while-loops] would be a good choice to use to compute the ranking level.

## 4. Compute the ranking for each ID

- A [for-in loop][for-in-loops] can be used to work on each hashed ID one at a time.
- Tuple decomposition can be used to name the individual elements of the tuple in the for-in loop.

[for-in-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID121
[while-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID124
[repeat-loops]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID126
[control-transfer]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID135
[labeled-statements]: https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID141
[basics-concept]: https://../../basics/.docs/after.md
[tuples]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID329
