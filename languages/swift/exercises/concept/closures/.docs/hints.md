## General

- The syntax for defining a closure is `{ (parameter list) -> ReturnType in bodyOfClosure }`

## 1. Write helper closures to generate a sequence of digits

- You can return the last digit of a number, `n`, by computing `n % 10`
- You can remove the last digit of a number, `n`, by computing `n / 10`

## 2. Generate a sequence of digits of an integer

- Closures capture the values in their enclosing scope and can access them without them being passed in.

## 3. Write helper closures to generate a sequence of bits

- You can return the last bit of a number, `n`, by computing `n % 2`
- You can remove the last bit of a number, `n`, by computing `n / 2`

## 4. Generate a sequence of bits of an integer

- Closures capture the values in their enclosing scope and can access them without them being passed in.
- The `stopAtZero` closure can be reused.

## 5. Generate a closure for any base from 2-10

- The closure returned from `generatorForBase(_:)` should function similarly to `generateDigit` for base 10 and `generateBit` for base 2.

## 6. Generate a Collatz sequence from a positive integer

- You can use [`isMultiple(of:)][is-multiple-of] to test if a number is even or odd.

[is-multiple-of]: https://developer.apple.com/documentation/swift/int/3127688-ismultiple
