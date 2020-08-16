In this exercise you will be using closures with a higher-order function to generate sequences of numbers from a seed number.

You are given the function

```swift
unroll(_: Int,
       generator: (Int) -> (seed: Int, value: Int),
       stop: (Int) -> Bool
      ) -> [Int]
```

`unroll` takes three parameters, a seed value; a `generator` function which, when applied to the seed returns a tuple consisting of the next value for the seed and the next value for the sequence; and a `stop` function which, when applied to the current seed value returns true if the sequence generation is finished. Unroll then returns the generated sequence. For example, to generate the powers of 3 up to 1,000,000, we can write:

```swift
let powersOf3 = unroll(1,
                       generator: { (n: Int) -> (seed: Int, next: Int) in (seed: 3 * n, next: n) },
                       stop: { (n: Int) -> Bool in n > 1_000_000 })
// => [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441]
```

You will use unroll to help you generate a few sequences.

## 1. Write helper closures to generate a sequence of digits

One can generate a sequence of digits of a number, _n_ by computing `n % 10` to get the last digit of `n` and then dividing `n` by 10 to remove that last digit. This process can then be repeated until `n` is 0.

For this task, you need to define a closure of type `(Int) -> (seed: Int, next: Int)` That takes a number and returns that number with its last digit removed as `seed:` along with the last digit as `next:`. Assign this closure to a constant named `generateDigit`.

You will also need to generate a closure of type `(Int) -> Bool` That returns `true` if n = 0 and `false` otherwise. Assign this closure to a constant named `stopAtZero`.

```swift
generateDigit(123)
// => (seed: 12, next: 3)

stopAtZero(0)
// => true
```

## 2. Generate a sequence of digits of an integer

Use these helper closures with unroll to define a closure of type `(Int) -> [Int]` that takes an Int as input and returns the sequence of digits. Assign this closure to a constant named `digitsOf`.

```swift
digitsOf(123)
// => [3, 2, 1]
```

## 3. Write helper closures to generate a sequence of bits

One can generate a sequence of digits of a number, _n_ by computing `n % 2` to get the last bit of `n` and then dividing `n` by 2 to remove that last bit. This process can then be repeated until `n` is 0.

For this task, you need to define a closure of type `(Int) -> (seed: Int, next: Int)` That takes a number and returns that number with its last digit removed as `seed:` along with the last digit as `next:`. Assign this closure to a constant named `generateBit`.

```swift
generateBit(11)
// => (seed: 5, next: 1)
```

## 4. Generate a sequence of bits of an integer

Use these helper closures with unroll to define a closure of type `(Int) -> [Int]` that takes an Int as input and returns the sequence of bits. Assign this closure to a constant named `bitsOf`.

```swift
bitsOf(11)
// => [1, 1, 0, 1]
```

## 5. Generate a closure for any base from 2-10

Implement the function:

```swift
generatorForBase(_ base: Int) -> ((Int) -> (seed: Int, next: Int))?
```

That takes an integer, `base` and returns an optional closure. The function should return a closure that works just like `generateDigit` or `generateBit`, but for base `base` rather than just base 2 or 10.

```swift
let gen7 = generatorForBase(7)
// => (Int) -> (seed: Int, next: Int)
gen7!(123)
// => (seed 17, next 4)
```

## 6. Generate a Collatz sequence from a positive integer

The [Collatz Conjecture][collatz-conjecture] is a famous mathematical conjecture that states that one can generate a sequence where each term can be generated from the previous term, `n`, by applying the following rule:

- if `n` is even, the next term is `n/2`
- if `n` is odd, the next term is `3*n + 1`

According to the conjecture, for any positive integer, the Collatz sequence generated starting with that number will always reach the value 1. It is not known if the conjecture is true or not, though it has been verified for all integers up to 2<sup>68</sup>.

For this task, you will write closures that will allow you to use `unroll` to generate Collatz sequences. You will need to write three closures:

- The first closure has type `(Int) -> (seed: Int, next: Int)`. It takes as input the current term of the sequence and returns that value as `next:` and the result of applying the Collatz rule as `seed`. Assign this closure to the constant `generateCollatz`
- The second closure is of type `(Int) -> Bool` and returns `true` if n = 1 and `false` otherwise. Assign this closure to a constant named `stopAtOne`.
- The third closure uses the first two closures, along with unroll, to define a closure of type `(Int) -> [Int]` that takes an Int as input and returns the Collatz sequence starting with the input. Note that the input must be positive, otherwise, return an empty array. Assign this closure to a constant named `collatz`.

```swift
generateCollatz(13)
// => (base 40, value 13)

stopAtOne(40)
// => false

collatz(13)
// => [13, 40, 20, 10, 5, 16, 8, 4, 2]
```

[collatz-conjecture]: https://en.wikipedia.org/wiki/Collatz_conjecture
