You're tasked with implementing a Scrabble score calculator.
Given that this is a solved problem, you decide to look up an example solution in a textbook instead of writing your own.
Unfortunately the example solution does not include tests.
You decide to write your own test suite based on the Scrabble rules to ensure that the calculator works as intended.

<!-- TODO: Add some explanation on how to actually count the score for people unfamiliar with the game -->

!!! note<!-- TODO: Does this syntax actually work? -->
    The point of this exercises is to learn how unit testing works in Julia, not to create a comprehensive test suite that you might find in a real world project.
    It's enough to write 2-3 tests for specific example inputs per task.

## 0. Exercise structure

- `scrabble-score.jl`: Contains the working Scrabble score calculator. _You should not edit this file!_
- `scrabble-score-tests.jl`: This is where you will write your test suite.
- `runtests.jl`: As always, this file contains tests that test your solution to the exercise.

## 1. Verify that the calculator works for English Scrabble rules

The English Score Distribution[^Scoring] is as follows:

| Points | Letters                      |
|-------:|------------------------------|
| 0      | blank                        |
| 1      | E, A, I, O, N, R, T, L, S, U |
| 2      | D, G                         |
| 3      | B, C, M, P                   |
| 4      | F, H, V, W, Y                |
| 5      | K                            |
| 8      | J, X                         |
| 10     | Q, Z                         |

Write a few test cases that `score(s, lang = :en)` returns the correct result.

### Example

"Quirky" is scored as worth 22 points:

```julia
julia> score("quirky", lang = :en)
22
```

- 10 points for Q
- 1 point for U
- 1 point for I
- 1 point for R
- 5 points for K
- 4 points for Y

Therefore: 10 + 1 + 1 + 1 + 5 + 4 = 22 points.

## 2. Verify that the calculator throws a `DomainError` when a word contains letters not included in the English Scrabble edition

```julia
julia> score("schräg", lang=:en)
ERROR: DomainError with ä:
Letter not included in English Scrabble.
```

## Group these tests together

## Add tests for another language

<!-- TODO: Do footnotes work? -->
[^Scoring]: Scrabble letter distributions, Wikipedia. (2020). https://en.wikipedia.org/w/index.php?title=Scrabble_letter_distributions&oldid=982590424 (accessed October 9, 2020).
