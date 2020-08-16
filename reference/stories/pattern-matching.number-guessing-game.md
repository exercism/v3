# Number guessing game

## Story

In this exercise, you are playing a number guessing game with a friend. The rules are simple: you secretly choose a number between `1` and `100` and your friend tries to guess what number you've chosen. To help your friend, you respond differently depending on how close the guess was to the number you've chosen (`42`). These are the rules for the different replies:

- If the guess is `42`: "Correct"
- If the guess is `41` or `43`: "So close"
- If the guess is less than `41`: "Too low"
- If the guess is greater than `43`: "Too high"

## Tasks

These are example tasks that fit the number guessing game exercise:

- Reply to a correct guess
- Reply to a close guess
- Reply to too low guesses
- Reply to too high guesses

## Implementations

- [F#: pattern-matching][implementation-fsharp] (reference implementation)
- [Elixir: multiple-clause-functions][implementation-elixir] (forked implementation)

## Related

- [`concepts/pattern_matching`][concepts-pattern_matching]

[concepts-pattern_matching]: ../concepts/pattern_matching.md
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/pattern-matching/.docs/instructions.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/multiple-clause-functions/.docs/instructions.md
