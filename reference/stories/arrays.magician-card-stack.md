# Magician card stack

## Story

As a magician-to-be, Elyse needs to practice some basics. She has a stack of cards that she wants to manipulate.

To make things a bit easier she only uses the cards 1 to 10.

> <!-- not part of the story -->
>
> We limit the card values from 1 to 10, as to only have to deal with numbers,
> so that exercises that use this story only need to rely on numbers.

## Tasks

These are examples of tasks that fit the story of Elyse being a beginner:

- Retrieve a card from a stack
- Exchange a card in the stack
- Insert a card at the of top the stack
- Remove a card from the stack
- Remove the top card from the stack
- Insert a card at the bottom of the stack
- Remove a card from the bottom of the stack
- Check size of the stack

This story can be continued to also teach array analysis:

- Find the position of a card
- Determine if a card is present
- Determine if each card is even
- Check if the stack contains an odd-value card
- Get the first odd card from the stack
- Determine the position of the first card that is even

This story can be continued to also teach array destructuring:

## Terminology

These are recommendations, not rules, for recurring terminology in the instructions (including stub commentary)

- The main character is **Elyse** and her pronouns are **she/her**. If you need to indicate the character, use the name or refer to her by her pronoun(s).
- Instead of `array` (or equivalent data type), use the word **stack** or **card stack**
- Instead of `element`, use the word **card**
- Instead of `length`, use **size of the stack**
- The **top card** is the _first element_
- The **bottom card** is the _last element_

## Implementations

- [JavaScript: arrays][implementation-javascript] (reference implementation)
- [JavaScript: array-analysis][implementation-javascript-2] (reference implementation)
- [JavaScript: array-destructuring][implementation-javascript-3] (reference implementation)
- [Swift: arrays][implementation-swift]

## Alternative version

An alternative version of the story uses **you** (the student) as actor, and focusses on re-arranging the deck only.

### Story

You're a magician and you handle a deck of cards. In order to correctly execute your magic trick, you need to be able to move a card from one position to another position. That is, you need to be able to rearrange the deck. Naturally, you want to be able to move cards in both directions, and be able to say "from the top of the deck" or "from the bottom of the deck".

### Tasks

These are examples of tasks that fit the story of you wanting to re-arrange cards:

- Implement a function arrange that moves a card in a stack from one given position to another, returning a new stack
- Implement a function rearrange that does the same, mutating the original stack

### Implementations

- [JavaScript 1-a (research)][implementation-javascript-research-1-a]
- [JavaScript 2-a (research)][implementation-javascript-research-2-a]

## Reference

- [`types/array`][types-array]

[types-array]: ../types/array.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/arrays/.docs/instructions.md
[implementation-javascript-2]: ../../languages/javascript/exercises/concept/array-analysis/.docs/instructions.md
[implementation-javascript-3]: ../../languages/javascript/exercises/concept/array-destructuring/.docs/instructions.md
[implementation-javascript-research-1-a]: https://github.com/exercism/research_experiment_1/tree/master/exercises/javascript-1-a
[implementation-javascript-research-2-a]: https://github.com/exercism/research_experiment_1/tree/master/exercises/javascript-2-a
[implementation-swift]: ../../languages/swift/exercises/concept/arrays/.docs/instructions.md
