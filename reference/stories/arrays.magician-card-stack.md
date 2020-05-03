# Magician card stack

## Story introduction

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

Example usage: [`javascript/concept/arrays`][javascript-concept-arrays]

## Terminology

These are recommendations, not rules, for recurring terminology in the instructions (including stub commentary)

- The main character is **Elyse** and her pronouns are **she/her**. If you need to indicate the character, use the name or refer to her by her pronoun(s).
- Instead of `array` (or equivalent data type), use the word **stack** or **card stack**
- Instead of `element`, use the word **card**
- Instead of `length`, use **size of the stack**
- The **top card** is the _first element_
- The **bottom card** is the _last element_

---

## Alternative version

An alternative version of the story uses **you** (the student) as actor, and focusses on re-arranging the deck only.

### Story introduction

You're a magician and you handle a deck of cards. In order to correctly execute your magic trick, you need to be able to move a card from one position to another position. That is, you need to be able to rearrange the deck. Naturally, you want to be able to move cards in both directions, and be able to say "from the top of the deck" or "from the bottom of the deck".

### Tasks

These are examples of tasks that fit the story of you wanting to re-arrange cards:

- Implement a function arrange that moves a card in a stack from one given position to another, returning a new stack
- Implement a function rearrange that does the same, mutating the original stack

Example usage: [`research/javascript-1-a`][javascript-research-1-a]

# Related

[arrays]: ../types/arrays.md
[javascript-concept-arrays]: ../../languages/javascript/exercises/concept/arrays
[javascript-research-1-a]: https://github.com/exercism/research_experiment_1/tree/master/exercises/javascript-1-a
