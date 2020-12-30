<!--
This is the markdown file with the instructions on how to complete the exercise.

See https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#docsinstructionsmd
-->

As a budding magician, Elyse needs to be able to analyze her deck in lots of different ways.

To keep things a little bit simpler, she only uses cards with values 1-10.

Try to use all three different methods of looping over arrays!

## 1. Determine how many cards of a certain type are in the deck

Write a function that takes in two parameters: an array of cards (Elyse's deck), and the card type that neeeds to be matched.

This function will return the number of cards in the deck that are a given type.

```javascript
const cardType = 3
cardTypeCheck([1, 2, 3, 4], cardType)
// => 1
```

## 2. Determine if there are any unique cards

Write a function that takes in one parameter: an array of cards (Elyse's deck).

This function should return an array of card types that only occur once in the entire deck (i.e. unique cards).

```javascript
uniqueCards([1, 2, 3, 1, 5, 6])
// => [2, 3, 5, 6]
```

## 3. Determine how many odd or even cards there are

Write a function that takes in two parameters: an array of cards (Elyse's deck), and a boolean (true is analogous to 'even', and false is analogous to 'odd').

This function should return a single number: the number of odd or even cards there are (depending on the value of the second argument) in the deck.

```javascript
oddEvenCards([1, 2, 3, 1, 5, 6], true)
// => 2

oddEvenCards([1, 2, 3, 1, 5, 6], false)
// => 4
```