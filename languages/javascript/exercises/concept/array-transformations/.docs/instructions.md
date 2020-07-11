Elyse, magician-to-be, continues her training. She will be given several stacks of cards
and attempt to transform those cards into different cards or even make
certain cards appear, disappear, or change position within the deck.
To make things a bit easier, she only starts the cards numbered 1 to 10 -
although some of the tricks might create different cards.

## 1. - Double every single card

Elyse wants to double the number of every card in the deck. This may result in higher cards than 1-10.

```javascript
const deck = [1, 2, 3, 4, 10]
seeingDouble(deck)
// => [2, 4, 6, 8, 20]
```

## 2. - Create multiple copies of every 3 found in the deck

Elyse wants to triplicate ever 3 found in the deck. If a deck
started with a single 3, after the trick the deck would have
three 3's in place of original.

```javascript
const deck = [1, 3, 9, 3, 7]
threeOfEachThree(deck)
// => [1, 3, 3, 3, 9, 3, 3, 3, 7]
```

## 3. - Find two cards from the exact middle of the deck

Elyse will take a deck and make every card disappear except the
middle two cards. This trick is always performed with exactly 10 cards.

```javascript
const deck = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
middleTwo(deck)
// => [5, 6]
```

## 4. - The outside two cards will reappear in the middle of the deck

Elyse wants to move the top and bottom cards of the deck
into the middle, in reverse order.

```javascript
const deck = [1, 2, 3, 5, 6, 10]
sandwichTrick(deck)
// => [2, 3, 10, 1, 5, 6]
```

## 5. - Every card that isn't 2 disappears

Elyse's favorite number today is 2. In this trick every card
that isn't a 2 will disappear from the eck.

```javascript
const deck = [1, 2, 3, 4, 10, 2]
twoIsSpecial(deck)
// => [2, 2]
```

## 6. - Convert a shuffled deck into a perfectly ordered deck

Elyse wishes to demonstrate her mastery of reordering the cards
perfectly - no matter how well shuffled.

```javascript
const deck = [10, 1, 5, 3, 2, 8, 7]
perfectlyOrdered(deck)
// => [1, 2, 3, 5, 7, 8, 10]
```
