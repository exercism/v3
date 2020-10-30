Elyse, magician-to-be, continues her training. She has a deck of cards she wants to manipulate.

To make things easier, she usually only starts with cards numbered 1 to 10, although some of the tricks may involve additional cards.

## 1. Get the first card

Elyse will summon the first card in the deck.

```javascript
const deck = [5, 9, 7, 1, 8]

getFirstCard(deck)
// => 5
```

## 2. Get the second card

Elyse will summon the second card in the deck.

```javascript
const deck = [3, 2, 10, 6, 7]

getSecondCard(deck)
// => 2
```

## 3. Swap the first two cards

Elyse will make the first two cards switch places.

```javascript
const deck = [10, 7, 3, 8, 5]

swapTopTwoCards(deck)
// => [7, 10, 3, 8, 5]
```

## 4. Discard the top card

Elyse will separate the deck into two piles. The first pile will contain only the top card of the original deck, while the second pile will contain all the other cards.

```javascript
const deck = [2, 5, 4, 9, 3]

discardTopCard(deck)
// => [2, [5, 4, 9, 3]]
```

## 5. Triplicate the deck

Elyse will make the cards in the deck repeat twice, maintaining the original card order.

```javascript
const deck = [3, 2, 6]

triplicateDeck(deck)
// => [3, 2, 6, 3, 2, 6, 3, 2, 6]
```

## 6. Insert face cards

Elyse will insert a set of face cards (i.e. jack, queen, and king) into her deck such that they become the second, third, and fourth cards respectively.

```javascript
const deck = [5, 4, 7, 10]

insertFaceCards(deck)
// => [5, 'jack', 'queen', 'king', 4, 7, 10]