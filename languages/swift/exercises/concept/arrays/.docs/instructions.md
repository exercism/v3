As a magician-to-be, Elyse needs to practice some basics. She has a stack of cards that she wants to manipulate.

To make things a bit easier she only uses the cards 1 to 10.

## 1. Retrieve a card from a stack

Return the card at position `index` from the given stack.

```swift
let index = 2
getCard(at: index, from: [1, 2, 4, 1])
// => 4
```

## 2. Change a card in the stack

Return a new stack that is a copy of the input stack but which has the card at position `index` changed to the the new card provided

```swift
let index = 2
let newCard = 6
setCard(at: index, in: [1, 2, 4, 1], to: newCard)
// => [1, 2, 6, 1]
```

## 3. Insert a card at the of top the stack

Return a copy of the stack with the new card provided added to the top of the stack.

```swift
let newCard = 8
insert(newCard, atTopOf: [5, 9, 7, 1])
// => [5, 9, 7, 1, 8]
```

## 4. Remove a card from the stack

Return a copy of the stack which has had the card at position `index` removed.

```swift
let index = 2
removeCard(at: index, from: [3, 2, 6, 4, 8])
// => [3, 2, 4, 8]
```

## 5. Remove the top card from the stack

Return a copy of the stack which has had the card at the top of the stack removed.

```swift
removeTopCard([3, 2, 6, 4, 8])
// => [3, 2, 6, 4]
```

## 6. Insert a card at the bottom of the stack

Return a copy of the stack with the new card provided added to the bottom of the stack.

```swift
let newCard = 8
insert(newCard, atBottomOf: [5, 9, 7, 1])
// => [8, 5, 9, 7, 1]
```

## 7. Remove a card from the bottom of the stack

Return a copy of the stack which has had the card at the bottom of the stack removed.

```swift
removeBottomCard([8, 5, 9, 7, 1])
// => [5, 9, 7, 1]
```

## 8. Check size of the stack

Check whether the size of the stack is equal a given `stackSize` or not.

```swift
let stackSize = 4
checkSizeOfStack([3, 2, 6, 4, 8], stackSize)
// => false
```
