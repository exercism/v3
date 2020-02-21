# Instructions

As a magician-to-be, Elyse needs to practice some basics. She has a stack of cards that she wants to do some operations on.

To make things a bit easier she uses only the cards 1 to 10.

### 1. Retrieve a card from a stack

Return the card at position `index` from the given stack.

```go
GetItem([]uint8{1, 2, 4, 1}, 2)
// Returns: 4
```

### 2. Exchange a card in the stack

Exchange the card at position `index` with the new card provided and return the adjusted stack.
Note that this will also change the input slice which is ok.

```go
SetItem([]uint8{1, 2, 4, 1}, 2, 6)
// Returns: []uint8{1, 2, 6, 1}
```

### 3. Create a stack of cards

Create a stack of given `length` and fill it with cards of the given `value`.

```go
PrefilledSlice(8, 3)
// Returns: []int{8, 8, 8}
```

### 4. Remove a card from the stack

Remove the card at position `index` from the stack and return the stack.

```go
RemoveItem([]int{3, 2, 6, 4, 8}, 2)
// Returns: []int{3, 2, 4, 8}
```
