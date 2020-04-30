As a magician-to-be, Elyse now need to practice more tricks. She will be given several stack of cards that she need check without looking through one by one.

To make things a bit easier she only uses the cards 1 to 10 and no redundant number in the stack.

### 1. Check card position

Elyse need the given card `index` within stack.

```javascript
const card = 2
getCardIndex([9, 7, 3, 2], card)
// => 3
```

### 2. Check whether the given card is in the stack

Elyse was given a card and she need to check if the card given is in the stack

```javascript
const cardGiven = 3
isStackIncludes([2, 3, 4, 5], cardGiven)
// => true
```

### 3. Check whether every card in stack is even number

Elyse must check if every card in the stack was even. looking through one by one.

```javascript
isEvenStack([2, 4, 6, 7])
// => false
```

### 4. Check whether stack contains odd number

Elyse need to check if the stack contains odd number

```javascript
isStackContainsOdd([3, 2, 6, 4, 8])
// => true
```

### 5. Retrive the first odd value card from the stack

Elyse need to get a first odd value card from bottom of the stack.
Note that she will we given at least 1 odd value card

```javascript
getFirstOddCard([4, 2, 8, 7, 9])
// => 7
```

### 6. Check the position of the first even card from stack

Elyse need to check the `index` of a first even value card, from bottom of the stack
Note that she will we given at least 1 even value card

```javascript
getFirstEvenCardIndex([5, 2, 3, 1])
// => 1
```
