# Instructions

This exercise is about working with slices.

### 1. Get item from a slice

Return the value at position `index` from the given slice.

```go
GetItem([]uint8{1, 2, 4, 1}, 2)
// Returns: 4
```

### 2. Set item in a slice

Overwrite the value at position `index` with the new value provided and return the adjusted slice.
Note that this will also change the input slice which is ok.

```go
SetItem([]uint8{1, 2, 4, 1}, 2, 6)
// Returns: []uint8{1, 2, 6, 1}
```

### 3. Prefilled slice

Create a slice of given `length` and fill all items with the given `value`.

```go
PrefilledSlice(8, 3)
// Returns: []int{8, 8, 8}
```

### 4. Number Sequence

Create a slice with the numbers 1, 2, 3, etc. The sum of the numbers has to be equal to or greater than `sumMin`.
The function shouldn't add more numbers than needed to reach `sumMin`.

```go
NumberSequence(11)
// Returns: []int{1, 2, 3, 4, 5}
```

### 5. Remove item

Remove the item `index` from the slice and return it.

```go
RemoveItem([]int{3, 2, 6, 4, 8}, 2)
// Returns: []int{3, 2, 4, 8}
```

### 6. Remove item in a pure way

Solve the same problem as in the previous task (`RemoveItem`) but without changing the input slice.

Note: A function is called `pure` if it has no side effects. That includes not changing the input values.

```go
RemoveItemPure([]int{3, 2, 6, 4, 8}, 2)
// Returns: []int{3, 2, 4, 8}
```
