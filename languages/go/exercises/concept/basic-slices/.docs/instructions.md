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

### 4. Remove item

Remove the item `index` from the slice and return it the slice.

```go
RemoveItem([]int{3, 2, 6, 4, 8}, 2)
// Returns: []int{3, 2, 4, 8}
```
