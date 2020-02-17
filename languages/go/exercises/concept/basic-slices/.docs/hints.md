# Hints

### General

- slices in Go are zero-based. The first index in a slice is `0`.

### 1. Get item from a slice

- To get the `n`th item from a slice use `slice[n]`.
- To check if an item exists in a slice use a conditional and compare with `len(slice)`.

Note: a slice always starts with index `0`, `1`, `2`, etc.

### 2. Set item in a slice

- To set the `n`th item in a slice asign a new value to it: `slice[n] = x`.
- To add a new item to a list use the builtin `append` function.

### 3. Prefilled slice

- There are several ways to create a new slice. The simplest is `var s []int`. More options you get with the `make` function.
- Use the techniques from 2. to fill the slice with the correct values in a `for` loop.

### 4. Number Sequence

- Create a loop to fill the slice with increasing numbers.
- Use an extra integer to sum up the already inserted values.
- Use the `break` keyword in a conditional to stop the loop when the sum is large enough.

### 5. Remove item

- Removing an item from a slice can be done by appending the part after `index` to the part before index.

### 6. Remove item in a pure way

- This one is about creating a new slice and filling it with all the values from the old slice except one: the index to be removed.
- To copy a slice or parts of a slice use the `copy` function.
- You can use the `append` function with multiple values at once: `append(slice, x1, x2, x3)`.
You'll need the expand operator to append a slice to a slice: `append(slice, otherslice...)`

Note: make sure not to change the input slice. The `append` function is not `pure` and changes the original slice.
Appending to a new, empty slice might do the trick.
