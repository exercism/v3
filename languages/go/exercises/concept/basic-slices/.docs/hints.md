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

### 4. Remove item

- Removing an item from a slice can be done by appending the part after `index` to the part before index.
