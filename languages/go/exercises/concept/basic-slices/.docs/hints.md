# Hints

### General

- slices in Go are zero-based. The first index in a slice is `0`.

### 1. Retrieve a card from a stack

- To get the `n`th item of a slice [use an index](https://blog.golang.org/go-slices-usage-and-internals).
- To check if an item exists in a slice use a conditional and compare with `len(slice)`.

Note: a slice always starts with index `0`, `1`, `2`, etc.

### 2. Exchange a card in the stack

- To set the `n`th item in a slice asign a new value to it: `slice[n] = x`.
- To add a new item to a list use the builtin `append` function.

### 3. Create a stack of cards

- There are several ways to create a new slice. The simplest is `var s []int`. More options you get with the `make` function.
- Use the techniques from 2. to fill the slice with the correct values in a `for` loop.

### 4. Remove a card from the stack

- Removing an item from a slice can be done by appending the part after `index` to the part before index.
