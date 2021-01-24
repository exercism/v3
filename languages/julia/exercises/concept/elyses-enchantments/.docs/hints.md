## 1. Retrieve a card from a stack

- `Vector` indices start at `1`.

## 2. Exchange a card in the stack

- `Vector`s are mutable, you can change their contents at any time.
- The method takes a [Pair](https://docs.julialang.org/en/v1/base/collections/#Base.Pair) as an argument.
- You can access the fields of a pair `p` using `p.first` and `p.second`.

## 3. Insert a card at the of top the stack

- There is a [built-in](https://docs.julialang.org/en/v1/base/collections/#Base.push!) method to add a new value to the end of a collection.

## 4. Remove a card from the stack

- There is a [built-in](https://docs.julialang.org/en/v1/base/collections/#Base.deleteat!) method to delete an element from a `Vector` at a given index.

## 5. Remove the top card from the stack

- There is a [built-in](https://docs.julialang.org/en/v1/base/collections/#Base.pop!) method to remove an element at the end of a collection.

## 6. Insert a card at the bottom of the stack

- There is a [built-in](https://docs.julialang.org/en/v1/base/collections/#Base.pushfirst!) method to insert a new value to start of a collection.

## 7. Remove a card from the bottom of the stack

- There is a [built-in](https://docs.julialang.org/en/v1/base/collections/#Base.popfirst!) method to remove an element at the start of a collection.

## 8. Check size of the stack

- There is a [built-in](https://docs.julialang.org/en/v1/base/collections/#Base.length) method to retrieve the length of a collection.
