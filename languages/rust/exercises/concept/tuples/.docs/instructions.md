# Instructions

You are working on a game targeting a low-power embedded system and need to write several convenience functions which will be used by other parts of the game.

## 1. Write a function `divmod` which returns both the quotient and remainder of a division

```rust
fn divmod(dividend: i16, divisor: i16) -> (i16, i16)
```

Example:

```rust
assert_eq!(divmod(10, 3), (3, 1));
```

## 2. Write an iterator adaptor `evens` which returns the even items from an arbitrary iterator

This will be helpful to enable a screen-buffer optimization, your boss promises.

[Iterators](https://doc.rust-lang.org/std/iter/trait.Iterator.html) are a separate concept, and we aren't going to get too in-depth about their syntax or mechanics here. The important facts about them for this task:

- An iterator is an arbitrary-length stream of items
- They have an [`enumerate` method](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.enumerate) which returns a tuple `(i, val)` for each value
- They have a [`filter` method](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.filter) which uses a closure to determine whether to yield an element of the iterator
- They have a [`map` method](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.map) which uses a closure to modify elements of the iterator

Don't worry too much about the generic or impl syntax here; it doesn't affect your implementation. It just means that this adaptor can be applied to any iterator, and the resulting iterator has the same item type as the input iterator.

```rust
fn evens<T>(iter: impl Iterator<Item=T>) -> impl Iterator<Item=T>
```

## 3. Implement a `manhattan` method on a `Position` tuple struct

For mapping convenience, you have a tuple struct `Position`:

```rust
struct Position(i16, i16);
```

You need to implement a method `manhattan` on `Position` which returns the [manhattan distance](https://en.wikipedia.org/wiki/Taxicab_geometry) of that position from the origin (`Position(0, 0)`).

```rust
impl Position {
    fn manhattan(&self) -> i16
}
```
