You are working on a game targeting a low-power embedded system and need to write several convenience functions which will be used by other parts of the game.

## 1. Calculate the quotient and remainder of a division

A quotient is the output of a division.

```rust
fn divmod(dividend: i16, divisor: i16) -> (i16, i16)
```

Example:

```rust
assert_eq!(divmod(10, 3), (3, 1));
```

## 2. Choose even-positioned items from an iterator

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

Examples:

```rust
let mut even_ints = evens(0_u8..);
assert_eq!(even_ints.next(), Some(0));
assert_eq!(even_ints.next(), Some(2));
assert_eq!(even_ints.next(), Some(4));
assert_eq!(even_ints.next(), Some(6));
```

```rust
let mut evens_from_odds = evens(1_u16..);
assert_eq!(evens_from_odds.next(), Some(1));
assert_eq!(evens_from_odds.next(), Some(3));
assert_eq!(evens_from_odds.next(), Some(5));
assert_eq!(evens_from_odds.next(), Some(7));
```

## 3. Calculate the manhattan distance of a position from the origin

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

Example:

```rust
assert_eq!(Position(3, 4).manhattan(), 7);
```
