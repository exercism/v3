# Instructions

In this exercise you'll be using a `HashMap`, along with entry API methods, to solve a simple algorithm problem.

The problem states: given a slice of `str`s representing a magazine and another slice of `str`s representing a note you're looking to construct, implement a function that returns a boolean indicating whether the note can be constructed from the magazine. In other words, does the magazine contain at least as many instances of each word required by the note?

You'll start with the following stubbed function signature:

```rust
pub fn construct_note(magazine: &[&str], note: &[&str]) -> bool {

}
```

Given the following input

```rust
let magazine = &["two", "times", "three", "is", "not", "four"];
let note = &["two", "times", "two", "is", "four"];
```

the function should return `false` since the magazine only contains one instance of `"two"` when the note requires two of them.

Note that case sensitivity matters.
