`String` is a potentially-mutable utf8-encoded representation of a sequence of Unicode codepoints. (In Java or C#, this type is called `StringBuilder`.)

`&str` is a read only view of a well-formed utf8 sequence.

`String` and `&str` in rust can not be indexed into like you might in other languages.
For example this will not work:

```rust,invalid
let hello = "Hello";
println!("First letter = {}", hello[0]);
```

This derives from the following properties:

- Strings are utf8, whose primitive is not a byte, but a Unicode codepoint: `char`.
- Unicode codepoints are up to 4 bytes wide, but encode with variable width.
- It is therefore impossible to compute the position of the `n`th codepoint without iterating over the actual encoded characters, because past the first character, the position of the next character depends on how wide the previous ones were.
- Rust's standard library authors therefore chose not to implement the `Index` trait for strings: while it takes a constant time to index into a byte array, it might take quite a long time to index into a string. This violates the principle of least surprise.

```rust
let hello = "Hello";
println!("First letter = {}", hello.chars().first().unwrap());
```
