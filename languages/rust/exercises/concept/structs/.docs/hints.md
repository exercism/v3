## General
- [https://learning-rust.github.io/docs/b2.structs.html#C-like-structs](https://learning-rust.github.io/docs/b2.structs.html#C-like-structs) provides a good overview of the different types of structs and their syntax.

- [https://doc.rust-lang.org/book/ch05-01-defining-structs.html](https://doc.rust-lang.org/book/ch05-01-defining-structs.html) in _The Rust Programming Language_ book talks about how to define structs.

- [https://doc.rust-lang.org/book/ch05-03-method-syntax.html](https://doc.rust-lang.org/book/ch05-03-method-syntax.html) in _The Rust Programming Language_ showcases the syntax for defining methods on structs.

- Notice that the `get_name` method returns a `&str` when the `name` field on the `User` struct is a `String`. How can we get `&str` and `String` to play nice with each other?

- There's no need to use a `return` statement in Rust unless you expressly want a function or method to return early. Otherwise, it's more idiomatic to utilize an _implicit_ return by omitting the semicolon for the result we want a function or method to return. It's not _wrong_ to use an explicit return, but it's cleaner to take advantage of implicit returns where possible.

```rust
fn foo() -> i32 {
    1
}
```
