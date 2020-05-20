Notice the usage of `&self` (an _immutable_ reference to our `User` instance) for the getter methods vs. the usage of `&mut self` (a _mutable_ reference to our `User` instance) for the setter methods. The getter methods don't need a mutable reference since their job is to fetch some data; they aren't looking to update or change any fields on the `User`. In contrast, the setter methods _are_ looking to change some data on the `User`, so they require mutable access to the `User` instance. 

It should also be noted that the usage of getter and setter methods in this concept exercise was a bit artificial in order to expose a use-case for `&self` vs. `&mut self` methods. In real projects, the fields would simply be made public, such that we'd have a struct like:

```rust
#[derive(Clone, Debug)]
pub struct User {
    pub name: String,
    pub age: u32, 
    pub weight: f32,
}
```

This way, we can fetch and/or update fields on structs without having to go through a function call.

There are also other struct variants that this exercise didn't touch on, namely _tuple structs_ and _unit structs_.

- [https://learning-rust.github.io/docs/b2.structs.html#Tuple-structs](https://learning-rust.github.io/docs/b2.structs.html#Tuple-structs) highlights the use-cases of tuple and unit structs.

- [https://doc.rust-lang.org/stable/rust-by-example/custom_types/structs.html](https://doc.rust-lang.org/stable/rust-by-example/custom_types/structs.html) in _Rust By Example_ showcases some examples of tuple and unit structs.
