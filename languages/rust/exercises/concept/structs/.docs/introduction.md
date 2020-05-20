Structs in Rust are one of the main ways of creating a new type by composing other types. Structs come in three flavors: structs with named fields, tuple structs, and unit structs. For this concept exercise, we'll be exploring the first variant: structs with named fields. 

Structs are defined using the `struct` keyword, followed by the capitalized name of the type the struct is describing:

```rust
struct Item {}
```

Additional types are then brought into the struct body as _fields_ of the struct, each with their own type:

```rust
struct Item {
    name: String,
    weight: f32,
    worth: u32,
}
```

Lastly, methods can be defined on structs inside of an `impl` block:

```rust
impl Item {
    // initializes and returns a new instance of our Item struct
    fn new() -> Self {
        unimplemented!()
    }
}
```

With that brief introduction to the syntax of structs out of the way, go ahead and take a look at the [instructions](instructions.md) for this exercise!