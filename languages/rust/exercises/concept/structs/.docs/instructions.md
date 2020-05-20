It is often useful to group a collection of items together, and handle those groups as units. In Rust, we call such a group a struct, and each item one of the struct's fields. A struct defines the general set of fields available, but a particular example of a struct is called an instance.

Furthermore, structs can have methods defined on them, which have access to the fields. The struct itself in that case is referred to as `self`. When a method uses `&mut self`, the fields can be changed, or mutated. When a method uses `&self`, the fields cannot be changed: they are immutable. Controlling mutability helps the borrow-checker ensure that entire classes of concurrency bug just don't happen in Rust.

In this exercise, you'll be implementing two kinds of methods on a struct. The first are generally known as getters: they expose the struct's fields to the world, without letting anyone else mutate that value. In Rust, these methods idiomatically share the name of the field they expose, i.e., if we have a getter method that fetches a struct field called `name`, the method would simply be called `name()`.

You'll also be implementing methods of another type, generally known as setters. These change the value of the field. Setters aren't very common in Rust--if a field can be freely modified, it is more common to just make it public--but they're useful if updating the field should have side effects, or for access control: a setter marked as `pub(crate)` allows other modules within the same crate to update a private field, which can't be affected by the outside world.

You'll start with some stubbed functions in an `impl` block as well as the following struct definition:

```rust
#[derive(Clone, Debug)]
pub struct User {
    name: String,
    age: u32,
    weight: f32,
}
```

Your goal is to implement the stubbed out methods on the `User` `struct` defined in the `impl` block. 

For example, the `new` method should return an instance of the `User` struct with the specified name, age, and weight values. 

```rust
let mut bob = User::new(String::from("Bob"), 32, 155.2);
// Returns: a User with name "Bob", age 32, and weight 155.2
```

The `weight` method should return the weight of the `User`.

```rust
bob.weight();
// Returns: 155.2
```

The `set_age` method should set the age of the `User`. 

```rust
bob.set_age(33);
// Updates Bob's age to 33; happy birthday Bob!
```

Have fun!
