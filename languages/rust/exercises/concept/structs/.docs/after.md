Notice the usage of `&self` (an _immutable_ reference to our `User` instance) for the `get_` methods vs. the usage of `&mut self` (a _mutable_ reference to our `User` instance) for the `update_` methods. The `get_` methods don't need a mutable reference since their job is to fetch some data; they aren't looking to update or change any fields on the `User`. In contrast, the `update_` methods _are_ looking to change some data on the `User`, so they require mutable access to the `User` instance. 

There are also other struct variants that this exercise didn't touch on, namely _tuple structs_ and _unit structs_.

- [This](https://learning-rust.github.io/docs/b2.structs.html#Tuple-structs) site highlights the use-cases of tuple and unit structs.

- [This](https://doc.rust-lang.org/stable/rust-by-example/custom_types/structs.html) chapter of _Rust By Example_ showcases some examples of tuple and unit structs.