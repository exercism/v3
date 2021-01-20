We need a csv builder as defined by this standard: https://tools.ietf.org/html/rfc4180.

The builder needs to concatenate immutable string slices (`&str`) and when `build` is called, the builder will be consumed and will return the created csv string.

```rust
let mut builder = ListBuilder::new();

builder.add("ant");
builder.add("ba\"t");
builder.add("cat");

let list = builder.build();
//Note that from now on we can do nothing now with builder.
assert_eq!("ant,\"ba\"t\",cat", &list);
```

## Edge cases

Handling edge cases nicely while creating CSV makes the world a better place. Typically the following is the best way to escape the data:

Fields containing line breaks, double quotes, and commas should be enclosed in double-quotes.

If double-quotes are used to enclose fields, then a double-quote appearing inside a field must be escaped by preceding it with another double quote.
