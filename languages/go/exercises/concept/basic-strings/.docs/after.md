# After

### Slice indexes

When addressing an index or index range in a slice make sure it exists.

Code that does not check if an index exists in a `slice`, will `panic` if that index does not exist.
A `panic` should be considered a bug in the code to be fixed. There will be more on panics later.

### Source code encoding

Source code files in Go are always `UTF-8` encoded. This makes it very easy to use special characters in strings.
