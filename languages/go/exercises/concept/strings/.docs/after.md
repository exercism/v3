# After

Using  `strings.Split` returns a `[]string` (spoken: Slice of string) which contains at least one element.
When having to address the second element in that slice (`parts[1]`) its best to make sure it exists.

Code that does not check if an index exists in a `slice` before accessing it, will `panic` if that index does not exist.
A `panic` should be considered a bug in the code to be fixed. There will be more on panics later.
