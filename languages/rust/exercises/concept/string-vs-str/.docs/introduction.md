`String` is a (mutable) collection of well-formed utf8 chars. (In Java or C#, this type is called `StringBuilder`.)

`&str` is a read only view of a well-formed utf8 sequence.

Strings and &str in rust can not be indexed into like you might in other languages.
For example this will not work:

```
let hello = "Hello";
println!("First letter = {}", hello[0]);
```

Rather than addressing the string in terms of individual bytes we work with discrete unicode characters,
so for example we could achieve the above result in rust like this:

```
let hello = "Hello";
println!("First letter = {}", hello.chars().first().unwrap());
```
