# Introduction

### Strings

A `string` in Go is a sequence of `bytes`, which doesn't necessarily have to represent characters.
That being said, `UTF-8` is a central part of `strings` in Go. It is easy to convert a string to `runes` (`UTF-8` characters) or iterate over `runes` in a string.
This makes dealing with different languages or other special characters in Go very simple.
When dealing with `UTF-8` characters it is important to know, that not all characters have the same length.
`ASCII` characters have the length of one `byte`. Other characters can have up to 4 bytes.
Runes, bytes and their connection to strings will be handled more in-depth in a later exercise.

Since `strings` are based on `[]byte` there are some commonalities. For example you can get a `byte` at position `i` from a `string` with `s[i]`. Other things are different than with a `slice`. For example can a string **not** be changed with `s[i] = 'a'`.

[Strings, bytes, runes and characters in Go](https://blog.golang.org/strings) provides a deep dive into this topic.

### Conditionals

This exercise also introduces _conditionals_. Here is a little intro:
[Go by Example: If/Else](https://gobyexample.com/if-else)
