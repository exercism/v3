In Go a `string` value can be written as a string literal, a sequence of bytes enclosed in double quotes:

```go
s := "Hello World!"
```

The "+" operator makes a new string by concatenating two strings:

```go
fmt.Println("Hello" + " world!")
```

String values are immutable: the byte sequence contained in a str ing value can never be changed, thoug h of course we can assig n a new value to a str ing variable.
To append one string to another, we can use the `+=` statement :

```go
s := "Hello"
s += " word."
fmt.Println(s)
// Output: Hello world.
```
