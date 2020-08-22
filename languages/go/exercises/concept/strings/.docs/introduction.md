A `string` in Go is an immutable sequence of bytes. Strings may contain arbitrary data, including bytes with values 0, but usually they contain human-readable text.
Text strings are conventionally interpreted as UTF-8 encoded sequence of Unicode code points (runes) which will be explained in a future exercise.
A `string` value can be written as a string literal, a sequence of bytes enclosed in double quotes:

```go
s := "Hello World!"
```

In Go floating point values are convenietly printed with Printf's verbs: %g (compact representation), %e (exponent) or %f (non exponent). All three verbs allow field width and numberic position to be controlled.

```go
f := 4.3242
fmt.Printf("%.4", f)
// Output: 4.32
```

The [official documentation][official documentation] provides a deep dive into this topic.

[official documentation]: https://blog.golang.org/strings
