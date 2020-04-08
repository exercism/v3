A floating-point number is a number with zero or more digits behind the decimal separator. Examples are `-2.4`, `0.1`, `3.14`, `16.984025` and `1024.0`.

Different floating-point types can store different numbers of digits after the digit separator - this is referred to as its precision.

Golang has 2 floating-point types:

- `float32`: all IEEE-754 32-bit floating-point numbers. Written as `2.45`.
- `float64`: all IEEE-754 64-bit floating-point numbers. Written as `2.45`.

See [Go builtin type declarations][go-builtins].

As can be seen, each type can store a different number of digits. This means that trying to store PI in a `float32` will only store the first 6 to 9 digits (with the last digit being rounded).

In this exercise you may also want to use a loop.
Go has only one looping construct, the [for loop][go-loop].

[go-builtins]: https://github.com/golang/go/blob/master/src/builtin/builtin.go
[go-loop]: https://tour.golang.org/flowcontrol/1
