### General

- [Floating-point numeric types introduction][docs.microsoft.com-floating_point_numeric_types].

### 1. Calculate the interest rate

- See [Go builtin type declarations][go-builtins] for the `float64` type specification.

### 2. Calculate the annual balance update

- When calculating the annual yield, it might be useful to temporarily convert a negative balance to a positive one. One could use arithmetic here.

### 3. Calculate the years before reaching the desired balance

- To calculate the years, one can keep looping until the desired balance is reached. Go has only one looping construct, the [for loop][go-loop].

[go-builtins]: https://github.com/golang/go/blob/master/src/builtin/builtin.go
[go-loop]: https://tour.golang.org/flowcontrol/1
