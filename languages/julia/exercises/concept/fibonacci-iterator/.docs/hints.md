## General

## 2. Implement `iterate` methods

- The `state` argument doesn't have to be an integer. It can be of any type.
- Consider which state you need to calculate the next element. That should be the value of the `state` argument.
- The `iterate` method returns a tuple with two values `(i, state)`. `i` is the Fibonacci number, i.e. the `i` in `for i in Fib(10)`.

## 3. Define the optional methods that make `collect` work

- Work out which methods you need to implement by running `collect(Fib(10))` in the REPL and reading the error messages carefully.
