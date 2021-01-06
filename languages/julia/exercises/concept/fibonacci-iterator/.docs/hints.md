## General

## 2. Implement `iterate` methods

- The `state` argument doesn't have to be an integer. It can be of any type.
- Consider which state you need to calculate the next element. That should be the value of the `state` argument.
- The `iterate` method returns a tuple with two values `(item, state)`. `item` is the Fibonacci number, i.e. the `item` in `for item in Fib(10)`.
- If your `state` includes several values, consider using a named tuple to clear up the meaning of each value.

## 3. Define the optional methods that make `collect` work

- Work out which methods you need to implement by running `collect(Fib(10))` in the REPL and reading the error messages carefully.
