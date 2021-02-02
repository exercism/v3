An `if/else` expression can be used to conditionally execute logic. PureScript also has another, more powerful way to conditionally execute logic: pattern matching. With pattern matching, a value can be tested against one or more _patterns_. An example of such a pattern is matching a value against a constant (e.g. `1` or `"hello"`).

In Purescript, pattern matching is done through the use of `guards`:

```purescript
describe number
  | number == 0 = "Zero"
  | number == 1 = "One"
```

**Note:** Notice the lack of an `=` sign after `describe number`.

This may look similar to `switch` statements in other languages.

In some cases, you may want to add an additional condition to a pattern. This is knows as a _guard_ (clause):

```purescript
describe number
  | number == 0 = "Zero"
  | number == 1 = "One"
  | number == 2 = "Two" -- Extra guard clause
```

In the above example, not all possible input will have a matching pattern. The compiler will detect this and output an error. This is known as _exhaustive checking_. To solve the error, one has to handle all cases. For this, the _wildcard pattern_ can be used, which is a pattern that matches on any value:

```purescript
describe number
  | number < 0 = "Negative number"
  | _ = "Positive number"

// No compiler error
```

Alternatively, you can use the `otherwise` keyword:

```purescript
describe number
  | number < 0 = "Negative number"
  | otherwise = "Positive number"

// No compiler error
```

Pattern matching will test a value against each pattern from top to bottom, until it finds a matching pattern and executes the logic associated with that pattern. The order of patterns matters!
