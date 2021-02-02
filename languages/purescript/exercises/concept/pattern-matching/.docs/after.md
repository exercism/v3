Using pattern guards is not the only method of pattern matching. Two other methods of pattern matching will be mentioned:

### Case/of expressions

The `case`/`of` expressions are very similar to `switch` statements in other languages. `case`/`of` expressions have the following general form:

```purescript
case value of
  pattern -> result
  pattern -> result
  ...
```

For example, if we wanted to complete this exercise using `case` expressions (excluding the "less than 41" and "greater than 43" cases):

```purescript
reply :: Int -> String
reply guess = case guess of
  41 -> "So close"
  43 -> "So close"
  42 -> "Correct"
```

To _exhaustively check_ using `case` expressions, you can use the _wildcard pattern_:

```purescript
reply :: Int -> String
reply guess = case guess of
  41 -> "So close"
  43 -> "So close"
  42 -> "Correct"
  _ -> "Not close"
```

### Function pattern matching

We can also pattern match the arguments of functions, and it has the following general form:

```purescript
foo :: Int -> String
foo pattern = result
foo pattern = result
...
```

For example, if we wanted to complete this exercise using function pattern matching (excluding the "less than 41" and "greater than 43" cases):

```purescript
reply :: Int -> String
reply 41 = "So close"
reply 43 = "So close"
reply 42 = "Correct"
```

To _exhaustively check_ using function pattern matching, you can use the _wildcard pattern_:

```purescript
reply :: Int -> String
reply 41 = "So close"
reply 43 = "So close"
reply 42 = "Correct"
reply _ = "Not close"
```

### Limitations

In both of these other methods of pattern matching, I have left out the "less than 41" and "greater than 43" cases of this exercise. This is because these other methods have limitations.

You can not (nicely) pattern match for "greater than a value" or "less than a value" using these methods. For example, this will throw an error:

```purescript
isGreaterThanOne :: Int -> True
isGreaterThanOne x = case x of
  (> 1) = True
  _ = False

-- Unexpected token '>'
```
