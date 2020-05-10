Using pattern guards is not the only method of pattern matching. Two other methods of pattern matching will be mentioned:

### Case/of expressions
The `case`/`of` expressions are very similar to `switch` statements in other languages. `case`/`of` expressions have the following general form:

```purescript
case value of
  pattern -> result
  ...
  pattern -> result
```
