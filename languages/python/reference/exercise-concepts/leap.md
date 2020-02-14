# Concepts of `leap`

## Example Implementation

```python
def leap(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)
```

## Concepts

- Functions: the exercise relies on a `def` statement to create a named function
- Parameters: the exercise requires a single positional parameter in the function signature
- Return Values: the exercise must use a `return` statement to return a value to the caller
- Expressions: the exercise relies on writing an expression that will be evaluated to a return value
- Modular Division: the exercise relies on the `%` operator to check if one number is evenly divisible by another
- Boolean Operators: the exercise relies on `and`, `or`, and (optionally) `not` to form Boolean predicates
- Boolean Logic: the exercise relies on `and` and `or` to combine Boolean predicates into a single logical answer
- Comparision: the exercise relies on the `==` and `!=` operators to make binary comparisons between values
- Equivalence: the exercise relies on the `==` and `!=` operators to check that two values are equivalent (or not)
- Order of Evaluation: the exercise relies on parentheses to explicitly modify the normal order of evaluation of an expression
- Operator Precedence: the exercise is most simply stated when the student understands the operator precedence binding rules of Python
- Short-Circuiting: the exercise relies on short-circuiting to avoid unnecessary calculations
- Generics: the exercise is polymorphic across numerical types (ie int, float, Decimal)
- Duck Typing: the exercise supports any argument that supports modular division and comparison to integers (ie int, float, Decimal)
