Two common types of numbers in Ruby are:

- Integers: numbers with no digits behind the decimal separator (whole numbers). Examples are `-6`, `0`, `1`, `25`, `976` and `500000`.
- Floating-point numbers: numbers with zero or more digits behind the decimal separator. Examples are `-2.4`, `0.1`, `3.14`, `16.984025` and `1024.0`.

They are implemented through the `Integer` and `Float` classes.

The `Float` and `Integer` classes have methods that will coerce values from one to the other. `Integer` numbers are precise to a whole unit, while `Float` has precision that is fractional to an whole number. This means that coercing a float to an integer may result in loss of precision.

In this exercise you must conditionally execute logic. A common way to do this in Ruby is by using an `if/else` statement:

```ruby
x = 5

if x == 5
  # Execute logic if x equals 5
elsif x > 7
  # Execute logic if x greater than 7
else
  # Execute logic in all other cases
end
```

The condition of an `if` statement does not have to be only `true` or `false`. It can be any value. But it's important to know that any value other than `nil` and `false` (_truthy_ values) will be treated as `true`, meaning that the code inside the `if` statement will be executed.
