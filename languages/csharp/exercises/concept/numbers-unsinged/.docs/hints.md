# Hints

### General

- The ranges integral numeric types can represent can be found in [Microsoft's documentation][docs.microsoft.com-integral].

### 1. Convert meters into light minutes

- When a calculation only uses integral numbers the result will always be an integral number. Use implicit conversion to get a floating-point number.

### 2. Convert light minutes into meters

- When a calculation uses both integral numbers and floating-point numbers, the result will always be a floating-point number. Use explicit conversion to get an integral number.
- When a number type reaches it's range limit it can throw an `OverflowException`.
