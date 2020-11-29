## Numbers
There are three different kinds of built-in numbers: integers, floating-point, and complex. In this exercise you will only be dealing with ints and floats.

### ints
`ints` are whole numbers. e.g. `1234`, `-10`, `20201278`.

### floats
`floats` are numbers containing a decimal point. e.g. `0.0`,`3.14`,`-9.01`.


You can see more detail the following resources:
- [Python numeric type documentation](https://docs.python.org/3/library/stdtypes.html#typesnumeric)
- [Documentation for `int()` built in](https://docs.python.org/3/library/functions.html#int)
- [Documentation for `float()` built in](https://docs.python.org/3/library/functions.html#float)


## Arithmetic
Python fully supports arithmetic between `ints` and `floats`. It will convert narrower numbers to match their less narrow counterparts when used with the binary arithmetic operators (`+`, `-`, `*`, `/`, `//`, and `%`). When division with `/`, `//` returns the quotient and `%` returns the remainder.

Python considers `ints` narrower than `floats`. So, using a float in an expression ensures the result will be a float too. However, when doing division, the result will always be a float, even if only integers are used.

```python
#the int is widened to a float here, and a float is returned
>>> 3 + 4.0
7.0
>>> 3 * 4.0
12.0
>>> 3 - 2.0
1.0
#division always returns a float
>>> 6 / 2
3.0
>>> 7 / 4
1.75
```

If an int result is needed, you can use `//` to truncate the result.

```python
>>> 6 // 2
3
>>> 7 // 4
1
```

To convert a float to an integer, you can use `int()`. Also, to convert an integer to a float, you can use `float()`.
```python
>>> int(6 / 2)
3
>>> float(1 + 2)
3.0
```