## 1. A Dough Ratio

In addition to the `+`, `*`, and `/` operators, it might be
helpful to know that Common Lisp has the constant `pi` that can be used to
calculate the crust length (as a circumference).

When it comes to rounding to the nearest gram, the `round` function may be worth
a look!

## 2. A Splash of Sauce

In addition to reusing `/` and `*`, you might find the `sqrt` function of some
help.

## 3. Some Cheese, Please

This one is a tad more complex and involves some squaring and cubing of
numbers. Luckily, the `expt` function should help in both of these cases.

When it comes to always rounding something down, you may want to take a look at
the `floor` or `truncate` functions. Unlike `round`, which simply rounds to the
nearest whole number, `floor` always towards negative infinity and `trunctate`
towards 0.

Either `floor` or `truncate` will work for this exercise as the tests only deal
in positive numbers (it doesn't make much sense to have a negative number of
pizzas!)

## 4. A Fair Share

This part requires using a function you may not have come across before, the
[modulo function](https://en.wikipedia.org/wiki/Modulo_operation) (`mod`). This
function gives you the remainder of the division between two numbers – for
example:

```lisp
(mod 10 3) ; => 1
```

This is because 3 goes 3 times into 10 and leaves 1 left over (3 \* 3 + 1 =
10). If a number is evenly divisible by another, then the remainder given by the
`mod` operation will be equal to 0. You might find the `=` function helpful in
testing for this.
