## 1. Define the expected oven time in minutes

- A global, named function is defined with
  [`defun`][cookbook-functions].
- A function "returns" (e.g. evaluates to) the value of the last
  expression in its body.
- A function definition can include an optional documentation string
  after the parameters list.

## 2. Calculate the remaining oven time in minutes

- You will need to write a [function][cookbook-functions] with a
  single parameter.
- You can use [`-`][hyper-subtract] to subtract numbers.

* You can invoke another function by putting the function name as the
  first element of an expression (_e.g._ `(func)`).

## 3. Calculate the preparation time in minutes

- You need to define a [function][cookbook-functions] with a single
  parameter
- You can use [`*`][hyper-multiply] to multiple numbers.

## 4. Calculate the elapsed time in minutes

- You need to define a [function][cookbook-functions] with two
  parameters.
- You can use [`+`][hyper-plus] to add numbers.
- You can call another function with parameters by adding those
  parameters to the expression where you call the function (_e.g._
  `(func x y z)`).

[cookbook-functions]: https://lispcookbook.github.io/cl-cookbook/functions.html
[hyper-multiply]: http://www.lispworks.com/documentation/HyperSpec/Body/f_st.htm
[hyper-plus]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pl.htm
[hyper-subtract]: http://www.lispworks.com/documentation/HyperSpec/Body/f__.htm
