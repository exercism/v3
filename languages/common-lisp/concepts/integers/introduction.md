Like many languages Common Lisp contains integers. These are whole numbers without a decimal point (like `-6`, `0`, `25`, `1234`,
etc.)

Common Lisp defines no limits on the magnitude of integers. Integers can be arbitrarily large (or small if negative).

In general, if you are working with only whole numbers, you should prefer
integers as they don't suffer from the same loss of precision as floating-point
numbers do over many calculations.
