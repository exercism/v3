## String Methods

A `str` in Python is a sequence of Unicode code points which
include letters, numbers, symbols, punctuation, etc. Strings
implement all of the [common sequence operations](https:/docs.python.org/3/library/stdtypes.html#typesseq-common),
along with iteration using the `for item in <string>` syntax.

Python provides a number of useful methods that you can use to manipulate
`strings`. These methods can be used for cleaning, splitting, translating,
or otherwise working with the str type. New strings can be created based
on method arguments, and/or additional information can be returned. Strings
can be concatenated using the `+` operator or with `str.join()`.

`Strings` are immutable (the value does not change), so each of these
methods will actually return a new instance of `str` instead of modifying
the existing one.
