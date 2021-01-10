## string-formatting

The Python `str` build-in type can be initialized using two robust string formatting methods. The string interpolation with `f'{variable}'` should be preferred as it is very readable, complete, and very fast. When a more versatile approach is needed, `''.format()` allows creating almost all the `str` you need.

# literal string interpolation. f-string

Literal string interpolation is a way of quickly and efficiently formatting and evaluating expressions to `str` using the `f` prefix and the curly brace `{object}`. It can be used with all enclosing string types as: single quote `'`, double quote `"` and for multi-lines and escaping triple quotes `'''` or `"""`.

In this basic example of **f-string**, the variable `name` is rendered at the beginning of the string, and the variable `age` of type `int` is converted to `str` and rendered after `' is '`.

```python
>>> name, age = 'Artemis', 21
>>> f'{name} is {age} year old.'
'Artemis is 21 year old.'
```

The expressions evaluated can be almost anything, to mention some of the possibilities that can be evaluated: `str`, numbers, variables, arithmetic expressions, conditional expressions, built-in types, slices, functions or any objects with either `__str__` or `__repr__` methods defined. Some examples:

```python
>>> waves = {'water': 1, 'light': 3, 'sound': 5}

>>> f'"A dict can be represented with f-string: {waves}."'
'"A dict can be represented with f-string: {\'water\': 1, \'light\': 3, \'sound\': 5}."'

>>> f'Tenfold the value of "light" is {waves["light"]*10}.'
'Tenfold the value of "light" is 30.'
```

f-string supports control mechanisms such as width, alignment, precision that are described for `.format()`.

# str.format() method

The `str.format()` allow to replace placeholders with values, the placeholders are identified with named indexes {price} or numbered indexes {0} or empty placeholders {}. The values are specified as parameters in the `format` method. Example:

```python
>>> 'My text: {placeholder1} and {}.'.format(12, placeholder1='value1')
'My text: value1 and 12.'
```

Python `.format()` supports a whole range of [mini language specifier][format-mini-language] that can be used to align text, convert, etc.

The complex formatting specifier is `{[<name>][!<conversion>][:<format_specifier>]}`:

- `<name>` can be a named placeholder or a number or empty.
- `!<conversion>` is optional and should be one of the three: `!s` for [`str()`][str-conversion], `!r` for [`repr()`][repr-conversion] or `!a` for [`ascii()`][ascii-conversion]. By default, `str()` is used.
- `:<format_specifier>` is optional and has a lot of options, which we are [listed here][format-specifiers].

Example of conversions for a diacritical ascii letter:

```python
>>> '{0!s}'.format('ë')
'ë'
>>> '{0!r}'.format('ë')
"'ë'"
>>> '{0!a}'.format('ë')
"'\\xeb'"

>>> 'She said her name is not {} but {!r}.'.format('Anna', 'Zoë')
"She said her name is not Anna but 'Zoë'."
```

Example of format specifiers, [more examples at the end of this page][summary-string-format]:

```python
>>> "The number {0:d} has a representation in binary: '{0: >8b}'.".format(42)
"The number 42 has a representation in binary: '  101010'."
```

[all-about-formatting]: https://realpython.com/python-formatted-output
[difference-formatting]: https://realpython.com/python-string-formatting/#2-new-style-string-formatting-strformat
[printf-style-docs]: https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting
[tuples]: https://www.w3schools.com/python/python_tuples.asp
[format-mini-language]: https://docs.python.org/3/library/string.html#format-specification-mini-language
[str-conversion]: https://www.w3resource.com/python/built-in-function/str.php
[repr-conversion]: https://www.w3resource.com/python/built-in-function/repr.php
[ascii-conversion]: https://www.w3resource.com/python/built-in-function/ascii.php
[format-specifiers]: https://www.python.org/dev/peps/pep-3101/#standard-format-specifiers
[summary-string-format]: https://www.w3schools.com/python/ref_string_format.asp
[template-string]: https://docs.python.org/3/library/string.html#template-strings
