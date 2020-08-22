# Design

## Goal

This concept exercise should convey a basic understanding of the various `string formatting` methods in python.

## Things to teach

- Types of string formatting, and [when each is best used](https://realpython.com/python-string-formatting/#which-string-formatting-method-should-you-use) and [deferring i18n use cases for f-strings](https://www.python.org/dev/peps/pep-0501/#deferring-consideration-of-possible-use-in-i18n-use-cases).
- "New Style" formatting with [`str.format()`](https://docs.python.org/3/library/string.html#format-string-syntax)
- String interpolation, or ["f-strings"](https://docs.python.org/3/reference/lexical_analysis.html#formatted-string-literals)
- Format specification ["mini-language"](https://docs.python.org/3/library/string.html#format-specification-mini-language) e.g. formatting an int variable as a hexadecimal string, truncating long strings etc

## Things not to teach

- Translation -- internationalization and localization of strings and string formats
- `str.maketrans` and [`str.translate`](https://docs.python.org/3/library/stdtypes.html#str.translate)
- `gettext`

## Concepts

- new style string formatting with `str.format()`
- string interpolation via f-strings

## Prerequisites

- `basics`
- `strings`

### After

Deeper discussions of pros and cons of each method. Security implications of string interpolation and user input. Unicode, internationalization and localization considerations and organization.

["Old Style"](https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting) formatting with the `%` operator

Template strings

## Representer

No changes required.

## Analyzer

Check if the student is using `f-strings` or `.format`, because we want the student not to use the `%` operand and `Template` strings.

## Implementing

Tests should be written using `unittest.TestCase` and the test file named bool_basic_test.py.

## Help

If you have any questions while implementing the exercise, please post the questions as comments in this issue.

## Edits
