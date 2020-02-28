# Hints

### General

- The [Python documentation for `str`][https://docs.python.org/3.7/library/stdtypes.html#text-sequence-type-str] is an overview of the Python `str` type.

### 1. Get message from a log line

 - Strings in Python have lots of convenient instance methods, like [`split()`][https://docs.python.org/3/library/stdtypes.html#str.split]
 - There are also [methods to assist handling extra whitespace][https://docs.python.org/3.7/library/stdtypes.html#str.strip] on the ends of a string.

### 2. Get log level from a log line

 - Strings have methods that help [convert letters from lower to uppercase][https://docs.python.org/3/library/stdtypes.html#str.lower] and vice-versa.

### 3. Reformat a log line

Strings are immutable, but can be combined together to make new strings, or have elements replaced. This goal can be accomplished by using string methods, or operators like `+` or `+=` (which are overloaded to work with strings).
Python also has a concept of string formatting, like many other languages.

 - The [`str.join()`][https://docs.python.org/3/library/stdtypes.html#str.join] method is useful to concatenate an iterable of strings into one string.
 - [`str.format()`][https://docs.python.org/3/library/stdtypes.html#str.format] is an idiomatic way to do string interpolation in Python (inserting one or more string value(s) into another).
 - [Format strings][https://docs.python.org/3/library/string.html#formatstrings] are another convenient way to interpolate values into a string. This strategy is particularly useful when more than one named variable needs to be inserted into a final output.
