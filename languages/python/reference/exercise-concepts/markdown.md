# Concepts for markdown

[Example Implementation (_not ideal_)](https://github.com/exercism/python/blob/master/exercises/markdown/example.py)  

[Alternate Example (_Regex only_)](https://exercism.io/tracks/python/exercises/markdown/solutions/daf30e5227414a61a00bac391ee2bd79)  

[Alternate Exmaple (_Python with Regex_)](https://exercism.io/tracks/python/exercises/markdown/solutions/a1f1d7b60bfc42818b2c2225fe0f8d7a)  


## General  
-refactor: Reviewing and rewriting (or re-organizing) code for clarity and efficiency. This exercise requires a re-write of pre-existing code that uses functions to parse passed-in text in markdown.

- functions: Tests for this exercise expect a function named `parse` that can be called to transform the _markdown_ formatted text and return HTML formatted text.

- function arguments:  The example solutions use functions that take function arguments to operate on passed in markdown strings.

- `re` module:  Both the original code to be refactored for this exercise and the example solution import and use the `re` module for Regular Expressions in python.

- `import` key word/ importing:  Both the original code to be refactored for the exercise and the example solution use the `import` keyword to import the `re` module in support of Regular Expressions in python.

- `str.splitlines()`/`str.split()` :  The example solution uses `str.split()` to break the passed in markdown string into a list of lines broken up by the `\n` character.  The alternate Python example solution uses `str.splitlines() for the same effect across all line end characters.

- `re.match()`:  Method of the `re` class that returns a `match` object with any matched values from a specified Regular Expression or pre-compliled Regular Expression.  The example uses `re.match()` in multiple places to search for text patterns that need re-formatting or subsitituting. 

- regular expressions:  A Domain Specific Language (DSL) for text processing. Like many other programming languages in use, python supports a quasi-dialect of PCRE (_Perl compatible regular expressions_).  `Regular expressions` can be used via the core python `re` module, or the third-party `regex` module.  Both the original code to be refactored for this exercise and the example solutions use the core `re` module to access `regular expressions` functionality.


- `return` keyword:  Most of the functions in the example solution specify a _return_ value using the `return` keyword.

- `None`:  Pythons null type, referred to when a null or "placeholder" is needed.  It is in and of itself a singleton in any given python program.

- booleans:  True and False of type `bopl`.  The example solution uses `True` and `False` as return values from functions that test membership in a list of values.

- assignment:  The example solution uses assignment for variables and other values.

- `re.sub()`:  function of the `re` module that replaces a `regular expression` match with a new value.  The example solutions use this function in various places to substitute _markdown_ syntax for _HTML_ syntax in the passed in markdown text.

- dictionaries:  Mapping type.  The example solution employes a dictionary to return values from the `parse_line()` function.

- for loops:  The example solution uses `for` loops to iterate over various function inputs.

- iteration/iterable:  The example solution uses the `for _ in _` syntax to iterate over a list of lines.  This is possible because a list is an `iterable`.

- conditionals:  The example solution uses `if` to check for pattern matching and membership conditions in different functions for processing different markdown patterns.

- `match.group`:  A return object of the `re.match()` function. Returns a `re.Match` _instance_ which in turn has a `Match.group` method. `Match.group` exists even if there are no groups specified in the pattern.  See [Match.group](https://docs.python.org/3/library/re.html#re.Match.group) in the standard docs for more detail.

- lists:  The example uses lists in several places to hold text to be processed or searched - or for tracking the state of pieces of the passed-in text.

- `range()` built-in: built-in type: Represents an immutable sequence of numbers (or any object that implements the __index__ magic method).  Used in the example to control the number of loops while iterating through a passed-in line or list.
