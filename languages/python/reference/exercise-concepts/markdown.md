# Concepts for markdown

[Example Implementation](https://github.com/exercism/python/blob/master/exercises/markdown/example.py)

## General  


- functions: This exercise requires a re-write of pre-esiting code that uses functions to parse passed-in text in _markdown_.  Tests expect a function named `parse` that can be called to transform the _markdown_ formatted text and return HTML formatted text.

- function arguments:  The example solution uses multiple functions that take various function arguments to operate on different pieces of the passed in markdown string.

- `re` module:  Both the original code to be refactored for this exercise and the example solution import and use the `re` module for Regular Expressions in python.

- `import` key word/ importing:  Both the original code to be refactored for the exercise and the example solution use the `import` keyword to import the `re` module in support of Regular Expressions in python.

- `str.format()` :  The example solution uses `str.split()` to break the passed in markdown string into a list of lines broken up by the `\n` character.

- `re.match()`:  Method of the `re` class that returns a `match` object with any matched values from a specified Regualr Expression or pre-compliled Regular Expression.  The example uses `re.match()` in multiple places to search for text patterns that need re-formatting or subsitituting. 

- regular expressions:  As a seperate language with a seperate interpreter that is often embeded inside another programming language.  Like many other programming lanagues in use, python supports the Regular Expression language and implements a Regular Expression interpreter in the core `re` module.  Both the original code to be refactored for this exercise and the example solution use the core `re` module to access Regualr Expression functionality.

- `return` keyword:  Most of the functions in the example solution specify a _return_ value using the `return` keyword.

- `None`:  None (null) type.  Used to define a null variable or an object.  It is in of itself a singleton in any given python program.  The example uses `None` as an expecit return value in several functions.

- booleans:  True and False of type `bopl`.  The example solution uses `True` and `False` as return values from functions that test membership in a list of values.

- assignment:  The example solution uses assingment for variables and other values.

- `re.sub()`:  Metho of the `re` class that replaces a Regular Expression match with a new value.  The example solution uses this method in various places to susititue _markdown_ syntax for _HTML_ syntax in the passed-in markdown text.

- dictionaries:  Mapping type.  The example solution employes a dictionary to return values from the `parse_line()` function.

- `str.split()`:  String method.  The example solution uses `str.split()` to split the incoming string into lines (_based on `\n`_), so that the text can be more easily processed for output.

- for loops:  The example solution uses `for` loops to iterate over various function inputs.

- iteration/iterable:  The example solution uses the `for _ in _` syntax to iterate over a list of lines.  This is possible because a list is an `iterable`.

- conditionals:  The example solution uses `if` to check for pattern matching and membershihp conditions in differnt functions for processing different markdown patterns.

- `match.group`:  A retun object of the `re.match()` method.  Returned when there are one or more Regular Expression groups specified in the RE pattern.

- while loop:  Indefinite iteration with or without a conditional.  A loop that continues unless an explicit break or condition is met.  The example uses `while` loops to continually check if certain formatting conditions have been completed or met before proceeding to compliling the final returned result.

- lists:  The example uses lists in several places to hold text to be processed or searched - or for tracking the state of pieces of the passed-in text.

- `range()` built-in: built-in type: Represents an immutable sequence of numbers (or any object that implements the __index__ magic method).  Used in the example to control the number of loops while iterating through a passed-in line or list.
