[Example Implementation](https://github.com/exercism/python/blob/master/exercises/rna-transcription/example.py):

```python

#re-written to only use python 3 and omit the `import sys` of the example

DNA_TO_RNA = str.maketrans("AGCT", "UCGA")

def to_rna(dna_strand):
    return dna_strand.translate(DNA_TO_RNA)

```

## General

-  `static` methods :  Seperate from built-in methods, instance methods, and class methods.  A method bound to a class rather than an instance and called _without_ an object for a given class. The example solution for this exercise uses the `static` `str` method `maketrans`.
-  `str.maketrans()`:  A static method of `str` that returns a dictionary (_translation table_) usable for `str.translate()`.  Takes as input either a dictionary or strings of equal length.  The example solution for this exercise uses `str.maketrans()` with a two-string argument.
-  dictionaries:  Mapping type that has key-value pairs.  Returned by `str.maketrans` in the example code.  Also one of the argument types accepted by `str.maketrans()`.
-  `str.translate()`:  An _instance method_ called on an object from the `str` class (e.g. `<my string>`.translate()).  Returns a copy of the inital string with each character re-mapped through the given _translation table_.  The _translation table_ is typically a mapping or sequence type that implements indexing via the magic method `__getitem__()`.
-  function:  A named (_and often reusable_) section of code that performs a specific task.  It may or may not have _arguments_ passed in, and may or may not _return_ data.
-  function arguments:  Parameters passed into a function.  In python, these are noted in the `()` following a function name.  The example code uses a function named `to_rna()` with an argument of `dna_strand`.
- `def` keyword:  Used in python to denote a function definition.
- `return` keyword : Used in a _return statement_ at the end of a function.  Exits a function and may or may not pass data or an expression back to calling code.  Functions in python without an expicit `return` keyword and statment will return (pass back) the singleton object `none`.  The example code _returns_ a copy of the passed-in argument (assumed to be a string) that has been mapped through `str.translate()`, using the table made from `str.maketrans()`
