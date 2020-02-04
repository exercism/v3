[Example Implementation](https://github.com/exercism/python/blob/master/exercises/rna-transcription/example.py):

```python

#re-written to only use python 3 and omit the `import sys` of the example

DNS_TO_RNA = maketrans("AGCT", "UCGA")

def to_rna(dna_strand):
    return dna_strand.translate(DNA_TO_RNA)

```

## General

-  `static` methods :  Seperate from built-in methods and class methods.  A method bound to a class rather than an instance and called _without_ an object for a given class. The example solution for this exercise uses the `static` `str` method `maketrans`.
-  `str.maketrans()`:  A static method of `str` that returns a _translation table_ usable for `str.translate()`.  Takes either a dictionary or strings of equal length.  The example solution for this exercise uses `maketrans()` with a two-string argument.
-  dictionaries:  Mapping type that has key-value pairs.  One of the argument types accepted by `str.maketransk()`.
-  `str.translate()`:  
