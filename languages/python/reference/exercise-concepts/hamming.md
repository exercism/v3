[Example Solution](https://github.com/exercism/python/blob/master/exercises/hamming/example.py)

```
def distance(s1, s2):
    if len(s1) != len(s2):
        raise ValueError("Sequences not of equal length.")

    return sum(a != b for a, b in zip(s1, s2))
```
    
## Concepts

### Data types

**function**
 - functions are defined and named using the `def` keyword
 - functions take named arguments which are accessible within the body of the function; this one requires the student to make a function that accepts 2
 - the function must return a number (int)

**string** 
 - strings have a length, accessible by calling `len()`, a builtin python function
 - strings are iterable, which provides a lot of opportunity to leverage Python functions against them
 - strings are immutable (*immutability*)

**boolean**
 - this solution creates a list of boolean (`True`/`False`) values by comparing `a` to `b`.
 - Booleans also can be added together, since they also evaluate to 0 or 1, i.e.  `assert True == 1` will return True
 
**iterables**
 - `zip()` in this solution creates an iterable, which is iterated over by using the `for ... in ` syntax
 - `zip()` and `sum()` are both examples of built in Python functions that operate on iterables
  - the values in an iterable can be unpacked into variables and used, i.e. `for a, b in zip(s1, s2)`

**Exceptions**
 - the student is required to raise an `Exception` for incorrect input
 - the idiomatic `Exception` type is a `ValueError`, meaning the input is incorrect
 - `Exceptions` can be caught from outside the scope where they are raised, using the `try/except` syntax
 - Custom error messages can (and should) be supplied to an Exception when raised
 - All `Exceptions` types inherit from the base class, `Exception` and thus can be caught by either checking specifically for the 
   type of Exception, or for any Exception
   

### Operators

**equality**
 - `!=` is "not equal", which is not the same thing as `is`, or an identity check
 - `==` is equal


### Concepts

**loops, iteration**
 - the `for ... in` syntax is useful for looping through a list or other iterable object
 
**generator expression**
 - generators calculate then `yield` a value one at a time, as opposed to lists which calculate and return all values in memory at once.
 - a generator will pick up where it leaves off, and generate one item at a time, on demand
 - this expression is an iterable, meaning it can be passed directly to the `sum()` function without storing it in a list first

 
 
 
