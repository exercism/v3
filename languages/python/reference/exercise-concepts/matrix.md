[Example Implementation](https://github.com/exercism/python/blob/master/exercises/matrix/example.py):

[Alternate Implementation](https://exercism.io/tracks/python/exercises/matrix/solutions/e5004e990ddc4582a50ecc1f660c31df)

[Extended Implmentation](https://exercism.io/tracks/python/exercises/matrix/solutions/b6a3486a35c14372b64fdc35e7c6f98f)


 ## General  

  - classes:  the exercise objective is to define a `matrix` type.  Tested methods are linked to a `matrix` class
  - objects:  creating different instances with different data representing different `matrices` is tested
  - constructor:  customizing object initalization with actions and persisting data.  The example uses a constructor to process the passed in data into a list of lists assigned to an instance property
  - magic methods: the example uses the `__init__` magic method as its constructor for the class 
  - return values: "row" and "column" list values are expected from defined instance method(s)
  - `self`:  the example uses this keyword for methods and properties linked to a specific instance of the class  
  - namespaces:  knowing to use `self`.`<propertyname>` for instance properties and `self` as first argument to instance methods in a class
  - instance methods: tests for this exercises require one or more instance methods that will return a specified row or column list of the `matrix`. 
  - instance properties:  this exercise rquires one or more instance properties to persist passed in data.
  - mutability: in the extended example, knowing there are no protected or private properties in python and adjusting coding patterns
  - assignment:  instance properties need to be assigned passed in data
  - method arguments:  the methods returning "row" and "column" need to take both `self` and an integer as arguments
  - lists:  this exercise requires "row" or "column" be returnd as a `list`. A `list` of `lists` is also the reccommended way to process and store the passed-in data.
  - indexing: the "rows" and "columns" of this exercise need to be retrieved from a list of lists via index 
  - bracket notation: knowing that `[]` should be used to refer to a value at a specific index in a list
  - slicing:  the extended solution to this exercise can employ a slice (returns a copy) instead of calling `.copy()`.
  - iteration:  the passed-in string is iterated over, and split into rows.  The row lists are iterated over and split into elements
  - iterables:  understanding that strings, lists, and other data structures can be iterated over in the same fashion
  - iterators:  the example solution for this exercise uses `zip()`, which returns an _iterator_.
  - for loop: iterating over the passed in `matrix` string using a `for` loop to extract "rows" and "columns" that are appended to a list
  - comprehension syntax:  knowing that this is equivelent to a `for loop` - putting the row or column creation code _inside_ the list literal instead of using loop + append.
  - zip built-in:  the example solution for this exercise uses this function to aggregage the column-wise elements of each rown list to form the matrix "columns".

  - argument unpacking:  the example solution for this exercise uses `splat` (`*`) to unpack rows for the `zip()` function. 
  -`str.split`:  the example uses `.split` with and without seperators to break the passed in string into "rows" and then "elements"
  - type conversion:  the passed in data is in `str` format but the output is expected as a list of type `int`.
  - `int()`:  the example convertes the parsed `str` elements into `int`
