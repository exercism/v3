[Example Implementation](https://github.com/exercism/python/blob/master/exercises/matrix/example.py):
```python
class Matrix:
'''
Row and Column representation is calulated on instantiation.
'''
    def __init__(self, line):
        self.rows = [[int(element) for element in row.split()]
                      for row in line.split('\n')]
        self.columns = [list(row) for row in zip(*self.rows)]

    def row(self, index):
        return self.rows[index - 1]

    def column(self, index):
        return self.columns[index - 1]
  ```
 
 **Alternate Implementation**:
 ```python
 class Matrix:
 '''
 Row represnetation is calculated on instantiation, columns are calculated by calling the column method.
 '''
    def __init__(self, line):
        self.rows = [[int(element) for element in row.split()]
                      for row in line.split('\n')]

    def row(self, index):
        return self.rows[index-1]

    def column(self, index):
        return [row[index-1] for row in self.rows]
  ```
  
  **Extended Implmentation**
  ```python
  class Matrix:
  '''
  Methods return row/column copies to protect against unintended data mutation.
  '''
  
    def __init__(self, line):
        self.rows = [[int(element) for element in row.split()]
                      for row in line.split('\n')]

    def row(self, index):
        return self.rows[index-1].copy() #can also use a slice:  self.rows[index-1][:]

    def column(self, index):
        return [row[index-1] for row in self.rows].copy() #can also use a slice:  [row[index-1] for row in self.rows][:]
 ```
 
 
 ## Concepts  
 
 
  - **Objects** in Python: _just about everything's an object_.
  - **Classes**:  bundle data and functions together to create a new _type_.  The objective of this exercise is to define a class that represents a `matrix` type with "rows" and "columns". 
  - **Objects(_instances_)** of a class:  created/customized from a class. Different instances with different data are tested for this exercise.
  - **Initializer** (_constructor_):  special function (`__init__`) that creates/inializes instances (objects). This exercise requires a constructor, so that a string passed in can be transformed into a representation of a `matrix`, with "row" and "column" properties.
      - These run on object initialization
      - These can define or set properties/data members for individual objects
  
  - **`self`**:  used as as a keyword reference to an instance/object created from a class.  Required as a first argument for methods/functions intended to operate on an individual object (including/especially `__init__`.)
  - **Instance (_object_) properties**:  it is expected that each object made from this class will contain one or more properties/data members that represent a `matrix`.
      - `self` is used to denote a property tied to an instance (object).
      - properties for an instance (object) are particular to that object, and can be different from other instances.

  - **Instance methods**(_functions_) inside a class:  this exercises requires one or more instance methods that will return a specified row or column of the `matrix` built from the passed-in source string. 
      - Run when called (`<objectname>`.`method`())
      - Requires an instance/object reference  (`self`) as a first argument
      - _internally_, the `self` keyword is required.  _externally_, it is resolved through `<objectname>`.`<methodname>`() 
      - As with non-class functions, will return `None` if no `return` value  is specified 

  - **Property (_attribute_) access**
      -  _*externally*_, `<objectname>`.`<propertyname>`
      -  _internally_, `self`(_instance reference_).`<propertyname>`

  - **Property (_attribute_) mutability**
      - there are no protected or private properties in python, only obfuscated ones
      - coding patterns need to account for the mutability of properties

  - **List Data Structure**:  this exercise requires the "row" or "column" requested be returnd as a _list_, and a _list of lists_ is the reccommended way to process and store the provided source strings.
      - mutable 
      - collection of pointers to underlying (possibly) heterogeneous data
      - can containe other lists or additional nested data structures
      - iterable, sortable
  -  **List Indexing**:  efficient way to retieve items at known postitions in a list.  The "rows" and "columns" of this exercise are easily retrieved from a list of lists via index.
      - bi-directional
      - zero-indexed from left, -1 indexed from right
      - bracket notation
  - **List Slicing**:  quick way to copy a range or sub-sequence of a list.  The extended solution to this exercise can employ a slice instead of calling `.copy()`.
      - slicing makes a _copy_
      - bracket notation [`start` : `stop` : `step`]
      
  - **Comprehension Syntax** for Building Basic Data Structures:  while not strictly required, using a _nested list comprehension_ is considered the most compact way of processing and storing the passed in string for this exercise.
      - Replaces the basic `for` loop in constructing/filling certain data structures (list, dict, set)
      - Uses different orderling than a loop, "flattens" the loop appearance in code
      - Code that "builds" or "fills" the data structure is placed _inside_ the literal for that data structure
      - Evaulates differently than a `for` loop, but has the same Big-O
      - Can contain conditional logic
      - Can be nested, but cautions for radability and performance apply
      
  - **Iterators**:  the example solution for this exercise uses `zip()`, which returns an _iterator_.  There are other ways to assemble the "columns", but `zip()` is considered a very popular and efficent way of doing so.
    - one-directional
    - implements `__iter__` and returns `self`
    - implements `__next__` and retruns `self`
    
  - **zip(_*iterables_) built-in**:  the example solution for this exercise uses this function to "zip together" the column-wise elements of each rown list to form the matrix "columns".
    - returns an `iterator` of tuples (e.g. a tuple of first elemenst, a tuple of second elements, etc.)
    - "glues together"/aggregates elements from the input iterables
    - stops when the _shortest_ iterable is exhausted
    
  - **`splat` (`*` and `**`) for Argument Unpacking**:  the example solution for this exercises _unpacks_ the row-lists into the `zip()` function.  While it is possible to do this in a seperate step, usinkg the `*` shortens and unclutters code.
    - "unwraps" or "flattens" a variable set/list/tuple/dict of arguments passed to a function
    - `*` for non-keyworded arguments, `**` for keyworded arguments
    
- **`str.split(_sep-None, maxsplit=-1_)**:  efficient, core python method of breaking apart this exercises passed-in matrix string.  While RegEx could be used for this exercise it would require an extra import, and less readable code.
    - returns a list of strings, compiled by using `sep` as the delimiter
    - If no `sep` is indicated, _consecutive whitespace_ is used as `sep`
    
- **Converting a string data type to a number with `int()`**:  the input to the exercise is a string, but the output rquires lists of numbers.  `int()` is used to convert from the string data type to integer data type.
