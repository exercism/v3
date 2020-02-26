# Python

Python is an interpreted, dynamically (but strongly) typed, and garbage-collected general programming language that has become extremely popular due to its readability, low barrier for entry, and exceptionally deep ecosystem of libraries and tools. Python is object-based, but is inherently multi-paradigm and has drawn together influences from a wide range of programming languages, including ABC, Haskell, Lisp, and Modula-3.

## Language-unique concepts

- [The Zen of Python](./concepts/zen_of_python.md)
- [Pythonic](./concepts/pythonic.md) code
- [Python Enhancement Proposals](./concepts/python_enhancement_proposals.md)
- [PEP 8](./concepts/pep_8_style_guide.md) style conventions

## Object-oriented concepts

- [Classes](../../../reference/concepts/classes.md)
- [Composition](../../../reference/concepts/composition.md)
- [Encapsulation](../../../reference/concepts/encapsulation.md)
- [Inheritance](../../../reference/concepts/inheritance.md)
- [Interfaces](../../../reference/concepts/interfaces.md)
- [Mutation](../../../reference/concepts/mutation.md)
- [Objects](../../../reference/concepts/objects.md)
- [Polymorphism](../../../reference/concepts/polymorphism.md)
- [State](../../../reference/concepts/state.md)

## Functional concepts

- [Anonymous functions](../../../reference/concepts/anonymous_functions.md) (Python's `lambda`s)
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Immutability](../../../reference/concepts/immutability.md) (of certain builtin primitives)
- [Nested functions](../../../reference/concepts/nested_functions.md)
- [Partial application](../../../reference/concepts/partial_application.md)
- [Pipelines](../../../reference/concepts/pipelines.md)
- [Pure functions](../../../reference/concepts/pure_functions.md)
- [Recursion](../../../reference/concepts/recursion.md)
- [REPL](../../../reference/concepts/repl.md)
- [Type inference](../../../reference/concepts/type_inference.md)

## General concepts

- [Arithmetic](../../../reference/concepts/arithmetic.md)
- [Bitwise manipulation](../../../reference/concepts/bitwise_manipulation.md)
- [Boolean logic](../../../reference/concepts/boolean_logic.md)
- [Comments](../../../reference/concepts/comments.md)
- [Conditionals](../../../reference/concepts/conditionals.md)
- [Enumeration](../../../reference/concepts/enumeration.md)
- [Functions](../../../reference/concepts/functions.md)
- [Generics](../../../reference/concepts/generics.md)
- [Locking](../../../reference/concepts/locking.md)
- [Loops](../../../reference/concepts/loops.md)
- [Methods](../../../reference/concepts/methods.md)
- [Scope](../../../reference/concepts/scope.md)
- [Variables](../../../reference/concepts/variables.md)

## Types

- [array](../../../reference/types/array.md)
- [bool](../../../reference/types/boolean.md)
- [bytes](../../../reference/types/bytes.md)
- [class](../../../reference/types/class.md)
- [float](../../../reference/types/floating_point_number.md)
- [decimal](../../../reference/types/decimal_number.md)
- [deque](../../../reference/types/deque.md)
- [dict](../../../reference/types/hash_map.md)
- [int](../../../reference/types/integer.md)
- [list](../../../reference/types/list.md)
- [None](../../../reference/types/null.md)
- [set](../../../reference/types/set.md)
- [str](../../../reference/types/string.md)
- [struct](../../../reference/types/struct.md)
- [tuple](../../../reference/types/tuple.md)

## Resources used

- https://www.python.org

## Extracted Concepts

### [Dictionary][dictionary]

- Mapping type. The example solution employes a dictionary to return values from the `parse_line()` function.
- mapping type that has key-value pairs. Returned by `str.maketrans` in the example code. Also one of the argument types accepted by `str.maketrans()`.
- the example uses a dictionary to map passed in move arguments to methods that perform the moves. The example also uses a dictionary/mapping created by calling `str.maketrans()`.

### [Method Arguments][method-arguments]

- Parameters passed into a function. In python, these are noted in the `()` following a function name. The example code uses a function named `to_rna()` with an argument of `dna_strand`.
- The example solutions use functions that take function arguments to operate on passed in markdown strings.
- the exercise requires a single positional parameter in the function signature
- concept over arguments of a function and how to use them is required
- the methods returning "row" and "column" need to take both `self` and an integer as arguments

### [Booleans][booleans]

- this solution uses Boolean values (`True` / `False`)
- True and False of type `bopl`. The example solution uses `True` and `False` as return values from functions that test membership in a list of values.

### [Bracket Notation][bracket-notation]

- knowing that `[]` should be used to refer to a value at a specific index in a list

### [String formatting][string-formatting]

- How to format strings, ie `%` operator, `str.format`, f-strings

### [Inheritance][inheritance]

- The default `__str___` method is inherited from `Object`, which every class in Python inherits from. (See: inheritance)
- a "subclass" will inherit all methods, attributes from it's parent class, and can then override methods as needed. Overriding means the logic in the parent class is not used. The `super` builtin function (not shown here) exists to allow the programmer to defer logic up the inheritance chain to the parent class when needed.
- the knowledge of inheritance can be useful in this exercises because all `Exceptions` types inherit from the base class

### [For Loop][for-loop]

- the `for ... in` concept is useful to loop over the lists
- The example solution uses `for` loops to iterate over various function inputs.
- iterating over the passed in `matrix` string using a `for` loop to extract "rows" and "columns" that are appended to a list

### [Classes][classes]

- the exercise relies on the `class` statement to create a custom class
- use of `class` to create a custom data structure
- the exercise objective is to define a `matrix` type. Tested methods are linked to a `matrix` class
- a general comprehension of class concept and and how it works is required, `class` statement
- classes are defined with the `class <ClassName>:` syntax
- the exercise objective is to define a `robot` type. Tested methods are linked to a `robot` class.

### [String Translation][string-translation]

- the `str.translate()` _instance method_ is called on an object from the `str` class (e.g. `<my string>`.translate()). Returns a copy of the inital string with each character re-mapped through the given _translation table_. The _translation table_ is typically a mapping or sequence type that implements indexing via the magic method `__getitem__()`.

### [Argument Unpacking][Argument Unpacking]

- the example solution for this exercise uses `splat` (`*`) to unpack rows for the `zip()` function.

### [Immutability][immutability]

- strings are immutable, and so cannot have values assigned; new strings can be created, however
- `text` str in Python is [immutable](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str).

### [Instance Methods][instance-methods]

- tests for this exercises require one or more instance methods that will return a specified row or column list of the `matrix`.
- tests for this exercises require one or more instance methods that will take in a set of starting coordinates and a bearing and then accept a series of instructions that "move" the instance to a new set of coordinates and bearing.

### [Non-Public Methods][non-public-methods]

- Methods or attributes (including those of an imported module) prefixed with an underscore, `_`, are conventionally treated as "non-public" methods. Python does not support data privacy in the way a language like Java does. Instead convention dictates that methods and attributes that are not prefixed with a single underscore can be expected to remain stable along with semver, i.e. a public method will be backwards compatible with minor version updates, and can change with major version updates. Generally, importing non-public functions or using non-public methods is discouraged, though Python will not explicitly stop the programmer from doing so.

### [Booleans are integers][booleans-are-integers]

- Booleans values are just named aliases for the integers 1 (`True`) and 0 (`False`)

### [Static Methods][static-methods]

- Distinct from built-in functions, instance methods, and class methods, these are methods that are bound to a class, rather than an instance, and called _without_ explicitly or implicitly passing in an object of the class. The example solution for this exercise uses the `static` `str` method `maketrans`.

### [Property Decorator][property-decorator]

- this exercise relies on the `@property` decorator to provide read-only dynamic access to the current list of allergens

### [overload][overload]

- students need to overload methods and specifically dunder methods in this exercise

### [Data Structures][data-structures]

- the exercise requires the use of a collection like enum.Flag or collections.OrderedDict

### [in][in]

- use of the `in` statement is useful to look for an object into a list

### [Implicit Argument][implicit-argument]

- the example uses the `self` implicit argument for methods and properties linked to a specific instance of the class
- the example uses `self` for methods and properties linked to a specific instance of the class.

### [class methods][class-methods]

- student must know how methods of a class work inside and outside the class, the use and meaning of `def` statement

### [Functions][functions]

- functions are defined and named using the `def` keyword
- A named (_and often reusable_) section of code that performs a specific task. It may or may not have _arguments_ passed in, and may or may not _return_ data. Created using the `def` keyword.
- Tests for this exercise expect a function named `parse` that can be called to transform the _markdown_ formatted text and return HTML formatted text.

### [Modular Division][modular-division]

- the exercise relies on the `%` operator to check if one number is evenly divisible by another
- the example uses the modulus operator to calculate the appropriate _compass_ setting when calling the `right` compass method

### [Comprehension Syntax][comprehension-syntax]

- knowing that this is equivalent to a `for loop` - putting the row or column creation code _inside_ the list literal instead of using loop + append.
- the use of list comprehension concept can be useful in this exercise

### [Instance Properties][instance-properties]

- this exercise rquires one or more instance properties to persist passed in data.

### [Bitwise Operators][bitwise-operators]

- this exercise relies on bitwise AND (`&`) and potentially bitwise LSHIFT (`<<`) to inspect the Boolean value of individual bits in a bitflag
- bitwise operators such as `<<`, `>>`, `&`, `|`, `~`, `^` are central to this exercise

### [Conditionals structures][conditionals]

- The example solution uses `if` to check for pattern matching and membership conditions in different functions for processing different markdown patterns.
- `if ... else` and `elif` allow a programmer to switch code branches depending on some condition

### [Return Values][return-value]

- the knowledge of `return` statement could be a useful concept in this exercise
- the function must return a number (int)
- Most of the functions in the example solution specify a _return_ value using the `return` keyword.
- the exercise must use a `return` statement to return a value to the caller
- this function return a string by this line: `return text[::-1]`
- the `return` keyword is used in a _return statement_ at the end of a function. Exits a function and may or may not pass data or an expression back to calling code. Functions in python without an expicit `return` keyword and statment will return (pass back) the singleton object `none`. The example code _returns_ a copy of the passed-in argument (assumed to be a string) that has been mapped through `str.translate()`, using the table made from `str.maketrans()`
- knowing that functions need not have _explicit_ return statements or values but will return `None` if `return` is not specified. Except for the two `@property`-decorated functions, all of the functions in the example omit an explicit `return` statment and all return `None`.
- the knowledge of `return` statement could be a useful concept in this exercise
- "row" and "column" list values are expected from defined instance method(s)

### [Bitflags][bitflags]

- a general understanding of bitflags is required to solve this exercise

### [Constants][constants]

- Avoid "magic numbers", defining instead meaningfully named constants. PEP 8 convention for constants: `UPPER_SNAKE_CASE`
- are not enforced by the runtime, but are used via the convention of `UPPER_CASE` to signal that these values are expected to remain unchanged. Preferably, constants are defined at a module level. The example solution uses the `UPPER_CASE` convention to define NORTH, SOUTH, EAST, and WEST constants.

### [Dunder Methods][dunder-methods]

- the exercise relies on the `__init__` dunder method to control class instantiation
- student needs to know when to use dunder methods `__init__` and `__str__`
- "dunder" -> "double under", referring to the names of these methods being prefixed with two underscores, e.g. `__init__`. There is no formal privacy in Python, but conventionally a single underscore indicates a private method, or one that the programmer should assume may change at any time; methods without an underscore are considered part of an object's public API. Double underscores are even more special - they are used by Python's builtin functions like `len()`, for example, to allow objects to implement various interfaces and functionality. They can also be used for operator overloading. If you have a custom class that you would like to be able to compare to other instances of the same class, implementing `__lt__`, `__gt__`, `__eq__` etc. allow programmers to use the `>`, `<`, `=` operators. Dunder methods allow programmers to build useful objects with simple interfaces, i.e. you can add two instances together using `+` instead of writing something like `instance1.add(instance2)`.
- the example uses the `__init__` magic method as its constructor for the class
- User defined classes can (and generally do) overload the `__init__` method, whose first argument is `self`, because the result of `__init__` is a class *instance*.
- The example uses `__init__` as a constructor for the class, which also calls `__new__`. In addition, the example uses `__call__()` via the appending of `()` to instance method names, and `__eq__()` (_rich compairison_) via the use of `==`

### [Standard Library][standard-library]

- the `re` module is an example of the Python stdlib (standard library), or included code libraries and tools that are frequently used in Python

### [Lookup Efficiency][lookup-efficiency]

- an efficient solution requires knowing that membership testing is O(1) in **dict** and the **enum.Enum** variants, but is O(N) in **list** and other sequential types

### [None][none]

- student needs to know the meaning of `None` and how and when assign it to a variable
- Pythons null type, referred to when a null or "placeholder" is needed. It is in and of itself a singleton in any given python program.

### [Constructor][constructor]

- student needs to know how to build an object using its constructor
- customizing object initalization with actions and persisting data. The example uses a constructor to process the passed in data into a list of lists assigned to an instance property

### [Membership Testing][membership-testing]

- this exercise relies on testing membership of a value in a collection of values
- the `in` keyword, as in `"s" in "string`, allows the user to check membership in the longer string

### [Strings][strings]

- strings are used generally

### [Enumeration][enumeration]

- `zip()` in this solution creates an iterable, which is iterated over by using the `for ... in ` syntax

### [Indexing][indexing]

- the "rows" and "columns" of this exercise need to be retrieved from a list of lists via index
- for iterables, individual items can be accessed with `stringname[x]` notation. Negative numbers start to count backwards
- finding a value by key in a dictionary using `<dictionary name>[<key name>]` The example uses passed in move arguments as `keys` to look up corresponding `values` (_method names_) for moving the robot in the _instructions_ dictionary.

### [Polymorphism][polymorphism]

- Python is "dynamically typed," meaning that variable names are bound to objects only, not to a particular type. You can assign `foo` to a string, and then reassign it to an `int` with no issues. "Polymorphism" formally means that different types respond to the same function - so the ability to add custom class instances together using `+`, for example, shows how Python can define the same function against different types.

### [Equality Operator][equality-operator]

- the `==` operator calls the dunder method `__eq__()`. By default, objects of different types are never considered equal to each other unless they are numerical types (ie int, float) or have otherwise overloaded the default implementation of the `__eq__` dunder method. `==` is always defined, but for some object types (_like class objects_) it is equivalent to calling `is`. See [`__eq__`](https: //docs.python.org/3/reference/datamodel.html#object.__eq__) for additional details. This exercise uses the equality operator to test that the `self.direction` attribute is equal to one of the constant values defined.

### [Refactor][refactor]

- Reviewing and rewriting (or re-organizing) code for clarity and efficiency. This exercise requires a re-write of pre-existing code that uses functions to parse passed-in text in markdown.

### [PEP 8 Style][pep-8-style]

- PEP 8 is the Python official style guide. Black is emerging as the defacto "pyfmt" tool: should we recommend it? (since the advent of `gofmt` and then `rustfmt`, I'm totally sold on opinionated auto-format tools: saves time and no more bikeshedding)

### [List Methods][list-methods]

- the use of methods of list could be useful in this exercise. Methods like `append`, `pop`...

### [Short-Circuiting][short-circuiting]

- the exercise relies on short-circuiting to avoid unnecessary calculations

### [Exception message][exception-message]

- Custom error messages can (and should) be supplied to an Exception when raised

### [Instantiation][instantiation]

- creating different instances of the `robot` class with different data representing different starting positions and bearing are tested.

### [Comparison][comparison]

- concept required to solve the exercise, `==`, `>`, `<`
- the exercise relies on the `==` and `!=` operators to make binary comparisons between values

### [Integer comparison][integer-comparison]

- concept required to solve the exercise

### [Lists][lists]

- knowledge of lists and iteration on lists is required for this exercise
- The example uses lists in several places to hold text to be processed or searched - or for tracking the state of pieces of the passed-in text.
- this exercise requires "row" or "column" be returnd as a `list`. A `list` of `lists` is also the reccommended way to process and store the passed-in data.
- the example stores compass direction constants in a `list`
- knowledge of lists and iteration on lists is required for this exercise

### [Operators][operators]

- `!=` is "not equal", which is not the same thing as `is`, or an identity check, but is the inverse of `==`, which is equality

### [Duck Typing][duck-typing]

- Python is also a good example of "Duck typing," to wit, "if it walks like a duck, talks like a duck, it's a duck.". This is accomplished partly with "magic" or "dunder" methods (double-under) that provide various interfaces to an object. If an object implements `__iter__` and `__next__`, it can be iterated through; it doesn't matter what type the object actually is.
- the exercise supports any argument that supports modular division and comparison to integers (ie int, float, Decimal)

### [Iterators][iterators]

- the example solution for this exercise uses `zip()`, which returns an _iterator_.

### [Type hinting][type-hinting]

- In modern Python it's possibly to type hint annotations to parameters and variables, see [typing](https://docs.python.org/3/library/typing.html#module-typing). While not neccessary in Python such annotations can help your code be easier to read, understand, and check automatically using tools like `mypy`.

### [Expressions][expressions]

- the exercise relies on writing an expression that will be evaluated to a return value

### [Int][int]

- the example converts the parsed `str` elements into `int`

### [Regular Expressions][regular-expressions]

- the `re.sub()` function of the `re` module that replaces a `regular expression` match with a new value. The example solutions use this function in various places to substitute _markdown_ syntax for _HTML_ syntax in the passed in markdown text.
- Both the original code to be refactored for this exercise and the example solution import and use the `re` module for Regular Expressions in python.
- the `re.match()` function from the `re` module returns a `match` object with any matched values from a specified Regular Expression or pre-compliled Regular Expression. The example uses `re.match()` in multiple places to search for text patterns that need re-formatting or subsitituting.
- Various functions in the re module return a `re.Match` _instance_ which in turn has a `Match.group` method. `Match.group` exists even if there are no groups specified in the pattern. See the [Match.group docs](https://docs.python.org/3/library/re.html#re.Match.group) for more detail.
- regular expressions is a language of sorts that can detect substrings and extract groups from a string, as well as replace them with something else
- A Domain Specific Language (DSL) for text processing. Like many other programming languages in use, python supports a quasi-dialect of PCRE (_Perl compatible regular expressions_). `Regular expressions` can be used via the core python `re` module, or the third-party `regex` module. Both the original code to be refactored for this exercise and the example solutions use the core `re` module to access `regular expressions` functionality.

### [Generator comprehension][generator-comprehension]

- a generator comprehension is passed to `sum()` to drive summation without storing all the values in a list first

### [Importing][importing]

- to use the module, the `import` syntax can be used
- Both the original code to be refactored for the exercise and the example solution use the `import` keyword to import the `re` module in support of Regular Expressions in python.
- a reasonably readable solution will require importing from the standard library

### [Function signature][function-signature]

- functions take named arguments which are accessible within the body of the function; this one requires the student to make a function that accepts 2

### [Assignment][assignment]

- The example solution uses assignment for variables and other values.
- instance properties need to be assigned passed in data
- the example uses assignment for all the instance properties and `instructions` dictionary.

### [Generators][generators]

- generators calculate then `yield` a value one at a time, as opposed to lists which calculate and return all values in memory at once. A generator will pick up where it leaves off, and generate one item at a time, on demand

### [Exception handling][exception-handling]

- the exercise requires Exception handling

### [Operator overloading][operator-overloading]

- How to overload the `+` and `-` operators using the `__add__` and `__sub__` special methods.

### [Multiple Assignment][multiple-assignment]

- Python allows multiple assignment, assigning the items on the left of `=` _in order_ to the values on the right of `=` by forming tuples and then "unpacking" them. This exercise solution uses multiple assignment for the constant values NORTH, EAST, SOUTH, WEST.

### [Methods of list][methods-of-list]

- the use of methods of list could be useful in this exercise. Methods like `append`, `pop`...

### [Zip][zip]

- builtin that joins multiple iterables into a single one
- the example solution for this exercise uses this function to aggregage the column-wise elements of each rown list to form the matrix "columns".

### [Rich comparison methods][rich-comparison-methods]

- The `__eq__` method is overloaded

### [Inequality][inequality]

- this solution checks if `a` is not equal to `b`.

### [String Methods][string-methods]

- strings (and other types) have built in instance methods - in this case, `"string".startswith("s")` which are called from the instance of the string itself
- this exercise uses `str.maketrans()` (a static method of `str` that returns a dictionary to create a _translation table_ as required by the `str.translate()` instance method. This method is unusual in that it takes either a single dictionary or two strings of equal length. The example solution for this exercise uses `str.maketrans()` with a two-string argument.

### [Loops][loops]

- concept required to solve the exercise
- the `for ... in` syntax is useful for looping through a list or other iterable object
- the knowledge of `while` and `for` loops are useful to solve this exercise

### [Builtin Function][builtin-functions]

- strings have a length, accessible by calling `len()`, a builtin python function
- Python has several handy builtin functions in the stdlib that can operate on many types of data, e.g. `len()`, `max()`, `min()`. Under the hood these are implemented via dunder methods - if an object (and everything in Python is an object) implements the correct dunder methods (see that topic for more information), it can support use in these functions. (For example, if an object implements `__len__`, the len(<object>) will return that value.) Because these functions are not strictly tied to any data type, they can be used almost anywhere, and will crop up again and again as we learn Python. Docs: https://docs.python.org/3/library/functions.html
- the `sum()` built-in function is a useful concept to solve the exercise
- the `len()` built-in function is a useful concept in this exercise
- the `enumerate` built-in function is a useful concept in this exercise

### [Tuple unpacking][tuple-unpacking]

- the values in an iterable can be unpacked into variables and used, i.e. `for a, b in zip(s1, s2)`
- iterating through a list of tuples, i.e. [(1, 2), (2,3)], each piece of each tuple can be unpacked into a separate variable (syntax: `a, b = (1, 2)`); this works for any sort of iterable (lists, for example, and even strings!) but is commonly used with tuples because they are typically of a known size/length, and so can be safely unpacked into N variables, with names.

### [Iteration][iteration]

- the passed-in string is iterated over, and split into rows. The row lists are iterated over and split into elements
- the example uses a `for loop` to iterate through the letters of the passed-in `commands` string and looks up the corresponding values in a dictionary, so that the appropriate methods can be called to move the `robot`.
- the `for ... in` concept is useful to loop over the lists

### [Iteration][iterable]

- strings are iterable, which provides a lot of opportunity to leverage Python functions against them
- understanding that strings, lists, and other data structures can be iterated over in the same fashion
- characters in a string are *iterables* and are subject to index and slice access as described below
- The example solution uses the `for _ in _` syntax to iterate over a list of lines. This is possible because a list is an `iterable`.

### [Raise][raise]

- the student is required to raise an `Exception` for incorrect input
- an appropriate Exceptions must be raised with the `raise` keyword
- the user could find useful the `Exception` concept to `raise` a `ValueError` for incorrect input

### [Boolean Logic][boolean-logic]

- the exercise relies on `and`, `or`, and (optionally) `not` to form Boolean predicates
- the exercise relies on `and` and `or` to combine Boolean predicates into a single logical answer
- the `or` and `and` keywords are used

### [Binary Numbers][binary-numbers]

- binary numbers are a core important concept for this exercise

### [Composition][composition]

- adding functionality from a class by incorporating an instance of that class in a class you are creating. The example creates a `robot` by instantiating a `compass` and assigning it to the `self`.compass attribute of `robot`.

### [Enumerated Values][enumerated-values]

- the exercise relies on a fixed enumeration of possible values in a data structure

### [Identity][identity]

- the best way to check if an element is `None` is via the _identity_ operator, `is`

### [Equivalence][equivalence]

- the exercise relies on the `==` and `!=` operators to check that two values are equivalent (or not)

### [Namespaces][namespaces]

- knowing to use `self`.`<propertyname>` for instance properties and `self` as first argument to instance methods in a class
- knowing to use `self.<propertyname>` for instance attributes and `self` as first argument to instance methods in a class. Additionally, the example uses `self.<methodname>()` to call a previously stored method name.

### [Exception hierarchy][exception-hierarchy]

- the idiomatic `Exception` type is a `ValueError`, meaning the input is incorrect

### [String Splitting][string-splitting]

- The example solution uses `str.split()` to break the passed in markdown string into a list of lines broken up by the `\n` character. The alternate Python example solution uses `str.splitlines()` for the same effect across all line end characters.
- the example uses `str.split` with and without seperators to break the passed in string into "rows" and then "elements"

### [Default Arguments][default-arguments]

- pre-setting function arguments to protect against them not being passed by a caller. The example uses `direction = NORTH` and `x=0, y=0` to ensure those values for a `robot` even if they are not initally passed.

### [Method Parameters][method-parameters]

- the example `__init__` method has `self`, direction, x, and y (coordinates) as parameters. It also uses `self` and `commands` (a string) for parameters of the `move()` method.

### [Recursion][recursion]

- recursion is a core concept in this exercise

### [Initialization][initialization]

- customizing object instatiation with actions and persisting data. The example uses `__init__` to persist a `compass` object and x, y coordinates assigned to instance attributes.

### [Range][range]

- the `range()` built-in represents an immutable sequence of numbers (or any object that implements the __index__ magic method). Used in the example to control the number of loops while iterating through a passed-in line or list.
- the `range()` built-in type represents an immutable sequence of numbers (or any object that implements the `__index__` dunder method). Used in the example to represent the values from zero to 3 as assigned to NORTH, EAST, SOUTH, WEST.

### [Powers of Two][powers-of-two]

- the exercise relies on the use of powers of two in fundamental binary (bitwise) operations

### [class members][class-members]

- student must know how members of a class work

### [Higher-Order Function][higher-order-function]

- a function that takes one or more other functions as arguments, _returning_ a function as its return value. The example uses the built-in `property()` as a higher-order function through `@property`.

### [Generics][generics]

- the exercise is polymorphic across numerical types (ie int, float, Decimal)

### [Slicing][slicing]

- the extended solution to this exercise can employ a slice (returns a copy) instead of calling `.copy()`.
- a slice within an iterable, i.e. the slice of items from `<iterable>[x]` to `<iterable>[y]`, can be accessed via `<iterable>[x:y]` notation; a third parameter allows "skipping" by `z`, i.e. `stringname[x:y:z]`
- becase `str` in Python is a sequence type, [slicing](https://docs.python.org/3/reference/expressions.html#slicings) syntax can be used here. Specifically: for syntax `string[start:stop:stride]`:

### [Function Decorator][function-decorator]

- a higher-order function that takes another function as an argument. The "decorating" function extends the behavior of the "decorated" function without explicitly modifying it (i.e. it _wraps_ or _decorates_ it). Called in python by using the `@` symbol and the function name ahead of the function being decorated. The example uses pythons built-in `property()` as a decorator (`@property`) to return a "compound" read-only instance property made up of two separate instance attributes.

### [Objects][objects]

- creating different instances with different data representing different `matrices` is tested

### [Order of Evaluation][order-of-evaluation]

- the exercise relies on parentheses to explicitly modify the normal order of evaluation of an expression

### [Methods][methods]

- the exercise relies on the `def` statement to create an instance method
- use of `def` to define a class's methods
- classes can have instance *methods* which are called from an instance of the class (as opposed to class methods, called from the Class itself). The first parameter of an instance method is always `self`, which is provided when calling from the instance (i.e. the programmer does not need to pass it as an argument explicitly). Static methods are methods called from the class itself, and are not connected to an instance of the class. They have access to class attributes (those defined on the class, not connected to the `self`), and do not require an instance of the class to exist. Classes can also define a `property` by using the `@property` decorator (not shown here); a `property` can be "lazily evaluated" to avoid uneeded computation

### [Exception Message][exception-message]

- the user can use a custom error message inside the mentioned `ValueError`

### [Operator Precedence][operator-precedence]

- the exercise is most simply stated when the student understands the operator precedence binding rules of Python

### [Instance Attributes][instance-attributes]

- this exercise rquires one or more instance attributes to persist passed in data.

### [Implied Argument][implied-argument]

- the exercise relies on the implied passing of `self` as the first parameter of bound methods
- student needs to know how to use statement `self` in a class
- within the class definition, methods and properties can be accessed via the `self.` notation

### [Docstrings][docstrings]

- used to document the function, normally situated right below `def func():`

### [Type Conversion][type-conversion]

- the passed in data is in `str` format but the output is expected as a list of type `int`.

### [Mutability][mutability]

- in the extended example, knowing there are no protected or private properties in python and adjusting coding patterns
- in the example, knowing there are no protected or private properties in python and so consciously mutating `self.x`, `self.y` and `self.compass` through the called instance methods.

### [Call Semantics][call-semantics]

- knowing that appending `()` to the name of an instance method _calls_ it, since instance methods are _callable_.

### [Exception catching][exception-catching]

- `Exceptions` can be caught from outside the scope where they are raised, using the `try/except` syntax. All `Exceptions` types inherit from the base class, `Exception` and thus can be caught by either checking specifically for the type of Exception, or for any Exception

### [Property][property]

- the `property()` built-in is a function that returns a property attribute. When used as a decorator, this transforms the passed-in method into a _getter_ method for read-only attribute with the same name and docstring.
