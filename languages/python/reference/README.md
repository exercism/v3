# Python reference

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

- Mapping type. The example solution employes a dictionary to return values from the `parse_line()` function. [origin](./exercise-concepts/markdown.md)
- mapping type that has key-value pairs. Returned by `str.maketrans` in the example code. Also one of the argument types accepted by `str.maketrans()`. [origin](./exercise-concepts/rna-transcription.md)
- the example uses a dictionary to map passed in move arguments to methods that perform the moves. The example also uses a dictionary/mapping created by calling `str.maketrans()`. [origin](./exercise-concepts/robot-simulator.md)

### [Method Arguments][method-arguments]

- Parameters passed into a function. In python, these are noted in the `()` following a function name. The example code uses a function named `to_rna()` with an argument of `dna_strand`. [origin](./exercise-concepts/rna-transcription.md)
- The example solutions use functions that take function arguments to operate on passed in markdown strings. [origin](./exercise-concepts/markdown.md)
- the exercise requires a single positional parameter in the function signature [origin](./exercise-concepts/leap.md)
- concept over arguments of a function and how to use them is required [origin](./exercise-concepts/variable-length-quantity.md)
- the methods returning "row" and "column" need to take both `self` and an integer as arguments [origin](./exercise-concepts/matrix.md)

### [Booleans][booleans]

- this solution uses Boolean values (`True` / `False`) [origin](./exercise-concepts/hamming.md)
- True and False of type `bopl`. The example solution uses `True` and `False` as return values from functions that test membership in a list of values. [origin](./exercise-concepts/markdown.md)

### [Bracket Notation][bracket-notation]

- knowing that `[]` should be used to refer to a value at a specific index in a list [origin](./exercise-concepts/matrix.md)

### [String formatting][string-formatting]

- How to format strings, ie `%` operator, `str.format`, f-strings [origin](./exercise-concepts/clock.md)

### [Inheritance][inheritance]

- The default `__str___` method is inherited from `Object`, which every class in Python inherits from. (See: inheritance) [origin](./exercise-concepts/phone-number.md)
- a "subclass" will inherit all methods, attributes from it's parent class, and can then override methods as needed. Overriding means the logic in the parent class is not used. The `super` builtin function (not shown here) exists to allow the programmer to defer logic up the inheritance chain to the parent class when needed. [origin](./exercise-concepts/phone-number.md)
- the knowledge of inheritance can be useful in this exercises because all `Exceptions` types inherit from the base class [origin](./exercise-concepts/variable-length-quantity.md)

### [For Loop][for-loop]

- the `for ... in` concept is useful to loop over the lists [origin](./exercise-concepts/variable-length-quantity.md)
- The example solution uses `for` loops to iterate over various function inputs. [origin](./exercise-concepts/markdown.md)
- iterating over the passed in `matrix` string using a `for` loop to extract "rows" and "columns" that are appended to a list [origin](./exercise-concepts/matrix.md)

### [Classes][classes]

- the exercise relies on the `class` statement to create a custom class [origin](./exercise-concepts/allergies.md)
- use of `class` to create a custom data structure [origin](./exercise-concepts/clock.md)
- the exercise objective is to define a `matrix` type. Tested methods are linked to a `matrix` class [origin](./exercise-concepts/matrix.md)
- a general comprehension of class concept and and how it works is required, `class` statement [origin](./exercise-concepts/binary-search-tree.md)
- classes are defined with the `class <ClassName>:` syntax [origin](./exercise-concepts/phone-number.md)
- the exercise objective is to define a `robot` type. Tested methods are linked to a `robot` class. [origin](./exercise-concepts/robot-simulator.md)

### [String Translation][string-translation]

- the `str.translate()` _instance method_ is called on an object from the `str` class (e.g. `<my string>`.translate()). Returns a copy of the inital string with each character re-mapped through the given _translation table_. The _translation table_ is typically a mapping or sequence type that implements indexing via the magic method `__getitem__()`. [origin](./exercise-concepts/rna-transcription.md)

### [Argument Unpacking][Argument Unpacking]

- the example solution for this exercise uses `splat` (`*`) to unpack rows for the `zip()` function. [origin](./exercise-concepts/matrix.md)

### [Immutability][immutability]

- strings are immutable, and so cannot have values assigned; new strings can be created, however [origin](./exercise-concepts/phone-number.md)
- `text` str in Python is [immutable](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str). [origin](./exercise-concepts/reverse-string.md)

### [Instance Methods][instance-methods]

- tests for this exercises require one or more instance methods that will return a specified row or column list of the `matrix`. [origin](./exercise-concepts/matrix.md)
- tests for this exercises require one or more instance methods that will take in a set of starting coordinates and a bearing and then accept a series of instructions that "move" the instance to a new set of coordinates and bearing. [origin](./exercise-concepts/robot-simulator.md)

### [Non-Public Methods][non-public-methods]

- Methods or attributes (including those of an imported module) prefixed with an underscore, `_`, are conventionally treated as "non-public" methods. Python does not support data privacy in the way a language like Java does. Instead convention dictates that methods and attributes that are not prefixed with a single underscore can be expected to remain stable along with semver, i.e. a public method will be backwards compatible with minor version updates, and can change with major version updates. Generally, importing non-public functions or using non-public methods is discouraged, though Python will not explicitly stop the programmer from doing so. [origin](./exercise-concepts/phone-number.md)

### [Booleans are integers][booleans-are-integers]

- Booleans values are just named aliases for the integers 1 (`True`) and 0 (`False`) [origin](./exercise-concepts/hamming.md)

### [Static Methods][static-methods]

- Distinct from built-in functions, instance methods, and class methods, these are methods that are bound to a class, rather than an instance, and called _without_ explicitly or implicitly passing in an object of the class. The example solution for this exercise uses the `static` `str` method `maketrans`. [origin](./exercise-concepts/rna-transcription.md)

### [Property Decorator][property-decorator]

- this exercise relies on the `@property` decorator to provide read-only dynamic access to the current list of allergens [origin](./exercise-concepts/allergies.md)

### [overload][overload]

- students need to overload methods and specifically dunder methods in this exercise [origin](./exercise-concepts/binary-search-tree.md)

### [Data Structures][data-structures]

- the exercise requires the use of a collection like enum.Flag or collections.OrderedDict [origin](./exercise-concepts/allergies.md)

### [in][in]

- use of the `in` statement is useful to look for an object into a list [origin](./exercise-concepts/binary-search-tree.md)

### [Implicit Argument][implicit-argument]

- the example uses the `self` implicit argument for methods and properties linked to a specific instance of the class [origin](./exercise-concepts/matrix.md)
- the example uses `self` for methods and properties linked to a specific instance of the class. [origin](./exercise-concepts/robot-simulator.md)

### [class methods][class-methods]

- student must know how methods of a class work inside and outside the class, the use and meaning of `def` statement [origin](./exercise-concepts/binary-search-tree.md)

### [Functions][functions]

- functions are defined and named using the `def` keyword [origin](./exercise-concepts/hamming.md)
- A named (_and often reusable_) section of code that performs a specific task. It may or may not have _arguments_ passed in, and may or may not _return_ data. Created using the `def` keyword. [origin](./exercise-concepts/rna-transcription.md)
- Tests for this exercise expect a function named `parse` that can be called to transform the _markdown_ formatted text and return HTML formatted text. [origin](./exercise-concepts/markdown.md)

### [Modular Division][modular-division]

- the exercise relies on the `%` operator to check if one number is evenly divisible by another [origin](./exercise-concepts/leap.md)
- the example uses the modulus operator to calculate the appropriate _compass_ setting when calling the `right` compass method [origin](./exercise-concepts/robot-simulator.md)

### [Comprehension Syntax][comprehension-syntax]

- knowing that this is equivalent to a `for loop` - putting the row or column creation code _inside_ the list literal instead of using loop + append. [origin](./exercise-concepts/matrix.md)
- the use of list comprehension concept can be useful in this exercise [origin](./exercise-concepts/variable-length-quantity.md)

### [Instance Properties][instance-properties]

- this exercise rquires one or more instance properties to persist passed in data. [origin](./exercise-concepts/matrix.md)

### [Bitwise Operators][bitwise-operators]

- this exercise relies on bitwise AND (`&`) and potentially bitwise LSHIFT (`<<`) to inspect the Boolean value of individual bits in a bitflag [origin](./exercise-concepts/allergies.md)
- bitwise operators such as `<<`, `>>`, `&`, `|`, `~`, `^` are central to this exercise [origin](./exercise-concepts/variable-length-quantity.md)

### [Conditionals structures][conditionals]

- The example solution uses `if` to check for pattern matching and membership conditions in different functions for processing different markdown patterns. [origin](./exercise-concepts/markdown.md)
- `if ... else` and `elif` allow a programmer to switch code branches depending on some condition [origin](./exercise-concepts/phone-number.md)

### [Return Values][return-value]

- the knowledge of `return` statement could be a useful concept in this exercise [origin](./exercise-concepts/variable-length-quantity.md)
- the function must return a number (int) [origin](./exercise-concepts/hamming.md)
- Most of the functions in the example solution specify a _return_ value using the `return` keyword. [origin](./exercise-concepts/markdown.md)
- the exercise must use a `return` statement to return a value to the caller [origin](./exercise-concepts/leap.md)
- this function return a string by this line: `return text[::-1]` [origin](./exercise-concepts/reverse-string.md)
- the `return` keyword is used in a _return statement_ at the end of a function. Exits a function and may or may not pass data or an expression back to calling code. Functions in python without an expicit `return` keyword and statment will return (pass back) the singleton object `none`. The example code _returns_ a copy of the passed-in argument (assumed to be a string) that has been mapped through `str.translate()`, using the table made from `str.maketrans()` [origin](./exercise-concepts/rna-transcription.md)
- knowing that functions need not have _explicit_ return statements or values but will return `None` if `return` is not specified. Except for the two `@property`-decorated functions, all of the functions in the example omit an explicit `return` statment and all return `None`. [origin](./exercise-concepts/robot-simulator.md)
- the knowledge of `return` statement could be a useful concept in this exercise [origin](./exercise-concepts/variable-length-quantity.md)
- "row" and "column" list values are expected from defined instance method(s) [origin](./exercise-concepts/matrix.md)

### [Bitflags][bitflags]

- a general understanding of bitflags is required to solve this exercise [origin](./exercise-concepts/allergies.md)

### [Constants][constants]

- Avoid "magic numbers", defining instead meaningfully named constants. PEP 8 convention for constants: `UPPER_SNAKE_CASE` [origin](./exercise-concepts/clock.md)
- are not enforced by the runtime, but are used via the convention of `UPPER_CASE` to signal that these values are expected to remain unchanged. Preferably, constants are defined at a module level. The example solution uses the `UPPER_CASE` convention to define NORTH, SOUTH, EAST, and WEST constants. [origin](./exercise-concepts/robot-simulator.md)

### [Dunder Methods][dunder-methods]

- the exercise relies on the `__init__` dunder method to control class instantiation [origin](./exercise-concepts/allergies.md)
- student needs to know when to use dunder methods `__init__` and `__str__` [origin](./exercise-concepts/binary-search-tree.md)
- "dunder" -> "double under", referring to the names of these methods being prefixed with two underscores, e.g. `__init__`. There is no formal privacy in Python, but conventionally a single underscore indicates a private method, or one that the programmer should assume may change at any time; methods without an underscore are considered part of an object's public API. Double underscores are even more special - they are used by Python's builtin functions like `len()`, for example, to allow objects to implement various interfaces and functionality. They can also be used for operator overloading. If you have a custom class that you would like to be able to compare to other instances of the same class, implementing `__lt__`, `__gt__`, `__eq__` etc. allow programmers to use the `>`, `<`, `=` operators. Dunder methods allow programmers to build useful objects with simple interfaces, i.e. you can add two instances together using `+` instead of writing something like `instance1.add(instance2)`. [origin](./exercise-concepts/hamming.md)
- the example uses the `__init__` magic method as its constructor for the class [origin](./exercise-concepts/matrix.md)
- User defined classes can (and generally do) overload the `__init__` method, whose first argument is `self`, because the result of `__init__` is a class *instance*. [origin](./exercise-concepts/phone-number.md)
- The example uses `__init__` as a constructor for the class, which also calls `__new__`. In addition, the example uses `__call__()` via the appending of `()` to instance method names, and `__eq__()` (_rich compairison_) via the use of `==` [origin](./exercise-concepts/robot-simulator.md)

### [Standard Library][standard-library]

- the `re` module is an example of the Python stdlib (standard library), or included code libraries and tools that are frequently used in Python [origin](./exercise-concepts/phone-number.md)

### [Lookup Efficiency][lookup-efficiency]

- an efficient solution requires knowing that membership testing is O(1) in **dict** and the **enum.Enum** variants, but is O(N) in **list** and other sequential types [origin](./exercise-concepts/allergies.md)

### [None][none]

- student needs to know the meaning of `None` and how and when assign it to a variable [origin](./exercise-concepts/binary-search-tree.md)
- Pythons null type, referred to when a null or "placeholder" is needed. It is in and of itself a singleton in any given python program. [origin](./exercise-concepts/markdown.md)

### [Constructor][constructor]

- student needs to know how to build an object using its constructor [origin](./exercise-concepts/binary-search-tree.md)
- customizing object initalization with actions and persisting data. The example uses a constructor to process the passed in data into a list of lists assigned to an instance property [origin](./exercise-concepts/matrix.md)

### [Membership Testing][membership-testing]

- this exercise relies on testing membership of a value in a collection of values [origin](./exercise-concepts/allergies.md)
- the `in` keyword, as in `"s" in "string`, allows the user to check membership in the longer string [origin](./exercise-concepts/phone-number.md)

### [Strings][strings]

- strings are used generally [origin](./exercise-concepts/hamming.md)

### [Enumeration][enumeration]

- `zip()` in this solution creates an iterable, which is iterated over by using the `for ... in ` syntax [origin](./exercise-concepts/hamming.md)

### [Indexing][indexing]

- the "rows" and "columns" of this exercise need to be retrieved from a list of lists via index [origin](./exercise-concepts/matrix.md)
- for iterables, individual items can be accessed with `stringname[x]` notation. Negative numbers start to count backwards [origin](./exercise-concepts/phone-number.md)
- finding a value by key in a dictionary using `<dictionary name>[<key name>]` The example uses passed in move arguments as `keys` to look up corresponding `values` (_method names_) for moving the robot in the _instructions_ dictionary. [origin](./exercise-concepts/robot-simulator.md)

### [Polymorphism][polymorphism]

- Python is "dynamically typed," meaning that variable names are bound to objects only, not to a particular type. You can assign `foo` to a string, and then reassign it to an `int` with no issues. "Polymorphism" formally means that different types respond to the same function - so the ability to add custom class instances together using `+`, for example, shows how Python can define the same function against different types. [origin](./exercise-concepts/hamming.md)

### [Equality Operator][equality-operator]

- the `==` operator calls the dunder method `__eq__()`. By default, objects of different types are never considered equal to each other unless they are numerical types (ie int, float) or have otherwise overloaded the default implementation of the `__eq__` dunder method. `==` is always defined, but for some object types (_like class objects_) it is equivalent to calling `is`. See [`__eq__`](https: //docs.python.org/3/reference/datamodel.html#object.__eq__) for additional details. This exercise uses the equality operator to test that the `self.direction` attribute is equal to one of the constant values defined. [origin](./exercise-concepts/robot-simulator.md)

### [Refactor][refactor]

- Reviewing and rewriting (or re-organizing) code for clarity and efficiency. This exercise requires a re-write of pre-existing code that uses functions to parse passed-in text in markdown. [origin](./exercise-concepts/markdown.md)

### [PEP 8 Style][pep-8-style]

- PEP 8 is the Python official style guide. Black is emerging as the defacto "pyfmt" tool: should we recommend it? (since the advent of `gofmt` and then `rustfmt`, I'm totally sold on opinionated auto-format tools: saves time and no more bikeshedding) [origin](./exercise-concepts/clock.md)

### [List Methods][list-methods]

- the use of methods of list could be useful in this exercise. Methods like `append`, `pop`... [origin](./exercise-concepts/variable-length-quantity.md)

### [Short-Circuiting][short-circuiting]

- the exercise relies on short-circuiting to avoid unnecessary calculations [origin](./exercise-concepts/leap.md)

### [Exception message][exception-message]

- Custom error messages can (and should) be supplied to an Exception when raised [origin](./exercise-concepts/hamming.md)

### [Instantiation][instantiation]

- creating different instances of the `robot` class with different data representing different starting positions and bearing are tested. [origin](./exercise-concepts/robot-simulator.md)

### [Comparison][comparison]

- concept required to solve the exercise, `==`, `>`, `<` [origin](./exercise-concepts/variable-length-quantity.md)
- the exercise relies on the `==` and `!=` operators to make binary comparisons between values [origin](./exercise-concepts/leap.md)

### [Integer comparison][integer-comparison]

- concept required to solve the exercise [origin](./exercise-concepts/binary-search-tree.md)

### [Lists][lists]

- knowledge of lists and iteration on lists is required for this exercise [origin](./exercise-concepts/variable-length-quantity.md)
- The example uses lists in several places to hold text to be processed or searched - or for tracking the state of pieces of the passed-in text. [origin](./exercise-concepts/markdown.md)
- this exercise requires "row" or "column" be returnd as a `list`. A `list` of `lists` is also the reccommended way to process and store the passed-in data. [origin](./exercise-concepts/matrix.md)
- the example stores compass direction constants in a `list` [origin](./exercise-concepts/robot-simulator.md)
- knowledge of lists and iteration on lists is required for this exercise [origin](./exercise-concepts/variable-length-quantity.md)

### [Operators][operators]

- `!=` is "not equal", which is not the same thing as `is`, or an identity check, but is the inverse of `==`, which is equality [origin](./exercise-concepts/hamming.md)

### [Duck Typing][duck-typing]

- Python is also a good example of "Duck typing," to wit, "if it walks like a duck, talks like a duck, it's a duck.". This is accomplished partly with "magic" or "dunder" methods (double-under) that provide various interfaces to an object. If an object implements `__iter__` and `__next__`, it can be iterated through; it doesn't matter what type the object actually is. [origin](./exercise-concepts/hamming.md)
- the exercise supports any argument that supports modular division and comparison to integers (ie int, float, Decimal) [origin](./exercise-concepts/leap.md)

### [Iterators][iterators]

- the example solution for this exercise uses `zip()`, which returns an _iterator_. [origin](./exercise-concepts/matrix.md)

### [Type hinting][type-hinting]

- In modern Python it's possibly to type hint annotations to parameters and variables, see [typing](https://docs.python.org/3/library/typing.html#module-typing). While not neccessary in Python such annotations can help your code be easier to read, understand, and check automatically using tools like `mypy`. [origin](./exercise-concepts/reverse-string.md)

### [Expressions][expressions]

- the exercise relies on writing an expression that will be evaluated to a return value [origin](./exercise-concepts/leap.md)

### [Int][int]

- the example converts the parsed `str` elements into `int` [origin](./exercise-concepts/matrix.md)

### [Regular Expressions][regular-expressions]

- the `re.sub()` function of the `re` module that replaces a `regular expression` match with a new value. The example solutions use this function in various places to substitute _markdown_ syntax for _HTML_ syntax in the passed in markdown text. [origin](./exercise-concepts/markdown.md)
- Both the original code to be refactored for this exercise and the example solution import and use the `re` module for Regular Expressions in python. [origin](./exercise-concepts/markdown.md)
- the `re.match()` function from the `re` module returns a `match` object with any matched values from a specified Regular Expression or pre-compliled Regular Expression. The example uses `re.match()` in multiple places to search for text patterns that need re-formatting or subsitituting. [origin](./exercise-concepts/markdown.md)
- Various functions in the re module return a `re.Match` _instance_ which in turn has a `Match.group` method. `Match.group` exists even if there are no groups specified in the pattern. See the [Match.group docs](https://docs.python.org/3/library/re.html#re.Match.group) for more detail. [origin](./exercise-concepts/markdown.md)
- regular expressions is a language of sorts that can detect substrings and extract groups from a string, as well as replace them with something else [origin](./exercise-concepts/phone-number.md)
- A Domain Specific Language (DSL) for text processing. Like many other programming languages in use, python supports a quasi-dialect of PCRE (_Perl compatible regular expressions_). `Regular expressions` can be used via the core python `re` module, or the third-party `regex` module. Both the original code to be refactored for this exercise and the example solutions use the core `re` module to access `regular expressions` functionality. [origin](./exercise-concepts/markdown.md)

### [Generator comprehension][generator-comprehension]

- a generator comprehension is passed to `sum()` to drive summation without storing all the values in a list first [origin](./exercise-concepts/hamming.md)

### [Importing][importing]

- to use the module, the `import` syntax can be used [origin](./exercise-concepts/phone-number.md)
- Both the original code to be refactored for the exercise and the example solution use the `import` keyword to import the `re` module in support of Regular Expressions in python. [origin](./exercise-concepts/markdown.md)
- a reasonably readable solution will require importing from the standard library [origin](./exercise-concepts/allergies.md)

### [Function signature][function-signature]

- functions take named arguments which are accessible within the body of the function; this one requires the student to make a function that accepts 2 [origin](./exercise-concepts/hamming.md)

### [Assignment][assignment]

- The example solution uses assignment for variables and other values. [origin](./exercise-concepts/markdown.md)
- instance properties need to be assigned passed in data [origin](./exercise-concepts/matrix.md)
- the example uses assignment for all the instance properties and `instructions` dictionary. [origin](./exercise-concepts/robot-simulator.md)

### [Generators][generators]

- generators calculate then `yield` a value one at a time, as opposed to lists which calculate and return all values in memory at once. A generator will pick up where it leaves off, and generate one item at a time, on demand [origin](./exercise-concepts/hamming.md)

### [Exception handling][exception-handling]

- the exercise requires Exception handling [origin](./exercise-concepts/hamming.md)

### [Operator overloading][operator-overloading]

- How to overload the `+` and `-` operators using the `__add__` and `__sub__` special methods. [origin](./exercise-concepts/clock.md)

### [Multiple Assignment][multiple-assignment]

- Python allows multiple assignment, assigning the items on the left of `=` _in order_ to the values on the right of `=` by forming tuples and then "unpacking" them. This exercise solution uses multiple assignment for the constant values NORTH, EAST, SOUTH, WEST. [origin](./exercise-concepts/robot-simulator.md)

### [Methods of list][methods-of-list]

- the use of methods of list could be useful in this exercise. Methods like `append`, `pop`... [origin](./exercise-concepts/variable-length-quantity.md)

### [Zip][zip]

- builtin that joins multiple iterables into a single one [origin](./exercise-concepts/hamming.md)
- the example solution for this exercise uses this function to aggregage the column-wise elements of each rown list to form the matrix "columns". [origin](./exercise-concepts/matrix.md)

### [Rich comparison methods][rich-comparison-methods]

- The `__eq__` method is overloaded [origin](./exercise-concepts/clock.md)

### [Inequality][inequality]

- this solution checks if `a` is not equal to `b`. [origin](./exercise-concepts/hamming.md)

### [String Methods][string-methods]

- strings (and other types) have built in instance methods - in this case, `"string".startswith("s")` which are called from the instance of the string itself [origin](./exercise-concepts/phone-number.md)
- this exercise uses `str.maketrans()` (a static method of `str` that returns a dictionary to create a _translation table_ as required by the `str.translate()` instance method. This method is unusual in that it takes either a single dictionary or two strings of equal length. The example solution for this exercise uses `str.maketrans()` with a two-string argument. [origin](./exercise-concepts/rna-transcription.md)

### [Loops][loops]

- concept required to solve the exercise [origin](./exercise-concepts/binary-search-tree.md)
- the `for ... in` syntax is useful for looping through a list or other iterable object [origin](./exercise-concepts/hamming.md)
- the knowledge of `while` and `for` loops are useful to solve this exercise [origin](./exercise-concepts/variable-length-quantity.md)

### [Builtin Function][builtin-functions]

- strings have a length, accessible by calling `len()`, a builtin python function [origin](./exercise-concepts/hamming.md)
- Python has several handy builtin functions in the stdlib that can operate on many types of data, e.g. `len()`, `max()`, `min()`. Under the hood these are implemented via dunder methods - if an object (and everything in Python is an object) implements the correct dunder methods (see that topic for more information), it can support use in these functions. (For example, if an object implements `__len__`, the len(<object>) will return that value.) Because these functions are not strictly tied to any data type, they can be used almost anywhere, and will crop up again and again as we learn Python. Docs: https://docs.python.org/3/library/functions.html [origin](./exercise-concepts/hamming.md)
- the `sum()` built-in function is a useful concept to solve the exercise [origin](./exercise-concepts/variable-length-quantity.md)
- the `len()` built-in function is a useful concept in this exercise [origin](./exercise-concepts/variable-length-quantity.md)
- the `enumerate` built-in function is a useful concept in this exercise [origin](./exercise-concepts/variable-length-quantity.md)

### [Tuple unpacking][tuple-unpacking]

- the values in an iterable can be unpacked into variables and used, i.e. `for a, b in zip(s1, s2)` [origin](./exercise-concepts/hamming.md)
- iterating through a list of tuples, i.e. [(1, 2), (2,3)], each piece of each tuple can be unpacked into a separate variable (syntax: `a, b = (1, 2)`); this works for any sort of iterable (lists, for example, and even strings!) but is commonly used with tuples because they are typically of a known size/length, and so can be safely unpacked into N variables, with names. [origin](./exercise-concepts/hamming.md)

### [Iteration][iteration]

- the passed-in string is iterated over, and split into rows. The row lists are iterated over and split into elements [origin](./exercise-concepts/matrix.md)
- the example uses a `for loop` to iterate through the letters of the passed-in `commands` string and looks up the corresponding values in a dictionary, so that the appropriate methods can be called to move the `robot`. [origin](./exercise-concepts/robot-simulator.md)
- the `for ... in` concept is useful to loop over the lists [origin](./exercise-concepts/variable-length-quantity.md)

### [Iteration][iterable]

- strings are iterable, which provides a lot of opportunity to leverage Python functions against them [origin](./exercise-concepts/hamming.md)
- understanding that strings, lists, and other data structures can be iterated over in the same fashion [origin](./exercise-concepts/matrix.md)
- characters in a string are *iterables* and are subject to index and slice access as described below [origin](./exercise-concepts/phone-number.md)
- The example solution uses the `for _ in _` syntax to iterate over a list of lines. This is possible because a list is an `iterable`. [origin](./exercise-concepts/markdown.md)

### [Raise][raise]

- the student is required to raise an `Exception` for incorrect input [origin](./exercise-concepts/hamming.md)
- an appropriate Exceptions must be raised with the `raise` keyword [origin](./exercise-concepts/phone-number.md)
- the user could find useful the `Exception` concept to `raise` a `ValueError` for incorrect input [origin](./exercise-concepts/variable-length-quantity.md)

### [Boolean Logic][boolean-logic]

- the exercise relies on `and`, `or`, and (optionally) `not` to form Boolean predicates [origin](./exercise-concepts/leap.md)
- the exercise relies on `and` and `or` to combine Boolean predicates into a single logical answer [origin](./exercise-concepts/leap.md)
- the `or` and `and` keywords are used [origin](./exercise-concepts/phone-number.md)

### [Binary Numbers][binary-numbers]

- binary numbers are a core important concept for this exercise [origin](./exercise-concepts/variable-length-quantity.md)

### [Composition][composition]

- adding functionality from a class by incorporating an instance of that class in a class you are creating. The example creates a `robot` by instantiating a `compass` and assigning it to the `self`.compass attribute of `robot`. [origin](./exercise-concepts/robot-simulator.md)

### [Enumerated Values][enumerated-values]

- the exercise relies on a fixed enumeration of possible values in a data structure [origin](./exercise-concepts/allergies.md)

### [Identity][identity]

- the best way to check if an element is `None` is via the _identity_ operator, `is` [origin](./exercise-concepts/binary-search-tree.md)

### [Equivalence][equivalence]

- the exercise relies on the `==` and `!=` operators to check that two values are equivalent (or not) [origin](./exercise-concepts/leap.md)

### [Namespaces][namespaces]

- knowing to use `self`.`<propertyname>` for instance properties and `self` as first argument to instance methods in a class [origin](./exercise-concepts/matrix.md)
- knowing to use `self.<propertyname>` for instance attributes and `self` as first argument to instance methods in a class. Additionally, the example uses `self.<methodname>()` to call a previously stored method name. [origin](./exercise-concepts/robot-simulator.md)

### [Exception hierarchy][exception-hierarchy]

- the idiomatic `Exception` type is a `ValueError`, meaning the input is incorrect [origin](./exercise-concepts/hamming.md)

### [String Splitting][string-splitting]

- The example solution uses `str.split()` to break the passed in markdown string into a list of lines broken up by the `\n` character. The alternate Python example solution uses `str.splitlines()` for the same effect across all line end characters. [origin](./exercise-concepts/markdown.md)
- the example uses `str.split` with and without seperators to break the passed in string into "rows" and then "elements" [origin](./exercise-concepts/matrix.md)

### [Default Arguments][default-arguments]

- pre-setting function arguments to protect against them not being passed by a caller. The example uses `direction = NORTH` and `x=0, y=0` to ensure those values for a `robot` even if they are not initally passed. [origin](./exercise-concepts/robot-simulator.md)

### [Method Parameters][method-parameters]

- the example `__init__` method has `self`, direction, x, and y (coordinates) as parameters. It also uses `self` and `commands` (a string) for parameters of the `move()` method. [origin](./exercise-concepts/robot-simulator.md)

### [Recursion][recursion]

- recursion is a core concept in this exercise [origin](./exercise-concepts/binary-search-tree.md)

### [Initialization][initialization]

- customizing object instatiation with actions and persisting data. The example uses `__init__` to persist a `compass` object and x, y coordinates assigned to instance attributes. [origin](./exercise-concepts/robot-simulator.md)

### [Range][range]

- the `range()` built-in represents an immutable sequence of numbers (or any object that implements the __index__ magic method). Used in the example to control the number of loops while iterating through a passed-in line or list. [origin](./exercise-concepts/markdown.md)
- the `range()` built-in type represents an immutable sequence of numbers (or any object that implements the `__index__` dunder method). Used in the example to represent the values from zero to 3 as assigned to NORTH, EAST, SOUTH, WEST. [origin](./exercise-concepts/robot-simulator.md)

### [Powers of Two][powers-of-two]

- the exercise relies on the use of powers of two in fundamental binary (bitwise) operations [origin](./exercise-concepts/allergies.md)

### [class members][class-members]

- student must know how members of a class work [origin](./exercise-concepts/binary-search-tree.md)

### [Higher-Order Function][higher-order-function]

- a function that takes one or more other functions as arguments, _returning_ a function as its return value. The example uses the built-in `property()` as a higher-order function through `@property`. [origin](./exercise-concepts/robot-simulator.md)

### [Generics][generics]

- the exercise is polymorphic across numerical types (ie int, float, Decimal) [origin](./exercise-concepts/leap.md)

### [Slicing][slicing]

- the extended solution to this exercise can employ a slice (returns a copy) instead of calling `.copy()`. [origin](./exercise-concepts/matrix.md)
- a slice within an iterable, i.e. the slice of items from `<iterable>[x]` to `<iterable>[y]`, can be accessed via `<iterable>[x:y]` notation; a third parameter allows "skipping" by `z`, i.e. `stringname[x:y:z]` [origin](./exercise-concepts/phone-number.md)
- becase `str` in Python is a sequence type, [slicing](https://docs.python.org/3/reference/expressions.html#slicings) syntax can be used here. Specifically: for syntax `string[start:stop:stride]`: [origin](./exercise-concepts/reverse-string.md)

### [Function Decorator][function-decorator]

- a higher-order function that takes another function as an argument. The "decorating" function extends the behavior of the "decorated" function without explicitly modifying it (i.e. it _wraps_ or _decorates_ it). Called in python by using the `@` symbol and the function name ahead of the function being decorated. The example uses pythons built-in `property()` as a decorator (`@property`) to return a "compound" read-only instance property made up of two separate instance attributes. [origin](./exercise-concepts/robot-simulator.md)

### [Objects][objects]

- creating different instances with different data representing different `matrices` is tested [origin](./exercise-concepts/matrix.md)

### [Order of Evaluation][order-of-evaluation]

- the exercise relies on parentheses to explicitly modify the normal order of evaluation of an expression [origin](./exercise-concepts/leap.md)

### [Methods][methods]

- the exercise relies on the `def` statement to create an instance method [origin](./exercise-concepts/allergies.md)
- use of `def` to define a class's methods [origin](./exercise-concepts/clock.md)
- classes can have instance *methods* which are called from an instance of the class (as opposed to class methods, called from the Class itself). The first parameter of an instance method is always `self`, which is provided when calling from the instance (i.e. the programmer does not need to pass it as an argument explicitly). Static methods are methods called from the class itself, and are not connected to an instance of the class. They have access to class attributes (those defined on the class, not connected to the `self`), and do not require an instance of the class to exist. Classes can also define a `property` by using the `@property` decorator (not shown here); a `property` can be "lazily evaluated" to avoid uneeded computation [origin](./exercise-concepts/phone-number.md)

### [Exception Message][exception-message]

- the user can use a custom error message inside the mentioned `ValueError` [origin](./exercise-concepts/variable-length-quantity.md)

### [Operator Precedence][operator-precedence]

- the exercise is most simply stated when the student understands the operator precedence binding rules of Python [origin](./exercise-concepts/leap.md)

### [Instance Attributes][instance-attributes]

- this exercise rquires one or more instance attributes to persist passed in data. [origin](./exercise-concepts/robot-simulator.md)

### [Implied Argument][implied-argument]

- the exercise relies on the implied passing of `self` as the first parameter of bound methods [origin](./exercise-concepts/allergies.md)
- student needs to know how to use statement `self` in a class [origin](./exercise-concepts/binary-search-tree.md)
- within the class definition, methods and properties can be accessed via the `self.` notation [origin](./exercise-concepts/phone-number.md)

### [Docstrings][docstrings]

- used to document the function, normally situated right below `def func():` [origin](./exercise-concepts/reverse-string.md)

### [Type Conversion][type-conversion]

- the passed in data is in `str` format but the output is expected as a list of type `int`. [origin](./exercise-concepts/matrix.md)

### [Mutability][mutability]

- in the extended example, knowing there are no protected or private properties in python and adjusting coding patterns [origin](./exercise-concepts/matrix.md)
- in the example, knowing there are no protected or private properties in python and so consciously mutating `self.x`, `self.y` and `self.compass` through the called instance methods. [origin](./exercise-concepts/robot-simulator.md)

### [Call Semantics][call-semantics]

- knowing that appending `()` to the name of an instance method _calls_ it, since instance methods are _callable_. [origin](./exercise-concepts/robot-simulator.md)

### [Exception catching][exception-catching]

- `Exceptions` can be caught from outside the scope where they are raised, using the `try/except` syntax. All `Exceptions` types inherit from the base class, `Exception` and thus can be caught by either checking specifically for the type of Exception, or for any Exception [origin](./exercise-concepts/hamming.md)

### [Property][property]

- the `property()` built-in is a function that returns a property attribute. When used as a decorator, this transforms the passed-in method into a _getter_ method for read-only attribute with the same name and docstring. [origin](./exercise-concepts/robot-simulator.md)
