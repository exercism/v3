# Concepts for robot-simulator

[Example Implementation](https://github.com/exercism/python/blob/master/exercises/robot-simulator/example.py)


## General

- constants: Are not enforced by the runtime, but are used via the convention of `UPPER_CASE` to signal that these values are expected to remain unchanged.  Preferably, constants are defined at a module level. The example solution uses the `UPPER_CASE` convention to define NORTH, SOUTH, EAST, and WEST constants. 

- multiple assignment:  Python allows multiple assignment, assigning the items on the left of `=` _in order_ to the values on the right of `=` by forming tuples and then "unpacking" them.  This exercise solution uses multiple assignment for the constant values NORTH, EAST, SOUTH, WEST.

- `range()` built-in type:  Represents an immutable sequence of numbers (or any object that implements the `__index__` magic method).  Used in the example to represent the values from zero to 3 as assigned to NORTH, EAST, SOUTH, WEST. 

- classes: The exercise objective is to define a `robot` type.  Tested methods are linked to a `robot` class.

- instantiation: Creating different instances of the `robot` class with different data representing different starting positions and bearing are tested.

- initialization: Customizing object instatiation with actions and persisting data. The example uses `__init__` to persist a `compass` object and x, y coordinates assigned to instance attributes.

- return values:  Knowing that functions need not have _explicit_ return statements or values but will return `None` if `return` is not specified.  Except for the two `@property`-decorated functions, all of the functions in the example omit an explicit `return` statment and all return `None`.

- self: The example uses `self` for methods and properties linked to a specific instance of the class.

- namespaces: Knowing to use `self.<propertyname>` for instance attributes and `self` as first argument to instance methods in a class.  Additionally, the example uses `self.<methodname>()` to call a previously stored method name.
  
- instance methods: Tests for this exercises require one or more instance methods that will take in a set of starting coordinates and a bearing and then accept a series of instructions that "move" the instance to a new set of coordinates and bearing.

- function decorator: a higher-order function that takes another function as an argument. The "decorating" function extends the behavior of the "decorated" function without explicitly modifying it (i.e. it _wraps_ or _decorates_ it). Called in python by using the `@` symbol and the function name ahead of the function being decorated.  The example uses pythons built-in `property()` as a decorator (`@property`) to return a "compound" read-only instance property made up of two separate instance attributes.

- higher-order functions:  a function that takes one or more other functions as arguments, _returning_ a function as its return value.  The example uses the built-in `property()` as a higher-order function through `@property`.

- `property()` built-in:  a function that returns a property attribute.  When used as a decorator, this transforms the passed-in method into a _getter_ method for read-only attribute with the same name and docstring.

- assignment:  the example uses assignment for all the instance properties and `instructions` dictionary.

- instance attributes: This exercise rquires one or more instance attributes to persist passed in data.

- mutability: In the example, knowing there are no protected or private properties in python and so consciously mutating `self.x`, `self.y` and `self.compass` through the called instance methods.

- method parameters: the example `__init__` method has `self`, direction, x, and y (coordinates) as parameters.  It also uses `self` and `commands` (a string) for parameters of the `move()` method.

- default arguments:  Pre-setting function arguments to protect against them not being passed by a caller.  The example uses `direction = NORTH` and `x=0, y=0` to ensure those values for a `robot` even if they are not initally passed.

- dictionaries/mapping type: the example uses a dictionary to map paassed in move arguments to methods that perform the moves.  The example also uses a dictionary/mapping created by calling `str.maketrans()`.

- indexing/lookup for dictionaries: Finding a value by key in a dictionary using `<dictionary name>[<key name>]` The example uses passed in move arguments as `keys` to look up corresponding `values` (_method names_) for moving the robot in the _instructions_ dictionary. 

- iteration: the example uses a `for loop` to iterate through the letters of the passed-in `commands` string and looks up the corresponding values in a dictionary, so that the appropriate methods can be called to move the `robot`.

- composition:  adding functionality from a class by incorporating an instance of that class in a class you are creating.  The example creates a `robot` by instantiating a `compass` and assigning it to the `self`.compass attribute of `robot`.

- modulus operator: the example uses the modulus operator to calculate the appropriate _compass_ setting when calling the `right` compass method.

- lists:  the example stores compass direction constants in a `list`

- call semantics:  knowing that appending `()` to the name of an instance method _calls_ it, since instance methods are _callable_.

- equality operator (`==`) :  `==` calls the magic method `__eq__()`.  By default, objects of different types are never considered equal to each other unless they are numerical types (ie int, float) or have otherwise overloaded the default implementation of the `__eq__` magic method.  `==` is always defined, but for some object types (_like class objects_) it is equivalent to calling `is`.  See [`__eq__`](https://docs.python.org/3/reference/datamodel.html#object.__eq__)  for additional details.  This exercise uses the equality operator to test that the `self.direction` attribute is equal to one of the constant values defined.

- magic methods:  The example uses `__init__` as a constructor for the class, which also calls `__new__`.  In addition, the example uses `__call__()` via the appending of `()` to instance method names, and `__eq__()` (_rich compairison_) via the use of `==`

