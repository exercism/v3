# Concepts for robot-simulator

[Example Implementation]():


## General

- contstants: Are not enforced by the runtime, but are used via the convention of `<ALL_CAPS_NAMING>` to signal that these values are expected to remaine unchanged.  Perferably, constants are defined at a module level. The example solution uses the ALL_CAPS convention to define NORTH, SOUTH, EAST, and WEST constants. 

- multiple assignment:  Python allows multiple assignment, assiging the items on the left of `=` _in order_ to the values on the right of `=`.  Equivelent to forming tuples of each side and then "unpacking" them.  This exercise solution uses multiple assignment for the contant values NORTH, EAST, SOUTH, WEST.

- `range()` built-in type:  Represents an immutable sequence of numbers (or any object that implements the `__index__` magic method).  Used in the example to represnet the values from zero to 3 as assigned to NORTH, EAST, SOUTH, WEST. 

- classes: The exercise objective is to define a `robot` type.  Tested methods are linked to a `robot` class.
- objects: Creating different instances of the `robot` class with different data representing different starting positions and bearing are tested.

- constructor: Customizing object initalization with actions and persisting data. The example uses a constructor (via `__init__` + `__new__`) to persist a `compass` object and x, y coordinates assigned to instance properties.

- retun values:  Knowing that functions need not have explicit return statements or values but will return `none` if `return` is not specified.  Except for the two `@property` functions, all of the functions in the example omit an explicit `return` statments, and all return `none`.

- self: The example uses this keyword for methods and properties linked to a specific instance of the class.

- namespaces: Knowing to use self.<propertyname> for instance properties and self as first argument to instance methods in a class.  Additionally, the example uses `self.<methodname>()` to call a previously stored function name.
  
- instance methods: Tests for this exercises require one or more instance methods that will take in a set of starting coordinates and a bearing and then accept a seiries of instructions that "move" the instance to a new set of coordinates and bearing.

- function decorators:

- `@property` decorator:

- getters:

- instance properties: This exercise rquires one or more instance properties to persist passed in data.
- mutability: In the example, knowing there are no protected or private properties in python and so conciously mutating `self.x`, `self.y` and `self.compass` through the called instance methods.

- method arguments:
- default arguments:
- dictionaries:
- indexing:
- bracket notation in context of dictionaries:
- iterators:
- composition:  adding functionality from a class by incorporating and instance of that class in a class you are creating
- dictionaries/mapping typ
- modulous operator:
- for loop:

- callable objects:  knowing that appending `()` to the name of an instance method _calls_ it, since instances of classes are _callable_, and implement `__call__()`.

- compairison operator built-in (`==` or equal) :  `==` calls the magic method `__equ__()`.  Knowing that objects of different types never compare equal unless they are numerica types.  `==` is always defined, but for some object types (_like class objects_) it is equivelent to calling `is()`.  This excercise uses the compairison operator to test that the `self.direction` attribute is equal to one of the constant values defined.

- magic methods:  The example uses `__init__` as a constructor for the class, which also calls `__new__`.  In addition, the example uses `__call__()` via the appending of `()` to instance method names, and `__eq__()` (_rich compairison_) via the use of `==`

