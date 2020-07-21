Python is a [dynamic and strongly](https://stackoverflow.com/questions/11328920/is-python-strongly-typed) typed [object-oriented](https://en.wikipedia.org/wiki/Object-oriented_programming) programming language. It supports both imperative (_object-oriented, procedural_) and declarative (_functional, concurrent_) programming paradigms. Python emphasizes code readability and (similar to Haskell) uses significant indentation.

Objects are [assigned](https://docs.python.org/3/reference/simple_stmts.html#assignment-statements) to names in Python via the `=` or _assignment operator_. _Variables_ are written in [`snake_case`](https://en.wikipedia.org/wiki/Snake_case), and _constants_ (usually) in `SCREAMING_SNAKE_CASE`. A name (_variable or constant_) is not itself _typed_, and can be attached or re-attached to different objects over its lifetime:

```python
>>> my_first_variable = 1
>>> my_first_variable = {"bear" : "brown", "cat": "black"}
>>> my_first_variable = collections.Counter()
>>> my_first_variable = "Last one, I promise"
>>> print(my_first_variable)
"Last one, I promise"
```

Constants are usually defined at the top of a script or file, and although they _can_ be changed, they are _intended_ to be named only once. Their `SCREAMING_SNAKE_CASE` is a message to other developers that the assignment should not be altered:

```python
# All caps signal that this is intended as a constant
MY_FIRST_CONSTANT = 16

# Re-assignment will be allowed by the compiler & interpreter,
# but is VERY strongly discouraged.
# Please don't do: MY_FIRST_CONSTANT = "Some other value"
```

In Python, units of functionality are encapsulated in [functions](https://docs.python.org/3/reference/compound_stmts.html#function). When functions are bound to a [class](https://docs.python.org/3/reference/datamodel.html#classes) name, they're referred to as [methods](https://docs.python.org/3/c-api/method.html#method-objects).

The keyword `def` begins a [function definition](https://docs.python.org/3/tutorial/controlflow.html#defining-functions). It must be followed by the function name and a parenthesized list of zero or more formal [parameters](https://docs.python.org/3/glossary.html#term-parameter). The `def` line is terminated with a colon. Statements for the _body_ of the function begin on the next line, and must be _indented in a block_. Functions explicitly return a value or object via the `return` keyword:

```python
#function definition on first line.
def add_two_numbers(number_one, number_two):
    return number_one + number_two  #returns the sum of the numbers

>>> add_two_numbers(3, 4)
7
```

Functions are _called_ using their name followed by `()`. The number of arguments passed in the parentheses must match the number of parameters in the original function definition:

```python
def number_to_the_power_of(number_one, number_two):
    '''Returns float or int.

       Takes number_one and raises it to the power of number_two, returning the result.
    '''
    return number_one ** number_two

>>> number_to_the_power_of(3,3)
27

>>> number_to_the_power_of(4,)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: number_to_the_power_of() missing 1 required positional argument: 'number_two'
```

Comments in Python start with a `#` (_hash character_) that is not part of a string, and end at line termination. Unlike many other programming languages, Python does not support multi-line comment marks. Each line of a comment block must start with the `#` character. Comments are ignored by the interpreter:

```python
#this is a single line comment

x = "foo"  #this is an in-line comment

#this is a multi-line
#comment block over multiple lines
#these should be used sparingly
```

The first statement of a function body can optionally be a _docstring_, which concisely summarizes the function or object's purpose. These docstrings are read by automated documentation tools and can be accessed from code. They are recommended for programs of any size where documentation is needed.

```python
def number_to_the_power_of(number_one, number_two):
    '''Returns float or int.

       Takes number_one and raises it to the power of number_two, returning the result.
    '''
    return number_one ** number_two

>>> print(number_to_the_power_of.__doc__)
Returns float or int.

       Takes number_one and raises it to the power of number_two, returning the result.

>>> print(str.__doc__)
str(object='') -> str
str(bytes_or_buffer[, encoding[, errors]]) -> str

Create a new string object from the given object. If encoding or
errors is specified, then the object must expose a data buffer
that will be decoded using the given encoding and error handler.
Otherwise, returns the result of object.__str__() (if defined)
or repr(object).
encoding defaults to sys.getdefaultencoding().
errors defaults to 'strict'.
```
