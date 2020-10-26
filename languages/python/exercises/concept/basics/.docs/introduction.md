Python is a dynamically (yet _strongly_) typed programming language. It supports multiple programming paradigms including many **imperative** (*structured, procedural, object-oriented, and aspect-oriented*) and **declarative** (_functional_) flavors. Python emphasizes code readability and (*similar to Haskell*) uses significant whitespace.

[Everything in Python is an object](https://docs.python.org/3/reference/datamodel.html#data-model).  Objects are assigned to [names](https://docs.python.org/3/reference/executionmodel.html#naming-and-binding) in Python via the `=` or _assignment operator_. _Variables_ are written in [`snake_case`](https://en.wikipedia.org/wiki/Snake_case), and _constants_ (usually) in `SCREAMING_SNAKE_CASE`. A name (_variable or constant_) is not itself _typed_, and can be [attached or re-attached](https://docs.python.org/3/reference/simple_stmts.html#assignment-statements) to different objects over its lifetime:

```python
>>> my_first_variable = 1
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

In Python, units of functionality are encapsulated in *functions*.

The keyword `def` begins a [_function definition_](https://docs.python.org/3/reference/compound_stmts.html#function-definitions), and must be followed by the _function name_ and a parenthesized list of zero or more formal _parameters_. The `def` line is terminated with a colon. 



Statements for the _body_ of the function begin on the next line, and must be _indented in a block_. There is no strict indentation amount (_either space **OR** [tab] characters are acceptable_), but indentation must be _consistent for all indented statements_. Functions explicitly return a value or object via the [`return`](https://docs.python.org/3/reference/simple_stmts.html#the-return-statement) keyword:

```python
#function definition on first line.
def add_two_numbers(number_one, number_two):
  return number_one + number_two  #returns the sum of the numbers, and is indented by 2 spaces.

>>> add_two_numbers(3, 4)
7

#the return statement line does not match the first line indent
>>> def add_three_numbers_misformatted(number_one, number_two, number_three):
...     result = number_one + number_two + number_three   #indented by 4 spaces
...    return result     #this was only indented by 3 spaces
  File "<stdin>", line 3
    return result
                ^
IndentationError: unindent does not match any outer indentation level
```

Functions are _called_ using their name followed by `()`. The number of arguments passed in the parentheses must match the number of parameters in the original function definition:

```python
def number_to_the_power_of(number_one, number_two):
    return number_one ** number_two

>>> number_to_the_power_of(3,3)
27

>>> number_to_the_power_of(4,)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: number_to_the_power_of() missing 1 required positional argument: 'number_two'
```

[Comments](https://www.python.org/dev/peps/pep-0008/#comments) in Python start with a `#` that is not part of a string, and end at line termination. Unlike many other programming languages, Python does **not** support multi-line comment marks. Each line of a comment block must start with the `#` character. Comments are ignored by the interpreter:

```python
#this is a single line comment

x = "foo"  #this is an in-line comment

#this is a multi-line
#comment block over multiple lines
#these should be used sparingly
```

The first statement of a function body can optionally be a[ _docstring_](https://www.python.org/dev/peps/pep-0257/), which concisely summarizes the function or object's purpose. These docstrings are read by automated documentation tools and can be accessed from code via the special `__doc__` attribute. They're recommended for programs of any size where documentation is needed.

```python
def number_to_the_power_of(number_one, number_two):
    '''Returns float or int.

       Takes number_one and raises it to the power of number_two, returning the result.
    '''
    return number_one ** number_two

>>> print(number_to_the_power_of.__doc__)
Returns float or int.

       Takes number_one and raises it to the power of number_two, returning the result.
```
