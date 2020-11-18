## enums

In Python, [an enum](https://docs.python.org/3/library/enum.html) is a set of names that are bound to unique `literal`, or `constant` values. Enums are defined by inheriting an `Enum` class. Built-in enum types are available in `enum` and `Enum` can be imported with `from enum import Enum`.

```python
class Color(Enum):
    RED = 1
    GREEN = 2
```

Note that the values of the enum members can be any data types such as str, tuple, float, etc.

```python
class Color(Enum):
    RED = 'red'
    GREEN = 'green'
```

When you assign the same value to 2 members in an enum, the latter assigned member will be an alias to the formed one.

```python
class Color(Enum):
    RED = 1
    GREEN = 2
    ALIAS_OF_RED = 1

Color.ALIAS_OF_RED
#=> <Color.RED: 1>

Color.ALIAS_OF_RED.value
#=> 1
```

You can iterate through the members of the enum:

```python
for member in Color:
    print((member.name, member.value))
#=> (RED, 1)
#=> (GREEN, 2)
```

You can check and compare enum members by using the [`is` (called identity)](https://www.w3schools.com/python/ref_keyword_is.asp) or `is not` operators. You can also use the `==` or `!=` operator.

```python
a = Color.RED

a is Color.RED
#=> True

a == Color.RED
#=> True
```

You can get the enum member based on a value using:

```python
g = Color(2)

g is Color.GREEN
#=> True

g
#=> <Color.GREEN: 2>
```
