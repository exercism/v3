In Python, [an enum][enum-docs] is a set of names that are bound to unique `literal`, or `constant` values. Enums are defined by inheriting an `Enum` class. Built-in enum types are available in `enum` and `Enum` can be imported with `from enum import Enum`.

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

Enums can also be created via the following [functional API][enum-functional-api].

```python
Animal = Enum('Animal', 'ANT BEE CAT DOG')
list(Animal)
#=> [<Animal.ANT: 1>, <Animal.BEE: 2>, <Animal.CAT: 3>, <Animal.DOG: 4>]

Animal.ANT.value
#=> 1
```

When you assign the same value to 2 members in an enum, the latter assigned member will be an alias to the formed one.

```python
class Color(Enum):
    RED = 1
    GREEN = 2
    ALIAS_OF_RED = 1

Color.ALIAS_OF_RED
#=> <Color.RED: 1>
```

You can iterate through the members of the enum:

```python
for member in Color:
    print((member.name, member.value))
#=> (RED, 1)
#=> (GREEN, 2)

# __members__.items() helps you to loop through alias as well
for member in Color.__members__.items():
    print(member)
#=>('RED', <Color.RED: 1>)
#=>('GREEN', <Color.GREEN: 2>)
#=>('ALIAS_OF_RED', <Color.RED: 1>)
```

You can check and compare enum members by using the [`is` (called identity)][identity-keyword] or `is not` operators. You can also use the `==` or `!=` operator.

```python
a = Color.RED

a is Color.RED
#=> True

a == Color.RED
#=> True
```

You can use the [`auto()` function][enum-auto-docs] to automatically assign starting at 1 and increment subsequent values.

```python
class Shape(Enum):
    CIRCLE = auto()
    SQUARE = auto()
    OVAL = auto()
```

If you want to disallow aliases, and want to keep the members of the enum unique, you can use the `@unique` decorator.

```python
@unique
class Shape(Enum):
    CIRCLE = 1
    SQUARE = 2
    TRIANGLE = 1
#=> ValueError: duplicate values found in <enum 'Shape'>: TRIANGLE -> CIRCLE
```

You can define your own [restricted `Enum`][restricted-enums] by subclassing `Enum` with any mix-in or data-type.

```python
class StrEnum(str, Enum):
    pass
```

[enum-docs]: https://docs.python.org/3/library/enum.html
[enum-auto-docs]: https://docs.python.org/3/library/enum.html#using-auto
[enum-functional-api]: https://docs.python.org/3/library/enum.html#functional-api
[restricted-enums]: https://docs.python.org/3/library/enum.html#restricted-enum-subclassing
[identity-keyword]: https://www.w3schools.com/python/ref_keyword_is.asp
