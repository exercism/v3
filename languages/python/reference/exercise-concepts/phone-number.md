[Example Solution](https://github.com/exercism/python/blob/master/exercises/phone-number/example.py)

```
import re


class PhoneNumber:
    def __init__(self, number):
        self.number = self._clean(number)
        self.area_code = self.number[:3]
        self.exchange_code = self.number[3:6]
        self.subscriber_number = self.number[-4:]

    def pretty(self):
        return "({}) {}-{}".format(
            self.area_code, self.exchange_code, self.subscriber_number
        )

    def _clean(self, number):
        return self._normalize(re.sub(r"[^\d]", "", number))

    def _normalize(self, number):
        if len(number) == 10 or len(number) == 11 and number.startswith("1"):
            valid = number[-10] in "23456789" and number[-7] in "23456789"
        else:
            valid = False

        if valid:
            return number[-10:]
        else:
            raise ValueError("{} is not a valid phone number".format(number))
```

## Concepts

**Classes**
 - classes are defined with the `class <ClassName>:` syntax
 - User defined classes can (and generally do) overload the `__init__` method, whose first argument is `self`, because the result of `__init__` is a class *instance*. This is inherited from `Object`, which every class in Python inherits from. (See: inheritance)
 - classes can have instance *methods* which are called from an instance of the class (as opposed to class methods, called from the Class itself). The first parameter of an instance method is always `self`, which is provided when calling from the instance (i.e. the programmer does not need to pass it as an argument explicitly).
 - Static methods are methods called from the class itself, and are not connected to an instance of the class. They have access to class attributes (those defined on the class, not connected to the `self`), and do not require an instance of the class to exist.
 - classes can define a `property` by using the `@property` decorator (not shown here)
 - a `property` can be "lazily evaluated" to avoid uneeded computation
 - Public and Private methods/attributes in Python: Methods or attributes (including those of an imported module) prefixed with an underscore, `_`, are conventionally treated as private methods, although Python does not actually support privacy in the way a language like Java does. Convention indicates that methods and attributes that are not prefixed can be expected to remain stable along with semver, i.e. a public method will be backwards compatible with minor version updates, and can change with major version updates. Generally, importing non-public functions or using non-public methods is discouraged, though Python will not explicitly stop the programmer from doing so.
 - within the class definition, methods and properties can be accessed via the `self.` notation
 - Inheritance: a "subclass" will inherit all methods, attributes from it's parent class, and can then override methods as needed. Overriding means the logic in the parent class is not used.
 - `super()` : a subclass can call `super()`, a builtin method, which will allow the programmer to defer logic up the inheritance chain to the parent class when needed.

**Python stdlib**
 - the `re` module is an example of the Python stdlib (standard library), or included code libraries and tools that are frequently used in Python
 - to use the module, the `import` syntax can be used

**strings**
 - Characters in a string are *iterables* and are subject to index and slice access as described below
 - strings are immutable, and so cannot have values assigned
 - new strings can be created, however
 - `<string>.format()` can be used to replace values in a string
 - `"s" in "string"` syntax allows the user to check membership in the longer string

**string methods**
 - strings (and other types) have built in instance methods - in this case, `"string".startswith("s")` which are called from the instance of the string itself

**iterables - index access**
 - for iterables, individual items can be accessed with `stringname[x]` notation
 - negative numbers start to count backwards

**iterables - slice access**
- A slice within an iterable, i.e. the slice of items from `<iterable>[x]` to `<iterable>[y]`, can be accessed via `<iterable>[x:y]` notation
- A third parameter allows "skipping" by `z`, i.e. `stringname[x:y:z]`

**regex**
 - regular expressions is a language of sorts that can detect substrings and extract groups from a string, as well as replace them with something else

**conditionality**
 - `if ... else` and `elif` allow a programmer to switch code branches depending on some condition

**or / and**
**Exceptions**
