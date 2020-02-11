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
 - classes require an `__init__` method, whose first argument is `self`, because the result of `__init__` is a class *instance*
 - classes can have instance *methods* which are called from an instance of the class (as opposed to class methods, called from the Class itself)
 - classes can define a `property` by using the `@property` decorator (not shown here)
 - a `property` can be "lazily evaluated" to avoid uneeded computation
 - "privacy" in Python: Methods prefixed with an underscore, `_`, are conventionally treated as private methods, although Python does not actually support privacy.
 - within the class definition, methods and properties can be accessed via the `self.` notation

**Python stdlib**
 - the `re` module is an example of the Python stdlib (standard library), or included code libraries and tools that are frequently used in Python
 - to use the module, the `import` syntax can be used

**strings**
 - Characters in a string can be accessed with *bracket notation* since they are iterable
 - strings are immutable, and so cannot have values assigned
 - new strings can be created, however
 - `<string>.format()` can be used to replace values in a string
 - `"s" in "string"` syntax allows the user to check membership in the longer string

**string methods**
 - strings (and other types) have built in instance methods - in this case, `"string".startswith("s")` which are called from
   the instance of the string itself

**bracket notation - index access**
 - for iterables, individual items can be accessed with `stringname[x]` notation
 - negative numbers start to count backwards

**bracket notation - slice access**
- ranges of iterables can be accessed via `stringname[x:y]` notation
- a third parameter allows "skipping" by `z`, i.e. `stringname[x:y:z]`

**regex**
 - regular expressions is a language of sorts that can detect substrings and extract groups from a string, as well as replace them with something else

**conditionality**
 - `if ... else` and `elif` allow a programmer to switch code branches depending on some condition

**or / and**
**Exceptions**
