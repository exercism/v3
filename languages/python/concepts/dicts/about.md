A dictionary is a data structure that associates a key to a value. In Python, it's called the [mapping type dict][mapping-types-dict], this data type enables to retrieve the value object quickly knowing the key. Comparing with searching an object in the items a list, a dictionary would use more memory, but would be faster, especially for large collections.

## Keys and Values

A dictionary can be though of as a simple key:value pair mapping, it can be declared using `{k1: v1, k2: v2}` or `dict(k1=v1, k2=v2)`, but there is much more ways of creating and initializing a dictionary including dict comprehensions or other constructor parameters that are illustrated on the [official docs][mapping-types-dict]. Inserting a key:value pair can be done with `dict[key] = value` and the retrieval of a value by `v = dict[k]`.

A key is unique across a dictionary, but the value associated with it can be replaced, updated or altered as long as that key exists. To be a valid key, the key should be [hashable][term-hashable], hashable keys include for example numbers, strings or tuples of immutable values.

A value can be of any data type, including Build-in types, custom types or complex objects. Common used objects are: numbers, string, list, dictionary or sets.

## Methods

Dictionaries implement various methods to allow easy initializing, updates and viewing, for example getting a value from a specific key in the dictionary, setting a default value for a key and more. Dictionaries are also iterable by their keys, values or key-value pairs.

Some useful `dict` methods that are useful:

- Get a value from a dictionary by using the `.get(key, [default])` returns the value for a given key, would not error and returns the `default` value if the key is missing.
- Set a default value for a key with the `.setdefault(key, [default])` method, which is similar to the `.get()`, but it additionally inserts that key with the default value if it does not exist inside the dictionary.
- Return an iterable view of the keys with `.keys()`, of the values with `.values()` and of the `(key, value)` pairs as tuples with `.items()`.

For a complete explanation of dictionaries in python refer to the [official documentation][dicts-docs] or check out the [W3-Schools][how-to-dicts] tutorial.

## The collections module

The [`collections`][collections-docs] module adds more functionality to Python's standard collection-based datatypes (`dictionary`, `set`, `list`, `tuple`). A popular member of this module is the [`Counter`][counter-dicts] class, which counts items and returns it as a dictionary. There is also [`OrderedDict`][ordered-dicts-docs] which has methods specialized for re-arranging the order of a dictionary. Finally, there is [`defaultdict`][default-dicts] a subclass of the built-in `dict` module that sets a default value for any new key.

[term-hashable]: https://docs.python.org/3/glossary.html#term-hashable
[mapping-types-dict]: https://docs.python.org/3/library/stdtypes.html#mapping-types-dict
[dicts-docs]: https://docs.python.org/3/tutorial/datastructures.html#dictionaries
[how-to-dicts]: https://www.w3schools.com/python/python_dictionaries.asp
[collections-docs]: https://docs.python.org/3/library/collections.html
[counter-dicts]: https://docs.python.org/3/library/collections.html#collections.Counter
[ordered-dicts-docs]: https://docs.python.org/3/library/collections.html#collections.OrderedDict
[default-dicts]: https://docs.python.org/2/library/collections.html#collections.defaultdict
