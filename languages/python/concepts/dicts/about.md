A dictionary in Python is a data structure that associates [hashable][term-hashable] _keys_ to _values_ and can be known in other programming languages as a [hash table or hashmap][hashtable-wikipedia]. In Python, it's considered a [mapping type][mapping-types-dict]. `dicts` enable the retrieval of a value in constant time, given the key.

Compared to searching for a value within a list or array (_without knowing the index position_), a dictionary uses significantly more memory, but has very rapid retrieval. It's especially useful in scenarios where the collection is large and must be accessed frequently.

## Keys and Values

A dictionary can be though of as a collection of straightforward `key`:`value` pairs. Like other collection types (`list`, `tuple`, and `set`), `values` can contain arbitrary data and data types, including nested dictionaries. A dictionary’s `keys` are _**almost**_ as arbitrary, but require their data types to be _hashable_.

Keys are unique across a dictionary, and the values associated with any `key` can be replaced, updated or altered as long as the `key` exists. To be a valid, a key (_and all its contained values_) must be [hashable][term-hashable]. Hashable `keys` can include numbers, strings, tuples of _immutable_ values, or frozensets. `sets`, `lists`, and `dicts` are unhashable and therefor cannot be used as keys.

A value can be of any data type, including Build-in types, custom types or complex objects (even functions or classes). Commonly used objects are: numbers, strings, lists, dictionaries, tuples or sets.

A simple `dict` can be declared using the literal form `{"key_1": value_1, "key_2": value_2}` or via _constructor_ with `dict(key_1=value_1, key_2=value_2)`, but there are many more ways of creating and initializing dictionaries including the use of a _dict comprehension_ or passing additional constructor parameters as illustrated in the [Python docs][mapping-types-dict].

Inserting a new `key`:`value` pair can be done with `dict[key] = value` and the value can be retrieved by using `retrieved_value = dict[key]`.

## Methods

`dicts` implement various methods to allow easy initialization, updating and viewing.

Some useful `dict` methods:

- Retrieve a value "safely" from a dictionary by using the `.get(key, [default])` method. `.get(key, [default])` returns the value for the key **or** the _default value_ if the key is not found, instead of raising a `KeyError`. This works well in situations where you would rather not have extra error handling but cannot trust that a looked-for key will be present.
- Retrieve a value "safely" or insert a default _value_ if the key is not found using the `.setdefault(key, [default])` method. `setdefault(key, [default])` will insert the default value in the dictionary **only** if the key is not found, then it will retrieve either the **newly inserted** default value if the key was not found or the **unchanged** existing value if the key was found.
- Return various _iterable_ views of your `dict` with `.keys()`, `.values()`, `.items()` (_an iterable of (key, value) `tuples`_).

For a detailed explanation of dictionaries in Python, the [official documentation][dicts-docs] is an excellent starting place, or you can also check out the [W3-Schools][how-to-dicts] tutorial.

## Extending Dictionaries: The collections module

The [`collections`][collections-docs] module adds more functionality to Python's standard collection-based datatypes (`dictionary`, `set`, `list`, `tuple`). A popular `dict`-oriented member of this module is the [`Counter`][counter-dicts], which automatically counts items and returns them a `dict` with the items as keys and their counts as values. There is also the [`OrderedDict`][ordered-dicts-docs], which has methods specialized for re-arranging the order of a dictionary. Finally, there is the [`defaultdict`][default-dicts], a subclass of the built-in `dict` module that, based on a factory method, sets a default value if a key is not found when trying to retrieve or assign the value.

[term-hashable]: https://docs.python.org/3/glossary.html#term-hashable
[hashtable-wikipedia]: https://en.wikipedia.org/wiki/Hash_table
[mapping-types-dict]: https://docs.python.org/3/library/stdtypes.html#mapping-types-dict
[dicts-docs]: https://docs.python.org/3/tutorial/datastructures.html#dictionaries
[how-to-dicts]: https://www.w3schools.com/python/python_dictionaries.asp
[collections-docs]: https://docs.python.org/3/library/collections.html
[counter-dicts]: https://docs.python.org/3/library/collections.html#collections.Counter
[ordered-dicts-docs]: https://docs.python.org/3/library/collections.html#collections.OrderedDict
[default-dicts]: https://docs.python.org/2/library/collections.html#collections.defaultdict
