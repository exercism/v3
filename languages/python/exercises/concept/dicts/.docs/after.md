Python dictionaries are very powerful and you will be using them a lot if you're working with APIs and such. Dictionaries are essentially an array of _keys_ paired with _values_.

Just like `lists`, dictionaries are ordered by insertion order, they are mutable (which means you can change their content without changing their identity). They can also (just like `lists`) be nested, which means that a dictionary can contain another dictionary. Dictionaries are generally used in databases for use cases where looking up things by _key_ are used over and over. Although they are not generally used for use cases with demand for high-speed insertion. This [blog post][listsvdicts] by _Jessica Yung_ goes into what data type is prefered in what use cases, on a more _scientific_ level.

Now that you know the basics of _creation_, _membership_ and _retrieval_ here are some useful `dict` methods. You can get values from a dictionary by using the `.get(key, [default])`, which returns the value of the given key in the dictionary, if the key does not exist it returns the `default` value. Dictionaries also have the `.setdefault(key, [default])` method, which is almost the same as `.get()`, but it also places that key with the default value if it does not exist inside the dictionary.

[listsvdicts]: https://www.jessicayung.com/python-lists-vs-dictionaries-the-space-time-tradeoff/
