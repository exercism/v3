## General

- [The Python Dictionary Tutorial](https://docs.python.org/3/tutorial/datastructures.html#dictionaries) can be a great introduction.

## 1. Create the dictionary

- You need a [for loop](https://docs.python.org/3/tutorial/controlflow.html#for-statements) to iterate the list of items, then insert each item in the dictionary if missing and increment the item count using the dictionary accessor.
- You can use [`setdefault`](https://www.w3schools.com/python/ref_dictionary_setdefault.asp) to make sure the value is set before incrementing the count of the item.
- This function should [return a dict](https://www.w3schools.com/python/ref_keyword_return.asp).

## 2. Add items to a dictionary

- You need a [for loop](https://docs.python.org/3/tutorial/controlflow.html#for-statements) to iterate the list of items, then insert each item if not already in the dictionary and [increment](https://www.w3schools.com/python/gloss_python_assignment_operators.asp) the item count using the dictionary accessor.
- You can use [`setdefault`](https://www.w3schools.com/python/ref_dictionary_setdefault.asp) to make sure the value is set before incrementing the count of the item.
- The function `add_items` can be used by the `create_inventory` function with an empty dictionary in parameter.
- This function should [return a dict](https://www.w3schools.com/python/ref_keyword_return.asp).

## 3. Delete items from a dictionary

- You need [for loop](https://docs.python.org/3/tutorial/controlflow.html#for-statements) to iterate the list of items, if the number of items is not `0` then [decrement](https://www.w3schools.com/python/gloss_python_assignment_operators.asp) the current number of items.
- You can use the `key in dict` that returns `True` if the key exists to make sure the value is in the dictionary before decrementing the number of items.
- This function should [return a dict](https://www.w3schools.com/python/ref_keyword_return.asp).

## 4. List the items that are in stock

- You need [for loop](https://docs.python.org/3/tutorial/controlflow.html#for-statements) on the inventory and if the number of item is greater of `0` then append the tuple to a list.
- You can use `dict.items()` to iterate on both the item and the value at the same time, `items()` returns a tuple that you can deconstruct or use as it is.
- This function should [return](https://www.w3schools.com/python/ref_keyword_return.asp) a [list](https://www.w3schools.com/python/python_lists.asp) of [tuples](https://www.w3schools.com/python/python_tuples.asp).
