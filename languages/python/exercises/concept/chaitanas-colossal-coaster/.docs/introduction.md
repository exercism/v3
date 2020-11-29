## list-methods

Python allows you to manipulate a [`list`][list] in a lot of ways. Remember: when you manipulate a list with a list-method, you are _mutating the original list_. That is, **you will alter the list** object that the list-method is being called on. If you do not want to change your original list, you will need to copy it via `.copy()` or some other technique.

To begin, you will need a `list` object to apply the methods to.

```python
>>> empty_list = list() # (or) empty_list = []
>>> empty_list
[]
```

### Adding an Item to the List

If you want to add an item to an existing list, you can use the list-method `.append()` . As the name states, it _appends_ the item to the **end** (right-hand side) of the list.

```python
>>> numbers = [1, 2, 3]
>>> numbers.append(9)
>>> numbers
[1, 2, 3, 9]
```

Another way to add an item to the list is to `.insert()` an item. This method gives you the ability to add the item at particular index in the list.

`.insert()` takes 2 parameters:

1. index of the item _before which_ you want the new item to appear
2. the item to be inserted

Note: if the given index is 0, the item will be added to front of the list. If the supplied index is greater than or equal to the length of the list, the item will be added in the last position, and is the equivalent of using `.append()`.

```python
>>> numbers = [1, 2, 3]
>>> numbers.insert(0, -2)
>>> numbers
[-2, 1, 2, 3]
>>> numbers.insert(1, 0)
>>> numbers
[-2, 0, 1, 2, 3]
```

If you have an iterable that you would like to _combine_ with your current list (concatenating the two), you can use the `.extend()` method. `.extend()` will unpack the supplied iterable and add its elements in order to your list (_using `.append()` in this circumstance would add the entire iterable as a **single element**._).

```python
>>> numbers = [1, 2, 3]
>>> other_numbers = [5, 6, 7]

>>> numbers.extend(other_numbers)
>>> numbers
[1, 2, 3, 5, 6, 7]

>>> numbers.extend([8, 9])
>>> numbers
[1, 2, 3, 5, 6, 7, 8, 9]
```

### Removing Items

If you want to delete an element, you can use `.remove()` and pass the item you want removed from the list. `.remove()` will throw a `ValueError` if the item to be removed is not in the list.

```python
>>> numbers = [1, 2, 3]
>>> numbers.remove(2)
>>> numbers
[1, 3]
>>> numbers.remove(0)
ValueError: list.remove(x): x not in list
```

Alternatively, using the `.pop()` function will both remove **and** `return` an element for use. `.pop()` takes 1 parameter -- the index of the item you need to remove and receive. If you specify an index number higher than the length of the list, you will get an `IndexError`. If an index is not specified, the last element of the list will be removed and returned to you.

```python
>>> numbers = [1, 2, 3]
>>> numbers.pop(0)
1
>>> numbers
[2, 3]
>>> numbers.pop()
3
>>> numbers
[2]
```

If you'd like to remove all the items from the list you can use the `.clear()` method. It does not have any parameters.

```python
>>> numbers = [1, 2, 3]
>>> numbers.clear()
>>> numbers
[]
```

### Reversing and reordering

Items in the list can be reordered in reverse with `.reverse()`.

```python
>>> numbers = [1, 2, 3]
>>> numbers.reverse()
>>> numbers
[3, 2, 1]
```

You can re-order your list _**in place**_ with the help of the `.sort()` method. Internally, python uses [`Timsort`][timsort] to arrange the list. If the elements are alphanumerical, you don't have to provide any arguments to `.sort()`. Optionally, you can define a custom key for sorting criteria. The Python docs offer some [additional tips and techniques for sorting][sorting how to] lists effectively.

```python
>>> names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> names.sort()
>>> names
["Bruce", "Natasha", "Thor", "Tony"]
```

If you want the sort to be in descending order, you can use the reverse parameter.

```python
>>> names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> names.sort(reverse=True)
>>> names
["Tony", "Thor", "Natasha", "Bruce"]
```

For cases where changing your original list is undesirable, the built-in [`sorted()`][sorted] can be used to return a new, sorted copy of your original list.

### Occurrences of an item in the list

You can find the number of occurrences of an element in the list with the help of `.count()`. It takes the element you need to tally as its argument, and returns the total number of times it appears on the list.

```python
>>> items = [1, 4, 7, 8, 2, 9, 2, 1, 1, 0, 4, 3]
>>> items.count(1)
3
```

### Finding the index of items

`.index()` will provide you the index number of the first occurrence of the item you pass in. If you do not have any occurrences of the item, a `ValueError` is raised. If you do not need the exact position of an item and are only checking that it is present on the list, the built-in `in` operator is more efficient.

Index starts with 0.

```python
>>> items = [7, 4, 1, 0, 2, 5]
>>> items.index(4)
1
>>> items.index(10)
ValueError: 10 is not in list
```

You can provide start and end indices to search within a specific section of the list.

```python
>>> names = ["Tina", "Leo", "Thomas", "Tina", "Emily", "Justin"]
>>> names.index("Tina")
0
>>> names.index("Tina", 2, 5)
3
```

### Making Copies

Remember that _names_ in python are just labels that reference an underlying object. This creates a few surprises when working with lists.

```python
>>> actual_names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> same_list = actual_names
>>> same_list.append("Clarke")
>>> same_list
["Tony", "Natasha", "Thor", "Bruce", "Clarke"]
>>> actual_names
["Tony", "Natasha", "Thor", "Bruce", "Clarke"]
```

This referencing issue becomes exacerbated with operating on nested/multiplied lists.

```python
from pprint import pprint

# This will produce a game grid that is 8x8, pre-populated with zeros
>>> game_grid = [[0]*8] *8

>>> pprint(game_grid)
[[0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0]]

# Lets put an x in the bottom right corner
>>> game_grid[7][7] = "X"

# That doesn't work, because all of the rows are referencing the same underlying list
>>> pprint(game_grid)
[[0, 0, 0, 0, 0, 0, 0, 'X'],
 [0, 0, 0, 0, 0, 0, 0, 'X'],
 [0, 0, 0, 0, 0, 0, 0, 'X'],
 [0, 0, 0, 0, 0, 0, 0, 'X'],
 [0, 0, 0, 0, 0, 0, 0, 'X'],
 [0, 0, 0, 0, 0, 0, 0, 'X'],
 [0, 0, 0, 0, 0, 0, 0, 'X'],
 [0, 0, 0, 0, 0, 0, 0, 'X']]

```

To create a second copy of a list, you need to _slice_ or explicitly use the `.copy()` method, which will make a second set of references that can then be changed without the danger of unintentional mutation of elements. However, if your list contains _variables_ or nested data structures, those second-level references will **not be copied** (_To copy an entire tree of containers, references, and objects, you need to use `.deep_copy()` or a `list comprehension`-- more on that later._). For a detailed explanation of list behavior, see this excellent [making a game board][making a game board] article.

```python
>>> names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> new_list = names.copy()
>>> new_list.append("Clarke")
>>> new_list
["Tony", "Natasha", "Thor", "Bruce", "Clarke"]
>>> names
["Tony", "Natasha", "Thor", "Bruce"]


from pprint import pprint

# This loop will safely produce a game grid that is 8x8, pre-populated with zeros
>>> game_grid = []
>>> filled_row = [0] * 8
>>> for row in range(8):
...    game_grid.append(filled_row.copy())

>>> pprint(game_grid)
[[0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0]]

# Lets put an x in the bottom right corner
>>> game_grid[7][7] = "X"

# Now the game grid works the way we expect it to
>>> pprint(game_grid)
[[0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 0],
 [0, 0, 0, 0, 0, 0, 0, 'X']]
```

[list]: https://docs.python.org/3/tutorial/datastructures.html
[timsort]: https://en.wikipedia.org/wiki/Timsort
[sorted]: https://docs.python.org/3/library/functions.html#sorted
[sorting how to]: https://docs.python.org/3/howto/sorting.html
[making a game board]: https://nedbatchelder.com/blog/201308/names_and_values_making_a_game_board.html
