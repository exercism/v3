Python allows you to manipulate a [`list`][list] in a lot of ways. A `list` is simple a collection of objects of [type `List`][list std type]. They are mutable, ordered and indexed. Let's look at the methods that are available to manipulate a `list` object.

When you manipulate a list with a list-method, you are changing the properties of the list you pass. That is, **you will alter the list** object that is being used with the list-method. If you do not want to change the original list, you need to copy the list and then work on the copied list.

To begin, you first need a `list` object to use the functions.

```python
>>> empty_list = list() # (or) empty_list = []
>>> empty_list
[]
```

## Add an Item to the List

Topics to discuss:

1. `append`
2. `insert`
3. `extend`

If you want to add an item to an existing list, you use the list-method `append()` for it. As the name represent, it appends the item to the **end** of the list.

```python
>>> numbers = [1, 2, 3]
>>> numbers.append(9)
>>> numbers
[1, 2, 3, 9]
```

There is another way to add an item to the list. `insert()` method also gives you the ability to add the item to a particular index in the list.

`insert` takes 2 parameters.

1. index at which you want to add the item
2. item

Note: if the index is 0, the item will be added to the first of the list. if the index is more than or equal to the length of the list, then item will be added to the last of the list (like the append function)

```python
>>> numbers = [1, 2, 3]
>>> numbers.insert(0, -2)
>>> numbers
[-2, 1, 2, 3]
>>> numbers.insert(1, 0)
>>> numbers
[-2, 0, 1, 2, 3]
```

If you have another list object who's content you need to be copied to your list, then you can use the `extend` function.
The extend function takes an iterable as its parameter.

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

# Removing Items from the list

Topics to discuss:

1. `remove`
2. `pop`
3. `clear`

If you want to remove an item from a list, you can use the `remove` function which takes one parameter. Feed the function with the item you want to remove from the list. the `remove` function will throw a `ValueError` if the item does not exist in the loop.

```python
>>> numbers = [1, 2, 3]
>>> numbers.remove(2)
>>> numbers
[1, 3]
>>> numbers.remove(0)
ValueError: list.remove(x): x not in list
```

There is another way to remove an item from the list. If you don't specify any parameter, the function will remove the last element and returns it to you. Pop takes one optional parameter, the index of the item you want to remove and returns it to you. If you specify an index greater than the length of the list, you will get an `IndexError`.

```python
>>> numbers = [1, 2, 3]
>>> numbers.pop()
3
>>> numbers
[1, 2]
>>> numbers.pop(0)
1
>>> numbers
[2]
```

If you want to clear the list (ie: all the items in the list), then you can use the `clear` function. It does not take any parameter as input but will remove all the elements in the list.

```python
>>> numbers = [1, 2, 3]
>>> numbers.clear()
>>> numbers
[]
```

## Reverse the list

The items in the list can be reordered in the reverse order with the `reverse` function.

```python
>>> numbers = [1, 2, 3]
>>> numbers.reverse()
>>> numbers
[3, 2, 1]
```

## Sort the list

When you have an unordered list of items, you can sort them with the help of `sort` function. If you have items that are alphanumerical, you can use `sort` without any arguments.

If you have a list of names and you want to sort them:

```python
>>> names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> names.sort()
>>> names
["Bruce", "Natasha", "Thor", "Tony"]
```

If you want the sort to be in descending order, you can use the reverse parameter:

```python
>>> names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> names.sort(reverse=True)
>>> names
["Tony", "Thor", "Natasha", "Bruce"]
```

If you have a list of items that are complex, you can use the key parameter in the sort. Lets see more about this later.

Internally, python uses [`Timsort`][timsort] to sort the list.

## Occurrences of an item in the list

You can find the occurrences of an element in the list with the help of the `count` function. It takes one parameter which will be the item, for which you need to find the number of occurrences.

```python
>>> items = [1, 4, 7, 8, 2, 9, 2, 1, 1, 0, 4, 3]
>>> items.count(1)
3
```

## Find the index of the item

If you want to find the index of an item you want in the list, you can use the `index` function. If you have multiple occurrences of the same item, it will provide you the index of the first occurrence. If you do not have any occurrences of the item, then you will have a `ValueError` raised. The index is zero-based, meaning the first item is at the position 0.

```python
>>> items = [7, 4, 1, 0, 2, 5]
>>> items.index(4)
1
>>> items.index(10)
ValueError: 10 is not in list
```

You can also provide a start (and an end) to the index from where you need the search has to happen for the item.

```python
>>> names = ["Tina", "Leo", "Thomas", "Tina", "Emily", "Justin"]
>>> names.index("Tina")
0
>>> names.index("Tina", 2, 5)
3
```

## Copy a List

Lists are collections of items which are referenced by an index. Therefor, if you do re-assign a list object to another variable, any change you do to the new variable will also impact the original variable.

```python
>>> actual_names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> same_list = actual_names
>>> same_list.append("Clarke")
>>> same_list
["Tony", "Natasha", "Thor", "Bruce", "Clarke"]
>>> actual_names
["Tony", "Natasha", "Thor", "Bruce", "Clarke"]
```

You need to use the `copy` function in order to do a `shallow_copy` of the list. (More about `shallow_copy` and `deep_copy` later.)

```python
>>> names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> new_list = names.copy()
>>> new_list.append("Clarke")
>>> new_list
["Tony", "Natasha", "Thor", "Bruce", "Clarke"]
>>> names
["Tony", "Natasha", "Thor", "Bruce"]
```

To create a second copy of a list, you need to _slice_ or explicitly use the `.copy()` method, which will make a second set of references that can then be changed without the danger of unintentional mutation of elements.

```python
>>> names = ["Tony", "Natasha", "Thor", "Bruce"]
>>> new_list = names.copy()
>>> new_list.append("Clarke")
>>> new_list
["Tony", "Natasha", "Thor", "Bruce", "Clarke"]
>>> names
["Tony", "Natasha", "Thor", "Bruce"]
```

This reference limitation becomes exacerbated with operating on nested/multiplied lists.

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

# Let's put an x in the bottom right corner
>>> game_grid[7][7] = "X"

# That doesn't work, because all the rows are referencing the same underlying list
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

However, if your list contains _variables_ or nested data structures, those second-level references will **not be copied** (_To copy an entire tree of containers, references, and objects, you need to use `.deep_copy()` or a `list comprehension`-- more on that later._). For a detailed explanation of list behavior, see this excellent [making a game board][making a game board] article.

```python
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

# Let's put an x in the bottom right corner
>>> game_grid[7][7] = "X"

# Now, the game grid works the way we expect it to
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

[list]: https://docs.python.org/3/tutorial/datastructures.html#more-on-lists
[list std type]: https://docs.python.org/3.9/library/stdtypes.html#list
[timsort]: https://en.wikipedia.org/wiki/Timsort
[making a game board]: https://nedbatchelder.com/blog/201308/names_and_values_making_a_game_board.html
