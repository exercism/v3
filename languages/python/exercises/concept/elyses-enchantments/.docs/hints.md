## General

## 1. Creating a List

- `Lists` in python may be [constructed](https://docs.python.org/3/library/stdtypes.html#list) in several ways. 
- If you want to create a `list` with a small amount of elements, the easiest way to do it is to use the square brackets and a comma to separate two elements. For example, to create a `list` containing the elements a, b and c, you can write:

```python
>>> ['a', 'b', 'c']
```

- Another way to create a `list` is to use the type constructor `list()`. To create the same list as in the previous example, you can write:

```python
>>> list(('a', 'b', 'c'))

```

## 2. Creating a copy of a List

- `Lists` can be [nested](https://realpython.com/python-lists-tuples/#lists-can-be-nested), this means that a `list` can be an element of another `list`.

## 3. Concatenating Lists

- Sequence types such as `list` already support a few [common operations](https://docs.python.org/3/library/stdtypes.html#sequence-types-list-tuple-range)

## 4. Testing List Membership

- Sequence types such as `list` already support a few [common operations](https://docs.python.org/3/library/stdtypes.html#sequence-types-list-tuple-range)

## 5. Accessing List Elements by Index

- Sequence types such as `list` already support a few [common operations](https://docs.python.org/3/library/stdtypes.html#sequence-types-list-tuple-range)
- Remember that the first element of the `list` is at index 0
- In python, negative indexing starts the count from the end. This mean that you can find the last element of the `list` at index -1.

## 6. Accessing Sublists by Slicing

- Sequence types such as `list` already support a few [common operations](https://docs.python.org/3/library/stdtypes.html#sequence-types-list-tuple-range)
- For the last part of the exercise, think about reusing the code from the functions that you just implemented.

## 7. Iterating Over List Items

- There are a few techniques to [iterate over a `list` in python](https://www.geeksforgeeks.org/iterate-over-a-list-in-python/).
- The easiest one is the for loop, here is an example of how to use it:

```python
>>> list = [1, 2, 3, 4, 5]
>>> for item in list:
...     item*2
>>>list
[2, 4, 6, 8, 10]
```

## 8. Creating Lists with Mixed Data Types

- There are many [built-in types](https://docs.python.org/3/library/stdtypes.html) in python.
- [`Lists` Can Contain Arbitrary Objects](https://realpython.com/python-lists-tuples/#lists-can-contain-arbitrary-objects)

## 9. Modifying Values in Lists

- `Lists` in python are mutable, this means that once a `list` is created, you can modify, delete or add any element as you wish. Python provides a wide range of [ways to modify `lists`](https://realpython.com/python-lists-tuples/#lists-are-mutable).
- Here is an example of the easiest way to modify the value of a `list` element: 

```python
>>> list = [1, 2, 3, 4, 5]
>>> list[2] = 333
>>> list
[1, 2, 333, 4, 5]
```


