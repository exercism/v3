## Concept extraction for reverse-string

Example implementation:
```python
def reverse(text: str) -> str:
    """Reverse a given string
    Parameters
    ----------
    text : str
        CAS# for chemical of interest
    
    Returns
    -------
    [str]
        the given string in reverse
    """

    return text[::-1]
```

### Concepts:
- **Function**: `def` to create a function in Python
- **Immutability**: `text` str in Python is [immutable](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str).
In this exercise, you return a new string, the old string `text` is not changed.
- **Return value(s)**: this function return a string by this line: `return text[::-1]`
- **String [slicing](https://docs.python.org/3/reference/expressions.html#slicings)**: 
because `str` in python is a `sequence` object, slicing can be used here. Specifically: for syntax `string[start:stop:stride]`:
  - `start`: 0-index of the start position, `start=0` by default (i.e., not specified) (start from the beginning)
  - `stop`: 0-index of the stop position, `stop=-1` by default (i.e., not specified) (stop at the end)
  - `stride`: number of skip step. For example, 
     ```python
     >>> string = 'ABCDEF'[::2]
     >>> print(string)
     'ACE'
     ```
     In this exercise, `stride = -1` means start from the end
  Together effectively, slicing of `[::-1]` gives the reversed string
  [Extra material for string slicing.](https://www.digitalocean.com/community/tutorials/how-to-index-and-slice-strings-in-python-3)
- **Docstring**: used to document the function, normally situated right below `def func():`
- **[Type hinting](https://docs.python.org/3/library/typing.html#module-typing)**: 
(**Python 3.5+**) not neccessary in Python but can help for easy reading the python file as well as type checking if.
