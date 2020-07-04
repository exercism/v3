In Python, a [tuple](https://docs.python.org/3/library/stdtypes.html#tuple) is an immutable collection of items in _sequence_. Like most collections, tuples can hold any (or multiple) data type(s) -- including other tuples. Like any sequence, items are referenced by 0-based index number. Tuples support all [common sequence operations](https://docs.python.org/3/library/stdtypes.html#common-sequence-operations).

## Tuple Construction

Tuples can be formed in multiple ways, using either the `tuple` class constructor or the `tuple` literal declaration.

#### Using the `tuple()` constructor:

```python
no_elements = tuple()
no_elements
>>> ()

#elements are iterated through and added to the tuple in order
multiple_elements_string = tuple("Timbuktu")
multiple_elements_string
>>> ('T', 'i', 'm', 'b', 'u', 'k', 't', 'u')

multiple_elements_list = tuple(["Parrot", "Bird", 334782])
multiple_elements_list
>>> ("Parrot", "Bird", 334782)

```

#### Declaring a tuple _literal_ :

Note: generally parentheses are not _required_ to create a `tuple` literal - only commas. Parentheses are only required in cases of ambiguity.

```python
no_elements = ()
no_elements
>>> ()


elements_separated_with_commas = "Parrot", "Bird", 334782
elements_separated_with_commas
>>> ("Parrot", "Bird", 334782)

elements_with_commas_and_parentheses = ("Triangle", (60, 60, 60))
elements_with_commas_and_parentheses
>>> ("Triangle", (60, 60, 60))

```

## Concatenation

Tuples can be _concatenated_. Using the plus **`+`** operator unpacks each tuple and creates a new tuple from the combined elements.

```python
new_via_concatenate = ("George", 5) + ("cat", "Tabby")
new_via_concatenate
>>> ("George", 5, "cat", "Tabby")

new_nested_concatenate = ("Triangle", (60, 60, 60)) + ('orange', 'dashed-outline')
new_nested_concatenate
>>> ('Triangle', (60, 60, 60), 'orange', 'dashed-outline')

```

## Accessing data

Items inside tuples (_like the items inside sequence types `string` and `list`_), can be accessed via 0-based index and _bracket notation_.

```python

student_info = ("Alyssa", "grade 3", "female", 8 )

#name is at index 0
student_name = student_info[0]
student_name
>>> "Alyssa"

#grade is at index 1
student_grade = student_info[1]
student_grade
>>> 'grade 3'

#age is at index 3
student_age = student_info[3]
student_age
>>> 8

```

## Iterating through a tuple

Items inside tuples can be _iterated over_ in a loop using **`for item in`** syntax.

```python
for item in student_info:
print(item)

> > > Alyssa
> > > grade 3
> > > female
> > > 8
```

## Checking membership

The **`in`** operator can be used to check membership in a tuple.

```python

fruits = ('Apple', 'Pear', 'Guava', 'Lychee', 'Agbalumo')

'Tomato' in fruits
>>> False

'Agbalumo' in fruits
>>> True

```
