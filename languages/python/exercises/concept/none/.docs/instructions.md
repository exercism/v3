In this exercise, you will get the basic idea of how to use 'None'.

The exercise has 4 challenges to complete. Each dependent on the previous one.

You are present in an auditorium filled with people and there are some empty seats.

## 1. Find the Empty seat

Define the `find_the_seat()` function that takes 1 parameter. The parameter will consists of the list of seats available in the auditorium. Now find the first seat which is empty.

The indexing of the seats start from 1.

Note: there maybe many slots that can be empty, Your testcase should return the first empty slot.

You have to return the index of the first seat which is empty.

```python
find_the_seat([1, None, 2])
# => 2
```

## 2. Empty a seat

Define the `empty_the_seat()` function that takes two parameters. The first parameter will list all the seats available and occurpied in the auditorium. The second parameter is the seat you need to empty, that is you need to assign the seat with None.

Return the list of seats sent as the parameter after updating the empty seat.

```python
empty_the_seat([1, 2, 3], 3)
# => [1, 2, None]
```
