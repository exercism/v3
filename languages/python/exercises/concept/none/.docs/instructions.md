In this exercise, you will get the basic idea of how to use 'None'.

The exercise has 6 challenges to complete.

You are the Maître D' of a hotel, Your tasks are given based on the number of people who are waiting to eat at your hotel. Complete the tasks accordingly and proceed.

## 1. Arrange the Seats

Define the `arrange_the_seats()` function with 1 parameter. You know the number of guests who have reserved a table today.

The task of this function is to return a `dict` object containing the seat number (in `int`) as the key and `None` as the value for the number of guests who are going to attend.

```python
arrange_the_seats(guests=4)
#=> {1: None, 2: None, 3: None, 4: None}
```

## 2. Is the Seat Empty?

Define the `is_seat_empty()` function that takes 1 parameter. First parameter will contain the value of the seat which is checked before assigning to a guest.

If the seat is empty, It will be of `None` value, else it will be mentioned with string `Occupied`.

If the seat is empty, return `True` else return `False`.

```python
is_seat_empty(seat_value="Occupied")
# => False
is_seat_empty(seat_value=None)
# => True
```

## 3. Find the Empty seat

Define the `find_the_seat()` function that takes 1 parameter. First parameter will contain the `dict` object that is created by you while arranging the seats.

Find the first empty seat that you come across in the list of seats passed to you.

Your job is to return the seat number ( the key of the `dict` object ) that is passed to you.

Note: There can be more than one seat which can be empty in the seats sent to you as a parameter.

Bonus Tip: You can use the Function `is_seat_empty()` as an helper for this function.

```python
find_the_seat(seats={1: "Occupied", 2: None, 3: "Occupied"})
# => 2
```

## 4. Current Empty Seating Capacity

Define the `curr_empty_seat_capacity()` function that takes 1 parameter. The first parameter will list all the seats.

You need to find out what is the total number of seats that are empty.

```python
curr_empty_seat_capacity(seats={1: "Occupied", 2: None, 3: "Occupied"})
# => 1
can_accomodate_seats(seats={1: "Occupied", 2: None, 3: None})
# => 2
```

## 5. Should we wait?

Define the `can_accomodate_seats()` function that takes two parameters. The first parameter will list all the seats. The second parameter is the seats you need to fill the guests who have come unexpected.

You need to find out how many seats are empty, Based on the number of seats you need to find out whether you can give the unannounced guests the seats they want.

You need to return True, if you have the sufficient number of seats, else False.

Tip: You can use the curr_empty_seat_capacity() function as your helper.

```python
can_accomodate_seats(seats={1: "Occupied", 2: None, 3: "Occupied"}, guests=2)
# => False
can_accomodate_seats(seats={1: "Occupied", 2: None, 3: None}, guests=2)
# => True
```

## 6. Empty a seat

Define the `empty_the_seat()` function that takes two parameters. The first parameter will list all the seats. The second parameter is the seat you need to empty, that is you need to assign the seat with None.

Return the `dict` of seats sent as the parameter after updating the empty seat.

```python
empty_the_seat(seats={1: "Occupied", 2: None, 3: "Occupied"}, 3)
# => {1: "Occupied", 2: None, 3: None}
```
