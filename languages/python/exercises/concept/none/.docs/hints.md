## General

- Don't worry about how the arguments are derived, just focus on combining the arguments to return the intended result.

## 1. Arrange the Seats

- You need to create a dict object first. Then you can loop from 1 to the number of guests and assign them to None

## 2. Is the Seat Empty?

- You can check a variable is None by the if statements. `if var == None` or `if var is None` will tell you if the variable is None.

## 3. Find the Empty seat

- You can use the is_seat_empty() function through a loop of all the key, values in the dict object. When the value is none, you need to return the key.

## 4. Current Empty Seating Capacity

- You need to find the number of empty seats ( that is pointing to `None`) in the dict object. Once you get the count, that is the answer

## 5. Should we wait?

- You need to find the current number of empty seats and find if it is greater than or equal to the number of guests waiting.

## 6. Empty a seat

- Given the seat number ( the key to the `dict` object ) and the seats dict object, you need to assign that particular seat (key) to value `None`.
