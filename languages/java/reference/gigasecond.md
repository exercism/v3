# Gigasecond

## Exercise
Given a moment, determine the moment that would be after a gigasecond has passed.

## Concepts encountered while solving it

### Dates and Times
- Natively, java does not have a built-in Date time api, therefore we need to import the `java.time` library. Below are a few of the classes this library gives us access to:
  - `LocalDate`: Represnts a date
  - `LocalTime`: Represnts a time
  - `LocalDateTime`: Represnts a time on a specific date
  - `DateTimeFormatter`: Allows formating and parsing of different date/time strings to objects (useful to determing the pattern in dates are consumed i.e YYYY/MM/DD or MM/DD/YY or DD.MM.YYYY etc.)

### Integers
- `Integer` is a wrapper class around the primitive type `int` which contains a single field of type `int`. The difference between an `int` and an `Integer` is that `int` can only be a binary value of an integer whereas an `Integer`, being a class, has access to a number of other inbuilt functions making it more flexible to manipulation.
