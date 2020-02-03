# Concepts of meetup

[Example implementation](https://github.com/exercism/fsharp/blob/master/exercises/meetup/Example.fs)

## General

- functions: used as the main entry point for the exercise
- function arguments: input strand is passed as an arguments
- methods: calling the `DateTime.DaysInMonth` method
- properties: calling the `Day` property of a `DateTime`
- return values: returning a value from a function
- implicit returns: the last expression is automatically returned from a function
- type inference: automatically infer the type of the functions and values
- modules: the functions are defined in a module
- imports: import types through `open` statements
- namespaces: knowing where to find the `DateTime` class
- pattern matching: matching on the `Week` discriminated union
- assignment: assigning values
- anonymous functions: a lambda is used to filter and map
- partial application: partially applying arguments to functions to return a new function
- pipeline: using the `|>` operator to construct a pipeline
- collection filtering: using `List.filter`, `List.find`, `List.last` and `List.item` to filter the list of dates
- collection mapping: using `List.map` to map integers to `DateTime` instances
- collection iteration: using `List` module functions to iterate over collections
- ranges: using a range to iterate over the days
- discriminated unions: the `Week` type is defined as a discriminated union
- equality operators: `=` used to compare day of week
- comparison operators: `>=` used to compare
- dates: a `DateTime` value is returned from the function; a `WeekDay` value is passed as a parameter
- integers: an `int` is used as a year and month parameter
- tuples: passing arguments to the `DateTime` constructor and `DateTime.DaysInMonth` method
- integers: an `int` is used for item access and day comparison

## Approach: inner function

- nested functions
