# Concepts of high-scores

[Example implementation](https://github.com/exercism/csharp/blob/master/exercises/high-scores/Example.cs)

## General concepts
### Object-oriented
- classes: 
    - the tested methods are defined in a class
- methods: 
    - methods arguments: the time is passed as an argument
    - return values: returning a value from a method

### General
- scoping
    - use `{` and `}` to denote scoping
    - visibility: making tested method and tested class `public`
    - visibility: making the list that contains the scores `private` and potentially `readonly`
    - imports: import types through `using` statements (collections, LINQ)

- deep copy
    - creating an object using deep copy to prevent modification of the list of High Scores using the Scores method

### Types
- numbers: 
    - signed integers: used for score values
    - lists: strongly typed list of `int` objects 

### Functional
- LINQ Enumerable functions: provided as a set of static methods for querying - as: Min, Max, Last, OrderByDescending, Take, ToList

## Optional concepts
### Functional
- expression-bodied members: writing the methods as an expression-bodied member, makes the code cleaner 
