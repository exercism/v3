# Concepts of gigasecond

[Example implementation](https://github.com/exercism/csharp/blob/master/exercises/gigasecond/Example.cs)

## General

- classes: 
    - static classes: a static class cannot be instantiated. the exercise defines from the beginning the class `Gigasecond`
    - the tested method is defined in a class
- methods: 
    - used as the main entry point for the exercise
    - methods arguments: the time is passed as an argument
    - return values: returning a value from a method
    - static class members: the method is defined as static, and it is always accessed by the class name, not the instance name.

- scoping
    - use `{` and `}` to denote scoping
    - visibility: making tested method and tested class `public`
    - imports: import types through `using` statements

- numbers: 
    - floating point numbers: AddSeconds method has a double, as an input parameter 



### Functional
- expression-bodied members: using DateTime.AddSeconds is possible to write the method as a single line. Writing the method as an expression-bodied member, would makes the code cleaner 

### Types
- numbers: 
    - digit separator (`_`): Using this separator, it is easier to read the big value of gigasecond

### Object-oriented
- immutability: defining the gigasecond value with `const` keyword; gigasecond value is an immutable value which is known at compile time and do not change for the life of the program
