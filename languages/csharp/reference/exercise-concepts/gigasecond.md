# Concepts of gigasecond

[Example implementation](https://github.com/exercism/csharp/blob/master/exercises/gigasecond/Example.cs)

## General

- functions: used as the main entry point for the exercise
- methods: used on the template
- methods arguments: the time is passed as an argument
- static function: the static keyword is the modifier that makes the method static, and enables it to be called without instantiation. The static method can access the variables 		passed in as arguments, global, and only other static members of the class.
- static classes: used on the template, the class is defined as `static` 
- return values: returning a value from a method
- scoping: use `{` and `}` to denote scoping
- type inference: using `var` to define the seconds
- classes: the tested method is defined in a class
- visibility: making tested method and tested class `public`
- imports: import types through `using` statements
- immutability: some people define a `const` variable representing the gigasecond value
- namespaces: knowing where to find the `DateTime` class
- expression body method: it makes the code cleaner for a short (one-line) method
- exponential notation (scientific): use to represent numbers e.g 1e9 to represent 1000000000
- static method: to access metods at the type level.
- dates: using DateTime
- numbers: `AddSeconds` method has a `double`, as the input parameter.
 