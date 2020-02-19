# Concepts of gigasecond

[Example implementation](https://github.com/exercism/csharp/blob/master/exercises/gigasecond/Example.cs)

## Object-oriented
- Classes: used on the template. 
- Encapsulation: used on the template and test class
- Methods: used on the template
- Methods arguments: the time is passed as an argument
- Immutability: some people define a `const` variable representing the gigasecond value
- Static classes: used on the template, the class is defined as `static` 
- Static class members: the method `Add` is `static`, and it is always accessed by the class name

## General
- Namespaces: used on the template.
- Exception handling: some people use that to detect errors. Even though it is not required by the tests.
- Nullability: some people need to check for nulls.
- Scoping: use `{` and `}` to denote scoping
- Visibility: making tested method and tested class `public`
- Numbers: AddSeconds method has a `double`, as the input parameter 
- Dates: using DateTime
- Numbers - Digit separator (`_`): Using this separator, it is easier to read the big value of gigasecond

## Functional
- Expression-bodied members: some solutions include method written as expression-bodied member
