# Concepts of Two Fer

[Example implementation](https://github.com/exercism/csharp/blob/master/exercises/two-fer/Example.cs)

## General
- function: used as the entry point to this exercise
- static function: the `static` keyword is the modifier that makes the method static, and enables it to be called without instantiation. The static method can access the variables passed in as arguments, global, and only other static members of the class.  
- optional argument: adding an optional argument, avoids method overloading, and keep the code simpler    
- class: the tested method is defined in a class
- visibility: making tested method and tested class `public`
- imports: import types through `using` statements

## Optional, optimizing the code
- string interpolation: while string could be formatted in different ways, [string interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated) provides a more effective and simpler way in this exercise
- expression body method: it makes the code cleaner for a short (one-line) method - [link]([https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/expression-bodied-members#methods]) 
