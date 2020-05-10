PureScript is a statically-typed language, meaning that the type of a variable is checked when the program is compiled. Using the `=` operator, we can bind a value to a variable name:

```purescript
variable = 10 -- Bind the integer value 10
```

The value of a variable can not change once defined. Instead, a new variable can be created:

```purescript
-- Binding an integer value of 1 to variableOne
variableOne = 1

-- Bind an integer value of 2 to variableTwo
variableTwo = 2

-- The compiler will output an error when trying to re-bind the value of a variable:
variableTwo = 3
-- => Multiple value declarations exist for variableTwo.
```

The type of `variableOne` can be said to be an integer (known as an `Int` in PureScript). You can declare the type of a variable before declaring the variable, for example:

```purescript
variableOne :: Int -- variableOne is now of type Int. This syntax is known as the type signature declaration.
variableOne = 1 -- Binds the value of 1 to the Int variableOne
```

However, type can often be inferred by the compiler. **Note**: The `::` can be read "has type".

PureScript is an [functional-programming language][functional-programming] and requires all named functions to be defined in a [module][modules]. The `module` keyword is used to define a module. All modules are available to all other modules at runtime. A [module][modules] is analogous to a _class_ in other programming languages.

```purescript
module Calculator where
-- ...
```

Functions must be defined in a module.

A function is similar to methods/functions in other languages. Every function has one or more arguments. All arguments are statically-typed, and the return type can be explicitly declared using a type signature:

```purescript
foo :: Int -> Int -- foo takes an Int and returns an Int
foo x = x+1 -- foo takes an argument, x, and adds 1 to x
```

or often inferred by the compiler.

Sometimes you might need to import other modules to use their functions. This is done using the `import` keyword followed by the module you'd like to import. A common module to import is `Prelude` which supplies basic functions and operators such as addition (`+`) and subtraction (`-`).

```purescript
module Calculator where

import Prelude

sum :: Int -> Int -> Int
sum x y = x + y
```

**Note**: `Int -> Int -> Int` can be read "takes an Int and an Int, and returns an Int".

Using a function is achieved by specifying the function name and passing arguments for each of the functions parameters.

```purescript
sum = sum 1 2
-- => 3
```

Comments can be used for inline documentation. Single line comments in Elixir are preceded by `--`.

[functional-programming]: https://en.wikipedia.org/wiki/Functional_programming
[modules]: https://github.com/purescript/documentation/blob/master/language/Modules.md
