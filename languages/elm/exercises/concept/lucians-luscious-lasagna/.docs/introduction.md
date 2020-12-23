## basics

### Constants and functions

A constant value is defined with `name = expression`,
where in Elm, everything except definitions are expressions.

```elm
five = 5

six = 3 + 3
```

Functions are defined with `name parameters... = expression`,
parameters being only separated by space.

```elm
add number1 number2 = number1 + number2
```

Invoking a function also is an expression and is done by
specifying its name and passing arguments for each of the function's parameters,
separated by space, just like for function definition.

```elm
five = add 2 3
```

Parentheses can be used to specify the order of evaluation of an expression.

```elm
six = add 2 (1 * 4)

twelve = add 2 1 * 4
```

### Indentation / significant whitespace

Elm doesn't use syntactic markers such as curly brackets, parentheses, or semicolons to specify code boundaries. It uses whitespaces and indentations instead.

- Module, import, and top-level function definitions must start at the left most column.
- If an expression is split into multiple lines, the code that is part of that expression must be indented under that expression with at least one space.
- Parts of the expression that are grouped together should be indented with equal number of spaces.

```elm
-- A function split over multiple lines, so subsequent lines must be indented
add number1 number2 =
    number1 + number2
```

https://elmprogramming.com/indentation.html

### Modules

Each file in Elm is a module, and must contain a `module` statement before all other code.
Module names must match their file name, so module `Calculator` must be in file Calculator.elm.
Anything defined within a module is privatly scoped to it
and cannot be accessed from outside this module, unless listed in `exposing`.

```elm
-- Define the Calculator module, and expose the `add` function
module Calculator exposing (add)

six = 3 + 3

add number1 number2 = number1 + number2
```

```elm
-- Define the Calculator module, and expose everything within: `six` and `add`
module Calculator exposing (..)

six = 3 + 3

add number1 number2 = number1 + number2
```

https://elm-lang.org/docs/syntax#modules

### Comments

A comment is some text within the Elm file that is not interpreted as code.
It is mainly intented to be read by yourself and other programmers.
There is a lightweight syntax for single line comments, based on double dashes.
Multiline comments are also possible with the `{-` and `-}` pair
of opening and closing delimiters.

```elm
-- a single line comment
-- another single line comment

{- a multiline comment
   spawning multiple lines
-}
```

### Formatting

There is a [style guide](https://elm-lang.org/docs/style-guide),
and [elm-format](https://github.com/avh4/elm-format) can be used to automatically format code.
