In Clojure, functions are your basic tools for manipulating data.

Functions can be defined with the `defn` macro.

A Clojure function consists of 4 required parts:

- The `defn` macro (or `def fn`).
- The name of the functions being defined.
- A vector of arguments that the function accepts.
- The expressions which make up the function body.

Functions can also contain a [docstring](https://github.com/bbatsov/clojure-style-guide#documentation) which should be written on its own line and is placed between the function name and argument vector. Docstrings are the primary way to document Clojure code.

Once a function is defined, it can be invoked anywhere in the file in which is defined. (It can be imported in other files, but that's outside the scope of this lesson). 

Invoke a function by putting it as the first argument of a Clojure form.

In general, Clojure functions should be written with lowercase letters with words separated by dashes.

Clojure functions are extremely powerful and support many interesting features which are covered in later lessons.
