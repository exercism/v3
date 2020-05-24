In Clojure, assigning a value to a name is referred to as a _binding_. Top-level (global) bindings are called `var`s and should be treated like constants in other languages, but are commonly redefined at a REPL to facilitate dynamic development.

Top-level bindings are defined using `def`:

```clojure
(def fingers 10)
```

The `defn` macro can be used to define a function. Functions are also regular bindings, but with one or more parameters. A function automatically returns the result of its last expression.

```clojure
(defn add [x y]
  (+ x y))
```

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters.

```clojure
(def five (add 2 3))
```

Functions and values in Clojure can only be used _after_ they have been defined. Using it before it has been defined results in a compile error.

```clojure
;; Compile error as `add` has not yet been defined
(def seven (add 3 4))
```

In Clojure, whitespace has no significance other than formatting.

Clojure functions and vars are organized in namespaces. A namespace groups related functionality and is defined using the `ns` macro:

```clojure
(ns calculator)

(def pi 3.14)

(defn add [x y]
  (+ x y))
```

Clojure supports two types of comments. Single line comments are preceded by `;` and the `comment` form is used to prevent evaluation of everything between `(comment ` and `)`.
