### 1. Create a function that returns 42

You can use the `defn` macro to define a function!

The defn macro accepts the name of the function you are defining, and optional docstring, a vector of input arguments, and the expressions to be evaluated when the function is called!


### 2. Create a function that accepts an input and adds 42 to it 

You can name the argument passed into the function by writing its name within the argument vector when the function is defined. The argument can then be referenced by that name anywhere within the function.

Remember to use prefix notation with the '+' function. For example, in Clojure "one plus two" would be written as:

```clojure
(+ 1 2)
```

### 3. Create a function that takes and argument and calls function defined in (2) with it

Remember, you can invoke a function you defined the same way you invoke other functions- by putting its name as the first argument of a Clojure form and letting the Clojure compile evalute it!