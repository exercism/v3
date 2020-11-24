## 2. Write a boolean expression that describes whether a given year `y` is a leap year

- Consider writing a [truth table](https://en.wikipedia.org/wiki/Truth_table) for this exercise and see if you can work out what the simplest predicates are and how they could be combined with `&&` and `||`. A predicate is a statement or a function that is either true or false, depending on its inputs. `isodd(number)` is an example of a predicate function in Julia.

## 3. Wrap it in the `isleapyear` function

- You can return a boolean expression directly:

  ```julia
  julia> function equalstwo(x)
          return x == 2
      end
  equalstwo (generic function with 1 method)

  julia> equalstwo(3)
  false

  julia> equalstwo(2)
  true
  ```
