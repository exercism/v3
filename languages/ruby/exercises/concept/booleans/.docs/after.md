## True, False, and Nil

- `true` and `false` are used to represent boolean logical states.
  - They are singleton instances of the [`TrueClass`][true-class] and [`FalseClass`][false-class] objects.
  - they may occur as literals in code, or as the result of logical (`&&`, `||`, `!`) or [comparison][comparable-class] (`<`, `>`, `==`) methods.
- `nil` is a value to express _absence_ of an object.
  - It is a singleton instance of the [`NilClass`][nil-class]
  - It is often used for a default value
    - The default value for class instance variables is `nil` unless otherwise assigned

## Truthy, Falsey Logic

- Only `false` and `nil` evaluate to _falsey_.
- Everything else, objects and values, evaluate to _truthy_.

  ```ruby
  # A simplified definition
  def falsey
    nil || false
  end

  def truthy
    not falsey
  end
  ```

- It is common to use _truthy_ and _falsey_ values to determine the outcome of [control expressions][control-expressions].
  - `if`...`else` exists as a construct, similar to the [_C-family_ of programming languages][c-family]
  - it is often more idiomatic to use `if` and `unless` as [expression modifiers to "guard" an expression][if-modifier]

```ruby
1 + 1 if truthy
# => this will evaluate and return 2

2 + 2 if falsey
# => the numbers are not added because of the modifier, nil is returned

3 + 3 unless truthy
# => the numbers are not added because of the modifier, nil is returned

4 + 4 unless falsey
# => this will evaluate and return 8
```

[c-family]: https://en.wikipedia.org/wiki/List_of_C-family_programming_languages
[control-expressions]: https://docs.ruby-lang.org/en/master/syntax/control_expressions_rdoc.html
[if-modifier]: https://docs.ruby-lang.org/en/master/syntax/control_expressions_rdoc.html#label-Modifier+if+and+unless
[true-class]: https://docs.ruby-lang.org/en/master/TrueClass.html
[false-class]: https://docs.ruby-lang.org/en/master/FalseClass.html
[nil-class]: https://docs.ruby-lang.org/en/master/NilClass.html
[comparable-class]: https://docs.ruby-lang.org/en/master/Comparable.html
[constants]: https://www.rubyguides.com/2017/07/ruby-constants/
[integer-class]: https://docs.ruby-lang.org/en/master/Integer.html
[kernel-class]: https://docs.ruby-lang.org/en/master/Kernel.html
[methods]: https://launchschool.com/books/ruby/read/methods
[returns]: https://www.freecodecamp.org/news/idiomatic-ruby-writing-beautiful-code-6845c830c664/
