A ternary conditional in Ruby is a shorter way of writing simple `if/else` statements. If an `if/else` statement contains only two branches, one for when the condition is true and one for when it is false, it can be re-written as a ternary conditional.

It uses a combination of the `?` and `:` symbols, often called the ternary operator/s.

For example:

```ruby
if stoplight == 'green'
  cross_the_road
else
  wait
end
```

can be re-written as:

```ruby
stoplight == 'green' ? cross_the_road : wait
```

The code on the left side of the `?` is the condition and the code on the right contains the two possible branches, separated by the `:`. If the condition is *true*, the code on the *left* side of the `:` is executed; if the condition is *false*, then the code on the *right* of the `:` gets executed.
