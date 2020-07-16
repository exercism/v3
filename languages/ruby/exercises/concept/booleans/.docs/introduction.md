## True, False, and Nil

True and false logical states are represented with `true` and `false` in Ruby. These may either be used as literals on their own, or as a result of logical or comparison methods.

```ruby
true_variable = true
false_variable = false

true && false
# => false

1 < 2
# => true
```

[Nil][nil-dictionary] is an English word meaning "nothing" or "zero". In Ruby, `nil` is a value used to express the _absence_ of an object.

```ruby
# I do not have a favorite color
favorite_color = nil
```

In other programming languages, `null` or `none` values may play a similar role.

## _Truthy_ and _falsey_

When evaluating objects in `if` statements or other boolean contexts, all objects evaluate as _truthy_ **except** for `false` and `nil`.

## Control flow

_Truthy_ and _falsey_ evaluations are useful in the context of control flow. Like in procedural languages, Ruby has an `if`...`else` construct, but it may be more common to use `if` as a "guarding" statement to modify the evaluation of an expression.

```ruby
def falsey
  nil || false
end

def truthy
  not falsey
end

if truthy
  # this block is evaluated
end

if falsey
  # this block is not evaluated
else
  # this block is evaluated
end

1 + 1 if truthy
# => this will evaluate and return 2

2 + 2 if falsey
# => the numbers are not added because of the modifier, nil is returned
```

Sometimes `unless` is used in place of `if` to provide an inverse condition

```ruby
3 + 3 unless truthy
# => the numbers are not added because of the modifier, nil is returned

4 + 4 unless falsey
# => this will evaluate and return 8
```

[nil-dictionary]: https://www.merriam-webster.com/dictionary/nil
