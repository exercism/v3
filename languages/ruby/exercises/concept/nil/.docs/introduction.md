[Nil][nil-dictionary] is an English word meaning "nothing" or "zero". In Ruby, `nil` is a special value to express the _absence_ of an object.

```ruby
# I do not have a favorite color
favorite_color = nil
```

In other programming languages, `null` or `none` values might play a similar role.

## `nil?`

To determine if a variable holds a `nil` value, the `nil?` method returns a boolean value: `true` is returned if the value is `nil`, `false` otherwise.

```ruby
favorite_color = nil
if favorite_color.nil? do
  "I do not have a favorite color."
else
  "My favorite colors is #{favorite_color}."
end
# => "I do not have a favorite color."
```

It is also possible to write an `if` expression on a single line. Note the comma after the condition.

```ruby
if favorite_color.nil? then "no favorite" else "favorite"
```

It may also be written using a shorthand syntax called a `ternary expression`.

```ruby
favorite_color.nil? ? "no favorite" : "favorite"
```

These syntaxes are helpful for very short expressions, but should be avoided if the expression won't fit on a single line.

## _Truthy_ and _falsy_

In Ruby, all objects evaluate to a _truthy_ or _falsy_ value when they are encountered in a boolean context (like an `if` expression). All data is considered _truthy_ **except** for _false_ and _nil_.

[nil-dictionary]: https://www.merriam-webster.com/dictionary/nil
