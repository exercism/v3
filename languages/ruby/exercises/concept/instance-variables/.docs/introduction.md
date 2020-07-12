## Object state, instance variables

Objects can hold their own state by setting _instance variables_. _Instance variables_ are created a prefixing `@` to the variable name.

```ruby
@instance_variable = 2
```

This is often done by defining an `initialize` method to set an initial state.

```ruby
class Airplane
  def initialize
    @wings = 2
  end
end
```

The `initialize` method may also take arguments, so that each instance can start with a custom state:

```ruby
class Suitcase
  def initialize(locked)
    @locked = locked
  end
end
```

Consider _instance variables_ to be private from external read and writes. _Class instance methods_ should be used for getting and setting instance variables:

```ruby
class Suitcase
  #...

  def locked? # Methods returning true or false should have trailing `?`
    @locked
  end

  def unlock! # Methods which mutate state should have trailing `!`
    @locked = false
  end
end
```

## Nil

[Nil][nil-dictionary] is an English word meaning "nothing" or "zero". In Ruby, `nil` is a value used to express the _absence_ of an object. In other programming languages, `null` or `none` values may play a similar role.

```ruby
# I do not have a favorite color
favorite_color = nil
```

Ruby gives any undefined instance variable the default value of `nil` when it is accessed before being set.

[nil-dictionary]: https://www.merriam-webster.com/dictionary/nil
