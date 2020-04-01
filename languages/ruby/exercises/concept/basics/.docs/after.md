Ruby is a dynamic and strong typed language. On dynamic languages the type of a variable/object is resolved at runtime, which means that its value/type can be changed until the very last minute (when it gets parsed by the interpreter).
And what do we mean with strong typed? Once we know the type of variable/object, Ruby is strict about what you can do with it, for example:

```ruby
x = '2'
y = x + 'n'
=>  '2n'
```

**But**

```ruby
x = '2'
y = x + 2
=> TypeError (no implicit conversion of Integer into String)

And remember, in Ruby everything is an object even classes are instances of the class `Class`, for example:

```Ruby
1.class
=> Integer

Integer.instance_of?(Class)
=> true
```

This means that we can also define classes like this:

```ruby
Car = Class.new do
  def run
    'running'
  end
end

Car.new.run
=> 'running'
```

Finally, bear in mind that the `Integer` object holds values that may be defined as one or more (consecutive) digits and its methods support most of the [default mathematical operators][integers-docs].

[integers-docs]: https://ruby-doc.org/core-2.7.0/Integer.html
