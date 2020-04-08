Ruby is a dynamic [object-oriented language]. Everything in Ruby is an [object][object].

There are two ways to assign objects to names in Ruby - assigning variables or constants. Variables always start with lower-case letters and use [snake case][snake-case] for their formatting. A variable can be redefined to different objects over its lifetime. For example, `my_first_variable` can be defined and redefined many times using the `=` operator:

```ruby
my_first_variable = 1
my_first_variable = "Some string"
my_first_variable = SomeComplexObject.new
```

Constants, however, can only be assigned once. They must start with capital letters and are normally written in block capitals with words separated by underscores. For example:

```ruby
MY_FIRST_CONSTANT = 10

# Redefining not allowed
# MY_FIRST_CONSTANT = "Some String"
```

Ruby is organised into classes. Classes are defined using the `class` keyword followed by the name of the class. Classes are initialized using the `.new` method.

```ruby
# Define the class
class Calculator
  #...
end

# Create an instance of it and assign it to a variable
my_first_calc = Calculator.new
```

A function within a class is referred to as a _method_. Each method can have zero or more parameters. Objects are returned from methods using the `return` keyword. If no return value is specified, the final object in the method is returned instead. Methods can also have named parameters which are defined and called using the `:` syntax.  Methods are invoked using `.` syntax.

```ruby
class Calculator

  # Unnamed params
  def add(num1, num2)
    return num1 + num2 # Explicit return
  end

  # Named params
  def multiply(num1:, num2:)
    num1 * num2 # Implicit return
  end
end

calc = Calculator.new
calc.add(1, 3)
calc.multiply(num1: 2, num_2: 5)
```

[object-oriented-programming]: https://ruby-doc.org/docs/ruby-doc-bundle/UsersGuide/rg/oothinking.html
[object]: ../../../../../../reference/concepts/objects.md
[snake-case]: https://en.wikipedia.org/wiki/Snake_case
