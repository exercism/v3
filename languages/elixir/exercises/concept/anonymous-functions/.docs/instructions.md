In this exercuse, you'll be creating some auxillary functions in order to simplify to creation of closure functions.

For each task, make use of a closure and return a function that can be invoked from the calling scope.

You have six tasks:

### 1. Create an adder

Implement `ClosureMaker.make_adder/1`, have it return a function which takes one parameter and adds it to the parameter passed in to `make_adder`.

```elixir
adder = ClosureMacker.make_adder(2)
adder.(2)
# => 4
```

### 2. Create a subtractor

Implement `ClosureMaker.make_subtractor/1`, have it return a function which takes one parameter and subtracts the parameter passed in to `make_subtractor`.

```elixir
subtractor = ClosureMacker.make_subtractor(2)
subtractor.(3)
# => 1
```

### 3. Create a multiplier

Implement `ClosureMaker.make_multiplier/1`, have it return a function which takes one parameter and multiplies it by the parameter passed in to `make_multiplier`.

```elixir
multiplier = ClosureMacker.make_multiplier(7)
multiplier.(3)
# => 21
```

### 4. Create a divider

Implement `ClosureMaker.make_divider/1`, have it return a function which takes one parameter and divides it by the parameter passed in to `make_divider`.

```elixir
divider = ClosureMacker.make_divider(81)
divider.(9)
# => 9
```

Make use of integer division.

### 5. Create a list appender

Implement `ClosureMaker.make_list_appender/1`, have it return a function which takes one list parameter and appends the list parameter passed in to `make_list_appender`.

```elixir
list_appender = ClosureMacker.make_list_appender([7, 8, 9])
list_appender.([1, 2, 3])
# => [1, 2, 3, 7, 8, 9]
```

### 6. Create a string appender

Implement `ClosureMaker.make_string_appender/1`, have it return a function which takes one string parameter and appends the string parameter passed in to `make_string_appender`.

```elixir
string_appender = ClosureMacker.make_string_appender("James Bond")
string_appender.("Bond...")
# => "Bond...James Bond"
```
