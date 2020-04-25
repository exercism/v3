In Elixir pattern matching isn't an add-on feature, it is a fundamental part of the language: even the equals sign isn't an assigment operator: it is the **match operator**. Don't be lulled into thinking that `=` just means "variable assignment" like it does in other languages.

## Similarity in Other Languages

Although pattern matching is more common in functional languages, many object-oriented languages offer some similar functionality if you know where to look...

In Python, you get a taste of this when you unpack arguments using the `*` "splat" or "double-splat" operators:

```python
def my_fun(a, b, c, d):
    print(a, b, c, d)


my_list = [1, 2, 3, 4]

# Unpack the list into four arguments
my_fun(*my_list)
```

PHP has the (seldom used) [list construct](https://www.php.net/manual/en/function.list.php):

```php
<?php
$my_array = array("apple", "boy", "cat");
list($a, $b, $c) = $my_array;
print($a); // apple
```

[Ruby 2.7](https://www.ruby-lang.org/en/news/2019/12/25/ruby-2-7-0-released/) now includes pattern matching as an experimental feature, but perhaps an easier point of comparison is how Ruby can do multiple variable assignment (like PHP's `list` construct), e.g. from a function's output:

```ruby
def min_max(array)
  [array.min, array.max]
end
min, max = min_max([3,5,2])
min # => 2
max # => 5
```

Like Python, Ruby supports the `*` splat and `**` double-splat operators:

```ruby
def divide(numerator, denominator)
  numerator / denominator
end
divide(*[4,2]) # => 2
```

Javascript offers more thorough deconstructing functionality:

```javascript
let a, b, rest;

[a, b, ...rest] = [10, 20, 30, 40, 50];
console.log(a); // 10
console.log(b); // 20
console.log(rest); // [30, 40, 50]
```


## Pattern Matching in Elixir

Looking over the previous examples from other languages helps emphasize the importance of _destructuring_ data, and destructuring more complex data types is one of the most important things that the match operator `=` does for us. In a word, pattern matching is powerful.

For example, if you only only need a single value from a map? Pattern match. If you only need one item out of a list? Pattern match. If you only need the last part of a string? Pattern match.  There are other ways to achieve the same results, but in Elixir, using the match operator is often the quickest and most idiomatic way to accomplish these types of tasks. Eventually, we will see that pattern matching has uses that go beyond this: they allow you to forgo if-statements almost entirely and even to define multiple versions of the same function!

The basics of the match operator look pretty much like regular old assignment:

```iex
iex> x = 1
1
iex> 1 = x
1
iex> 2 = x
** (MatchError) no match of right hand side value: 1
```
But things start to get interesting when you start to destructure more complex data:

```iex
iex> {x, y} = {:ok, "Success"}
{:ok, "Success"}
iex> x
:ok
iex> y
"Success"
```

By matching on the same "shape" of data, you can destructure data into component parts.

A common pattern in Elixir is to use two-element tuples like `{:ok, "result"}` because this allows us to easily use pattern matching to route the execution flow, e.g. via a `case` statement:

```elixir
x = {:ok, ["a", "b", "c"]}
case x do
  {:ok, results} -> IO.inspect(results)
  {:error, _error} -> IO.puts("There was a problem!")
end
```

You can see that Elixir relies on pattern matching inside the `case` statement.

When pattern matching, it's common to discard some of the results. Elixir uses the underscore prefix for any variables that will be discarded by the compiler, or just the `_` all by itself.  This pattern is common when you only need to check that an operation succeeded, for example:

```elixir
{:ok, _ignored} = {:ok, "Anything goes!"}
{:ok, _} = {:ok, "Great success!"}
{:ok, _} = {:ok, 42}
```

This same `_` strategy is used any time your pattern needs a placeholder. For example, if you need to extract the second item from a list, you could do the following:

```
iex> [_, x] = ["foo", "bar"]
["foo", "bar"]
iex> x
"bar"
```

When matching on lists, you will need to use the `|` operator when you don't know the list's exact size: it designates the tail of the list. Our previous example would fail for lists with 3 or more items.

```iex
iex> [_, x] = [1, 2, 3]
** (MatchError) no match of right hand side value: [1, 2, 3]
```

But if we add the `|` to designate the tail of the list, everything works as expected:

```iex
iex> [_, x | tail] = [1, 2, 3]
[1, 2, 3]
iex> x
2
iex> tail
[3]
```

Remember that the tail will always contain a list.

Commonly, you can simply ignore the tail by using an underscore:

```iex
iex> [_, x | _tail] = [1, 2, 3]
```

It's common to put your pattern matching right inside your function's arguments.  For example, if you wrote a function that returned the first item from a given list, you might write something like this:

```elixir
# Don't do this:
def first_element(list) do
  [first | _tail] = list
  first
end
```

But it would be more idiomatic to do the match inside the function signature, e.g.

```elixir
# This is better
def first_element([first | _tail]) do
  first
end
```

However, Elixir already provides a function that returns the head (i.e. the first element) of a list: [`hd/1`](https://hexdocs.pm/elixir/Kernel.html#hd/1).
