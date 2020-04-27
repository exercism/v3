
Pattern matching in Elixir is not an add-on feature: it is a fundamental part of the language, so it's hard to isolate the concept into a single exercise. Don't be lulled into thinking that `=` just means "variable assignment" like it does in other languages: in Elixir, `=` is the **match operator**, and it does some amazing things.

## Similarity in Other Languages

Although pattern matching is more common in functional languages, many object-oriented languages offer varying degrees of similar functionality if you know where to look...

### Python

In Python, you get a taste of this when you [unpack](https://www.python.org/dev/peps/pep-0448/) arguments using the `*` "splat" or "double-splat" operators:

```python
def my_fun(a, b, c, d):
    print(a, b, c, d)

my_list = [1, 2, 3, 4]

# Unpack the list into four arguments
my_fun(*my_list)
```

### PHP

PHP has the (seldom used) [list construct](https://www.php.net/manual/en/function.list.php):

```php
<?php
$my_array = array("apple", "boy", "cat");
list($a, $b, $c) = $my_array;
print($a); // apple
```

[Ruby 2.7](https://www.ruby-lang.org/en/news/2019/12/25/ruby-2-7-0-released/) includes pattern matching as an experimental feature, but perhaps an easier point of comparison is how Ruby can do multiple variable assignment (like PHP's `list` construct), e.g. from a function's output:

```ruby
def min_max(array)
  [array.min, array.max]
end
min, max = min_max([3,5,2])
min # => 2
max # => 5
```

And like Python, Ruby supports the `*` splat and `**` double-splat operators:

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

Hopefully the previous examples from other languages will help make Elixir's syntax more familiar to you, and these examples help emphasize the importance of _destructuring_ data, and destructuring more complex data types is one of the most important things that the match operator `=` does for us. In a word, pattern matching is powerful.

For example, if you only only need a single value from a map? Pattern match. If you only need one item out of a list? Pattern match. If you only need the last part of a string? Pattern match.  There are other ways to achieve the same results, but in Elixir, using the match operator is often the quickest and most idiomatic way to accomplish these types of tasks.

Eventually, we will see that pattern matching has uses that go beyond this: they allow you to forgo if-statements almost entirely and even to define multiple versions of the same function!

The basics of the match operator look pretty much like regular old assignment:

```iex
iex> x = 1
1
iex> 1 = x
1
iex> 2 = x
** (MatchError) no match of right hand side value: 1
```

## Destructuring Data

Matches start to get interesting when you deconstruct complex data. One approach to extracting data out of a pattern is to line up copies of the data, and then place variables in the left-hand structure where you want to capture data. Here is an example with a tuple:

```iex
iex> {:ok, "Success"} =  {:ok, "Success"}
{:ok, "Success"}
# Replace the desired values with variables:
iex> {x, y} = {:ok, "Success"}
{:ok, "Success"}
iex> x
:ok
iex> y
"Success"
```
By matching on the same "shape" of data, you can destructure data into component parts.

Here is another breakdown of matching the "shape", this time on a map:

```iex
iex> %{id: 552, username: "hasan"} = %{id: 552, username: "hasan"}
%{id: 552, username: "hasan"}

# Replace the desired value with a variable:
iex> %{id: 552, username: username} = %{id: 552, username: "hasan"}
%{id: 552, username: "hasan"}
iex> username
"hasan"

# Or more simply:
iex> %{username: username} = %{id: 552, username: "hasan"}
%{id: 552, username: "hasan"}
iex> username
"hasan"
```
You can easily "boil down" a large map with many keys into a smaller map by constructing a pattern in this way.

## The Truthiness of a Match

You will see the match operator playing a role in tests:

```elixir
assert %{username: username} = %{id: 552, username: "hasan"}  # Pass!
```

When this test passes, it is saying "yes, this match succeeded." Sometimes your tests will use the `==` operator, which checks for equality and does *not* perform pattern matching:

```elixir
assert %{username: username} == %{id: 552, username: "hasan"}  # Fail!
```

Elixir can use this "truthiness" quality to check multiple patterns to see if there's a match. We see this frequently inside of `case` statements: the first clause that matches is the one that will be executed.

Elixir functions often return two-element tuples like `{:ok, "result"}`, so a `case` statement using pattern matching is a common control structure:

```elixir
case do_the_function() do
  {:ok, results} -> IO.puts("The function returned :ok!")
  {:error, _error} -> IO.puts("There was a problem!")
end
```

Pattern matching is how web frameworks handle URL routing: the first pattern that matches the incoming URL is the one that handles the request.

We will leverage this "finding a match" process more thoroughly in another exercise when explore multi-clause functions and guard clauses.

## The Underscore to Ignore Values

When pattern matching, it's common to discard some of the results. Elixir uses the underscore prefix for any variables that will be discarded by the compiler, or just the `_` all by itself.  This pattern is common when you only need to check that an operation succeeded, for example:

```elixir
{:ok, _ignored} = {:ok, "Anything goes!"}
{:ok, _} = {:ok, "Great success!"}
{:ok, _} = {:ok, 42}
```

This same `_` strategy is used any time your pattern needs a placeholder. Placeholders may be required for ordered data (primarily lists and tuples). For example, if you need to extract the second item from a list, you could do the following:

```
iex> [_, x] = ["foo", "bar"]
["foo", "bar"]
iex> x
"bar"
```

We will cover lists in more depth separately when we discuss tail recursion.

## Matching Inside Function Arguments

It's common to put your pattern matching inside your function's arguments.  For example, if you wrote a function that returned the second item from a given tuple, you might write something like this:

```elixir
# Don't do this:
def second_element(tuple) do
  {_, second} = tuple
  second
end
```

But it would be more idiomatic to do the match inside the function signature, e.g.

```elixir
# This is better
def second_element({_, second}) do
  second
end
```

This has important benefits when we deal with multi-clause functions, but we're getting ahead of ourselves. For now, let's focus on matching within lists and maps since they are the most common places where pattern matching gets used.
