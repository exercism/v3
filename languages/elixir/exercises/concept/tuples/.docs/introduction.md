In Elixir, a tuple is a data structure which organizes data in contiguous memory. It behaves as like static data structures, holding a fixed number of items, but doesn't have explicit names for each element. Tuples are often used in Elixir for memory read-intensive operations, since read-access of an element is a constant-time operation. They are not usually used when elements may need to be added/removed dynamically because rather than modifying the existing tuple, a new tuple is created which requires memory to be allocated upfront.

In practice, tuples are created in Elixir using curly braces, and element can be individually accessed using the `elem/1` function using 0-based indexing:

```elixir
empty_tuple = {}
one_element_tuple = {1}
multiple_element_tuple = {1, :a, "hello"}

elem(multiple_element_tuple, 2)
# => "hello"
```

## Tuples as grouped information

Tuples are often used in practice to represent grouped information.

```elixir
function_call()
# => {:ok, "response"} indicating success and the response

File.read("non-existent-file.txt")
# => {:error, :enoent} indicating an error occurred, and which error

Float.ratio(0.25)
# => {1, 4} indicating the numerator and denominator of the fraction ¼
```

## Pattern matching basics with tuples

The use of pattern matching is dominant in clear, assertive, idiomatic Elixir code. You might recall that `=/2` is described as a match operator rather than as an assignment operator. When elixir invokes the `=/2` function, it performs a pattern match from the right side of the operator, to the left side of the operator. If the pattern on the left matches the right, any variables on the left are bound, and the function returns the value of the right side. A `MatchError` is raised if there is no match.

```elixir
2 = 2
# => 2
# Literals can be matched

2 = 3
# => ** (MatchError) no match of right hand side value: 3

{:ok, contents} = File.read("existent-file.txt")
# => {:ok, "contents of existent-file as a string"}
# the variable `contents` is bound to the string contents of the file

{_, denominator} = Float.ratio(0.25)
# => {1, 4}
# the variable `denominator` is bound to the value 4
```

In the last example, in a pattern match, if we don't need a variable, we can discard it by referencing `_`. Any variable starting with an `_` is not tracked by the run-time.

## Pattern matching in named functions

Pattern matching is also a useful tool when creating multiple function clauses. Pattern matching can be used on the functions' parameters which then determines which function clause to invoke.

```elixir
defmodule Example do
  def named_function(:a) do
    1
  end

  def named_function(:b) do
    2
  end
end

Example.named_function(:a)
# => 1
# The first function clause matches, so it is invoked

Example.named_function(:b)
# => 2
# The first clause does not match, but the second does, so it is invoked

Example.named_function(:c)
# => ** (FunctionClauseError) no function clause matching in Example.named_function/1
# None of the function clauses match, so an error is raised
```
