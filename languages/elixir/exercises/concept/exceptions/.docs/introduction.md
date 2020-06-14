All errors in Elixir implement the _Exception Behaviour_. Just like the _Access Behaviour_, the _Exception Behavior_ defines callback functions that a module must implement to fulfill the software contract of the behaviour. Once an errors is defined, it has the following properties:

- The module's name defines the error's name.
- The module defines an error-struct.
- The module will have a `:message` field.
- The module can be be used with `raise/1` and `raise/2` to raise the intended error

The _Exception Behaviour_ also specifies two required callbacks: `message/1` and `exception/1`. Despite being required, `message/1` is often left to be implemented by the _behavior_, but it is common to implement `exception/1` instead to get custom error messages.

## Defining an exception

To define an exception from an error module, we use the `defexception` macro function:

```elixir
# Defines a minimal error, with the name `MyError`
defmodule MyError do
  defexception [:message]
end

# Defines an error with a customized exception/1 function
defmodule MyCustomizedError do
  defexception [:message]

  @impl true
  def exception(value) do
    case value do
      [] ->
        msg = "service did not get what was expected"
        %MyCustomizedError{message: msg}
      _ ->
        %MyCustomizedError{message: value}
    end
  end
end
```

## Using exceptions

Defined errors may be used like a built in error:

```elixir
raise MyError, "an error occured"
# => ** (MyError) an error occured

raise MyCustomizedError
# => ** (MyCustomizedError) service did not get what was expected

raise MyCustomizedError, "an error occured"
# => ** (MyCustomizedError) an error occured
```

If the defined error does not implment `exception/1` a string must be passed along with the message. If `exception/1` is defined, it is not neccessary.
