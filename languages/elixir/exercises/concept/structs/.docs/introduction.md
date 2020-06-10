Structs are an extension built on top of maps which provide compile-time checks and default values.[1] A struct is named after the module it is defined in. To define a struct use the `defstruct` construct. The construct usually immediately follows after the module definition. `defstruct` accepts either a list of atoms (for nil default values) and key-value tuples (for specified default values). The fields without defaults must precede the fields with default values.

```elixir
defmodule Plane do
  defstruct [:engine, wings: 2]
end

plane = %Plane{}
# => %Plane{engine: nil, wings: 2}
```

## Accessing fields and updating

Since structs are bare maps underneath, we can use any method that works with maps:

- get/fetch field values:

  ```elixir
  plane = %Plane{}
  plane.engine
  # => nil
  Map.fetch(plane, :wings)
  # => 2
  ```

- update field values

  ```elixir
  plane = %Plane{}
  %{plane | wings: 4}
  # => %Plane{engine: nil, wings: 4}
  ```
