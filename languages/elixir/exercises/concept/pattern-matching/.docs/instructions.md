In this exercise you will convert the output of two different date functions into strings.

### 1. Define the `DateString.from_tuple/1` function

The Erlang [:calendar.universal_time/0](http://erlang.org/doc/man/calendar.html#universal_time-0) function can be used by Elixir to return the current UTC datetime as a tuple.

```iex
iex> :calendar.universal_time()
{{2020, 4, 26}, {20, 31, 37}}
```

Using pattern matching, convert the given tuples to strings in the common `YYYY-MM-DD HH:MM:SS` format.



### 2. Define the `DateString.from_map/1` function

The `DateTime.utc_now/0` function returns a struct that represents the current UTC time.  It can be converted to a map for easier inspection using the `Map.from_struct/1` function:

```iex
iex> DateTime.utc_now() |> Map.from_struct()
%{
  calendar: Calendar.ISO,
  day: 26,
  hour: 20,
  microsecond: {514717, 6},
  minute: 32,
  month: 4,
  second: 45,
  std_offset: 0,
  time_zone: "Etc/UTC",
  utc_offset: 0,
  year: 2020,
  zone_abbr: "UTC"
}
```

`DateTime` structs can also be generated using the `~U` sigil, so the above is equivalent to the following:

```iex
iex> ~U[2020-04-26 20:32:32.633567Z] |> Map.from_struct()
```

Using pattern matching, convert the given maps to strings in the common `YYYY-MM-DD HH:MM:SS` format.
