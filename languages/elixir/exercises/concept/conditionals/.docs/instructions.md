In this exercise you'll be mapping logging codes to a label.

These are the different log codes:

- 0 (trace)
- 1 (debug)
- 4 (info)
- 5 (warning)
- 6 (error)
- 7 (fatal)

You have two tasks:

### 1. Return the logging code label

Implement the `LogLevel.to_label/1` function to return the label of a log's line from an integer code:

```elixir
LogLevel.to_label(1)
# => :debug
```

### 2. Support unknown logging codes

Unfortunately, some log lines occasionally have an unknown log code.  To gracefully handle these log lines, add an _unknown_ label to the `LogLevel.to_label/1` function.

```elixir
LogLevel.to_label(10)
# => :unknown
```
