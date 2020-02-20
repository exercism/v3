# Instructions

In this exercise you'll be processing log-lines.

Each log line is a string formatted as follows: `"[<LEVEL>]: <MESSAGE>"`.

There are three different log levels:

- `INFO`
- `WARNING`
- `ERROR`

You have three tasks, each of which will take a log line and ask you to do something with it.

### 1. Extract a message from a log line

Implement a method to return a log line's message:

```python
loglines.extract_message("[ERROR]: Invalid operation")
// Returns: "Invalid operation"
```

The message should be trimmed of any whitespace.

```python
loglines.extract_message("[ERROR]: Invalid operation.\t\n")
// Returns: "Invalid operation."

### 2. Change a message's loglevel.

Implement a method that replaces a log line's current log level with a new one:

```python
loglines.change_log_level("[INFO]: Fatal Error.", "ERROR")
/// Returns: "[ERROR]: Fatal Error."
```

### 3. Reformat a log line

Implement a method that reformats the log line, putting the message first and the log level after it in parentheses:

```python
loglines.reformat("[INFO]: Operation completed")
// Returns: "Operation completed (info)"
```
