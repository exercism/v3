# Instructions

In this exercise you'll be processing log lines.

Each log line is a string formatted as follows: `"[<LEVEL>]: <MESSAGE>"`.

There are three different log levels:

- `INFO`
- `WARNING`
- `ERROR`

You have three tasks.

### 1. Parse log level

Define a `LogLevel` enum that has three elements:

- `Info`
- `Warning`
- `Error`

Next, write a function to parse the log level of a log line:

```rust
parse_log_level("[INFO]: File deleted");
// Returns: LogLevel::Info
```

### 2. Support unknown log level

Unfortunately, occasionally some log lines have an unknown log level. To gracefully handle these log lines, add an `Unknown` member to the `LogLevel` enum which should be returned when parsing an unknown log level:

```rust
parse_log_level("[FATAL]: Invalid operation")
// Returns: LogLevel::Unknown
```
### 3. Convert log line to short format

The log level of a log line is quite verbose. To reduce the disk space needed to store the log lines, a short format is developed: `"[<ENCODED_LEVEL>]:<MESSAGE>"`.

The encoded log level is simple mapping of a log level to a number:

- `Unknown` -> `0`
- `Info` -> `1`
- `Warning` -> `2`
- `Error` -> `4`

Write a function that can output the shortened log line format:

```rust
output_for_short_log(LogLevel::Error, "Stack overflow")
// Returns: "4:Stack overflow"
```
