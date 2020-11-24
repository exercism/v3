In this exercise you'll be processing log-lines.

Each log line is a string formatted as follows: `"[<LVL>]: <MESSAGE>"`.

These are the different log levels:

- `TRC` (trace)
- `DBG` (debug)
- `INF` (info)
- `WRN` (warning)
- `ERR` (error)
- `FTL` (fatal)

You have three tasks, each of which need to be completed to provide the necessary functionality.

## 1. Create LogLevel enum

Define a `LogLevel` enum that has six cases corresponding to the above log levels, plus an unknown case for levels with missing or non-standard log levels.

- `trace`
- `debug`
- `info`
- `warning`
- `error`
- `fatal`
- `unknown`

## 2. Parse log level

Next, implement the `LogLine` initializer to parse the log level of a log line:

```swift
LogLine("[INF]: File deleted")
// => LogLevel.info
LogLine("Something went wrong!")
// => LogLevel.unknown
```

## 3. Convert log line to short format

The log level of a log line is quite verbose. To reduce the disk space needed to store the log lines, a short format is developed: `"<ENCODED_LEVEL>:<MESSAGE>"`.

The encoded log level is simple mapping of a log level to a number:

- `trace` - `0`
- `debug` - `1`
- `info` - `4`
- `warning` - `5`
- `error` - `6`
- `fatal` - `7`
- `unknown` - `42`

Implement the `shortFormat(message: String) -> String` method that can output the shortened log line format as a `String`:

```swift
let overflow = LogLevel.error
overflow.shortFormat(message: "Stack overflow")
// => "6:Stack overflow"
```
