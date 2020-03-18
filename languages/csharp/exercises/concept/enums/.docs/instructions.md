# Instructions

In this exercise you'll be processing log-lines.

Each log line is a string formatted as follows: `"[<LVL>]: <MESSAGE>"`.

These are the different log levels:

- `Unknown`
- `Trace`
- `Debug`
- `Info`
- `Warning`
- `Error`
- `Fatal`

You have three tasks.

### 1. Parse log level

Define a `LogLevel` enum that has seven elements. Here are the log levels and their corresponding messages from the system.

- `Unknown` - "[UNK] Message..."
- `Trace` - "[TRC] Here is the stack trace: ..."
- `Debug` - "[DBG] Solve this problem by ..."
- `Info` - "[INF] Processing..."
- `Warning` - "[WRN] Unsuspicious activity detected"
- `Error` - "[ERR] Missing semicolon;"
- `Fatal` - "[FTL] Stack Overflow"

Next, implement a method to parse the log level of a log line:

```csharp
LogLine.ParseLogLevel("[INF]: File deleted")
// Returns: LogLevel.Info
```

### 2. Support unknown log level

Unfortunately, occasionally some log lines have an unknown log level. To gracefully handle these log lines, add an `Unknown` element to the `LogLevel` enum which should be returned when parsing an unknown log level:

```csharp
LogLine.ParseLogLevel("[FATAL]: Invalid operation")
// Returns: LogLevel.Unknown
```

### 3. Convert log line to short format

The log level of a log line is quite verbose. To reduce the disk space needed to store the log lines, a short format is developed: `"[<ENCODED_LEVEL>]:<MESSAGE>"`.

The encoded log level is simple mapping of a log level to a number:

- `Unknown` -> `0`
- `Info` -> `1`
- `Warning` -> `2`
- `Error` -> `4`

Implement a method that can output the shortened log line format:

```csharp
LogLine.OutputForShortLog(LogLevel.Error, "Stack overflow")
// Returns: "4:Stack overflow"
```
