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

### 1. Output log line

Define a `LogLevel` enum that has seven elements. Here are the log levels and their corresponding messages from the system.

- `Trace` - "[TRC] Here is the stack trace: ..."
- `Debug` - "[DBG] Solve this problem by ..."
- `Info` - "[INF] Processing..."
- `Warning` - "[WRN] Unsuspicious activity detected"
- `Error` - "[ERR] Missing semicolon;"
- `Fatal` - "[FTL] Stack Overflow"

Next, implement a method that will output a log line, given the `LogLevel` and the message:

```csharp
LogLine.ParseLogLevel("[INF]: File deleted")
// Returns: LogLevel.Info
```

### 2. Create a more verbose log line

The system needs to improve by being a bit more verbose. This time, the displayed log levels are more than one word. Aside from that a new level is added to differentiate between errors. Here's the updated list:

- "Information Message" - 1
- "Warning" - 2
- "Minor Error" - 3
- "Critical Error" - 4

```csharp
LogLine.ParseLogLevel("[XYZ]: Overly specific, out of context message")
// Returns: LogLevel.Unknown
```

### 3. Convert log line to short format

The log level of a log line is quite verbose. To reduce the disk space needed to store the log lines, a short format is developed: `"[<ENCODED_LEVEL>]:<MESSAGE>"`.

The encoded log level is simple mapping of a log level to a number:

- `Trace` - `0`
- `Debug` - `1`
- `Info` - `4`
- `Warning` - `5`
- `Error` - `6`
- `Fatal` - `7`
- `Unknown` - `42`

Implement a method that can output the shortened log line format:

```csharp
LogLine.OutputForShortLog(LogLevel.Error, "Stack overflow")
// Returns: "6:Stack overflow"
```
