In this exercise you'll be processing log-lines.

Each log line is a string formatted as follows: `"[<LEVEL>]: <MESSAGE>"`.

There are three different log levels:

- `INFO`
- `WARNING`
- `ERROR`

You have three tasks.

### 1. Output log line

Define a `LogLevel` enum that has three elements, these are the numbers assigned to each of them:

- `Info` - 1
- `Warning` - 2
- `Error` - 3

Next, implement a method that will output a log line, given the `LogLevel` and the message:

```csharp
LogLine.OutputLogLine(1, "File deleted")
// Returns: "[Info]: File deleted"
```

### 2. Create a more verbose log line

The system needs to improve by being a bit more verbose. This time, the displayed log levels are more than one word. Aside from that a new level is added to differentiate between errors. Here's the updated list:

- "Information Message" - 1
- "Warning" - 2
- "Minor Error" - 3
- "Critical Error" - 4

```csharp
LogLine.OutputVerboseLogLine(4, "Invalid operation")
// Returns: "[CRITICAL ERROR]: Invalid operation"
```

### 3. Convert log line to short format

The log level of a log line is quite verbose. To reduce the disk space needed to store the log lines, a short format is developed: `"[<ENCODED_LEVEL>]:<MESSAGE>"`.

The encoded log level is simple mapping of a log level to a number:

- `Info` -> `1`
- `Warning` -> `2`
- `MinorError` -> `4`
- `CriticalError` -> `4`

Implement a method that can output the shortened log line format:

```csharp
LogLine.OutputForShortLog(LogLevel.Error, "Stack overflow")
// Returns: "4:Stack overflow"
```
