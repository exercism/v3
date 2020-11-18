In this exercise, you'll be processing log messages with six severity levels.

Each log line is a string formatted as follows: `"[<LVL>]: <MESSAGE>"`.

These are the different log levels:

| LEVEL     | LVL   |
| --------- | ----- |
| `Trace`   | `TRC` |
| `Debug`   | `DBG` |
| `Info`    | `INF` |
| `Warning` | `WRN` |
| `Error`   | `ERR` |
| `Fatal`   | `FTL` |

## 1. Parse log level

Define a `LogLevel` enum that has six elements corresponding to the log levels defined above.

Define the `parse_log_level` function which takes the log message as parameter and returns the enum member of its level.

```python
parse_log_level("[INF]: File deleted")
#=> LogLevel.Info
```

## 2. Support unknown log level

Unfortunately, occasionally some log messages have an unknown log messages. To gracefully handle these log messages in the function `parse_log_level`, add an `Unknown` member to the `LogLevel` enum which is returned when parsing an unknown log level:

```python
parse_log_level("[XYZ]: Overly specific, out of context message")
#=> LogLevel.Unknown
```

## 3. Convert a log message to the short format

The log level of a log line is quite verbose. To reduce the disk space needed to store the log messages, a short format is defined: `"[<CODE_LEVEL>]:<MESSAGE>"`.

The code log level is following a simple mapping:

| LEVEL     | CODE |
| --------- | ---- |
| `Trace`   | `0`  |
| `Debug`   | `1`  |
| `Info`    | `4`  |
| `Warning` | `5`  |
| `Error`   | `6`  |
| `Fatal`   | `7`  |
| `Unknown` | `42` |

Define the `convert_to_short_log()` which takes this two parameters:

1. Log level - The Log level of the log sent. ex: LogLevel.Error
2. Log Message - The message of type str.

```python
convert_to_short_log(LogLevel.Error, "Stack overflow")
# => "6:Stack overflow"
```

## 4. Create an Alias

It looks like the user has created logs for `LogLevel.Warn` instead of `LogLevel.Warning`. Create an `alias` for `LogLevel.Warning` and return the new alias member in the function `get_warn_alias`.

This can be done on the same enum class `LogLevel` already defined at the top of the file.

Note: Both the LogLevels should point to same value. ie: `LogLevel.Warning = "WRN"` & `LogLevel.Warn = "WRN"`

```python
get_warn_alias()
#=> LogLevel.Warn

get_warn_alias() == LogLevel.Warning
#=> True
```

## 5. All Member Names and Values

Define the function `get_members()`.

This function should return a list of tuple `(name, value)` with all the members of the enum `LogLevel`.

```python
get_members()
#=> [('Trace', 'TRC'), ('Debug', 'DBG'), ('Info', 'INF'), ('Warning', 'WRN'),
# ('Error', 'ERR'), ('Fatal', 'FTL'), ('Unknown', 'UKN')]
```
