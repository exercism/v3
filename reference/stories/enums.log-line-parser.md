# Log line parser

## Story

In this exercise you'll be processing log-lines.

Each log line is a string formatted as follows: `"[<LVL>]: <MESSAGE>"`.

These are the different log levels:

- `TRC` (trace)
- `DBG` (debug)
- `INF` (info)
- `WRN` (warning)
- `ERR` (error)
- `FTL` (fatal)

## Tasks

These are example tasks that fit the log line parser:

- Parse log level to enum
- Support unknown log levels
- Convert a log line to a different format with the log level converted to a numeric value

## Implementations

- [C#: enums][implementation-csharp] (reference implementation)
- [Rust: enums][implementation-rust]
- [Swift: enums][implementation-swift]

[implementation-csharp]: ../../languages/csharp/exercises/concept/enums/.docs/instructions.md
[implementation-rust]: ../../languages/rust/exercises/concept/enums/.docs/instructions.md
[implementation-swift]: ../../languages/swift/exercises/concept/enums/.docs/instructions.md
