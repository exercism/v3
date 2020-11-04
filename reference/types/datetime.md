# DateTime

## The Concept

A compound type combining [Date][type-date] and [Time][type-time] into one. Some DateTime types have _timezone information_.

The difference between two times may be representable by a [Duration][type-duration].

## What to cover

- **Language-specific implementation**: is it a single type with time and date components, or are the time and date components separate types?
- **Create a date/time**: how can one create a DateTime representing a specific date and time.
- **Get the current date/time**: how can one get a DateTime representing the current date and time.
- **Comparing DateTimes**: how to compare two DateTimes instances, for example to see if a date is in the past. Are there special (or standard) operators one can use?
- **Convert a string to a DateTime**: how to parse a string to a DateTime? Are there convenience functions/methods to do so? Any particular caveats, for example being culture-dependent?
- **Convert a DateTime to a string**: how can a DateTime be converted to a [string][type-string] representation? Are there convenience functions/methods to do so?

## Exercises

### Appointment scheduler

This exercise handles scheduled appointments. The reference implementation (C#) teaches:

- Creating a DateTime for a fixed date.
- Parsing a DateTime from a string
- Getting the current DateTime
- Comparing a DateTime to another DateTime
- Getting the time from a DateTime
- Converting a DateTime to a string

#### Implementations

| Track | Exercise                         | Changes |
| ----- | -------------------------------- | ------- |
| C#    | [strings][implementation-csharp] | None    |
| F#    | [strings][implementation-fsharp] | None    |

[type-date]: ./date.md
[type-duration]: ./duration.md
[type-time]: ./time.md
[type-string]: ./string.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/datetimes/.docs/introduction.md
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/beauty-salon/.docs/introduction.md
