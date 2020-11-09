# Enum

## The Concept

Sometimes also referred to as an **Enumeration** or **Enumeration type**, an enum represents a fixed set of named constants. Its chief purpose is to provide a type-safe way of interacting with numeric constants, limiting the available values to a pre-defined set.

In some languages, an enum is nothing more than _syntactic sugar_ over using [integers][type-integer]. Other languages implement enums as actual types, with the ability to add behavior to an enum.

## What to cover

- **Language-specific implementation**: is it syntactic sugar over an integer type, or is there more to it?
- **Defining an enum**: how to define an enum.
- **Using an enum value**: how to use an enum value, e.g. by assigning it or comparing it to another enum value.
- **Converting from and to string values**: how to convert a string to an enum value and vice versa.

## Exercises

### Log Levels

This exercise extracts log level information from log lines. The reference implementation (C#) teaches:

- Defining an enum.
- Assigning values to an enum's members.
- Comparing enum values.
- Converting a [string][type-string] to an enum.
- Converting an enum to a [string][type-string].

#### Implementations

| Track | Exercise                                | Changes |
| ----- | --------------------------------------- | ------- |
| C#    | [logs-logs-logs][implementation-csharp] | None    |

### Permissions

This exercise works with user account permissions. The reference implementation (C#) teaches:

- Defining as enum as a _flags_ enum, that is a single enum instance can represent multiple values being set.
- Setting and unsetting flags using bitwise operators.
- Checking for a flag being set on an enum instance.

#### Implementations

| Track | Exercise                                               | Changes |
| ----- | ------------------------------------------------------ | ------- |
| C#    | [attack-of-the-trolls][implementation-csharp-advanced] | None    |

[type-integer]: ./integer.md
[type-string]: ./string.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/logs-logs-logs/.docs/introduction.md
[implementation-csharp-advanced]: ../../languages/csharp/exercises/concept/attack-of-the-trolls/.docs/introduction.md
