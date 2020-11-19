# Event Ingest

## Story

In this story, you have a program which needs to ingest events, but messages are using 3 different date formats:

- `"01/01/1970"`
- `"January 1, 1970"`
- `"Thursday, January 1, 1970"`

Job is to write regex to match and capture from each type.

Touches on:

- matching literal characters
- quantifiers
- alternations
- character classes
- anchors

## Tasks

- Create patterns to match small components
- Build on and create patterns which capture the small components
- Build on and create patterns which capture from supplied date formats
- Build on and create regex which matches and captures only that date format

## Implementations

- [Elixir: regular-expressions][implementation-elixir] (reference implementation)

[implementation-elixir]: ../../languages/elixir/exercises/concept/date-parser/.docs/instructions.md
