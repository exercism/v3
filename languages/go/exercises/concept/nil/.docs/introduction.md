## Nil and Zero Values

In Go, uninitialized variables and their elements are given default values.

These default values are called the `zero values` for their type:

| Type       | Zero Value |
| ---------- | ---------- |
| bool       | false      |
| numeric    | 0          |
| string     | ""         |
| pointer    | nil        |
| func       | nil        |
| interface  | nil        |
| slice      | nil        |
| channel    | nil        |
| map        | nil        |

`Nil`, meaning zero, is the `zero value` for the more complex types in Go.
