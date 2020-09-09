# Nim Reference

## Concepts

(Taken in large part from [the manual](https://nim-lang.org/docs/manual.html))

### Types

#### General Type Related Concepts

- user types are capatilized
- static typing
- type inference
- generics
  - bind once
  - bind many
  - `is`
  - type class
  - implicit generic
  - compile time?
- `typedesc`
- `distinct`

#### Ordinal types

Ex: int (not uintX), bool enum, char (incld. distinct of ord type)

- countable & ordered
- can call `succ`, `pred`, `high`, `low`, `inc`, `dec` & `ord` on them
- can't count below or above lowest or highest elem

#### `int`

Ex: `int`, `int8`, `int16`, `int32`, `int64`

- `int` is defined by platform (and therefore affect `high` and `low`)
  - size of a pointer
- `intXX` is constructed as `'iXX`
- `uint` is unisigned
  - constructed with `'u` or `'uXX`
- operators
  - `+`, `-`, `/`, `*`, `div`, `mod

#### float

- `float` is `float 64`
- conform to IEEE standard
- exceptions for floats
- same as `int` for contstruction
- most same operators

#### Subrange

- range of `int` or `float`
- hold type of base type
- `range[(low end of range)..(high end of range)]
- as with oridinal type, static error or panic is raised

#### bool

- `true` or `false`
- one byte long
- `ord(false) == 0 and ord(true) == 1`
- operators
  - `not`, `and`, `or`, `xor`, `<`, `<=`, `>`, `>=`, `!=`, `==`

#### char

- one byte long
  - not UTF-\*
  - unicode module
- constructed with single quotes
- one charachter long

#### enum

- Declared with values and they are ordered as declaration
- Can be declared with or without enum type that they reside in
  - `Direction.north == north`
- assigned specific value
  - `type Direction = enum north = 1, east = 2, south = 3, west = 4`
  - ascending order
  - no explicit value means they are assigned previous + 1
  - not an ordinal type anymore
  - can have gaps
- stringification can be explicitly set
  - `type Direction = enum left = "<-", right = "->"
- stringification and ordinal value can both be set with a tuple with the oridnal value first
  - can be mixed and matched for different entries in a enum declaration
- `{.pure.}` (probably out of scope)

#### string

- similar to seq of char
  - accesing an charachter of a string is a `char` type
- zero terminated
- `len`
  - doesn't include terminating zero
- assignment copies string
- operations
  - `&` concats (can be used for chars on either side)
  - `&=` => concat and reassign to first var
- stringification
  - `$`
  - can be defined for custom types
  - used by echo implicitly so very useful for debugging
  - stringifying a string does nothing
- ordered lexicographically
- i-th element is the i-th char not unichar

#### cstring

- compatible string with the underlying backend
- `string` is implicitly converted to `cstring` when neccesary
  - not GC safe
- use `$` to convert to nim string

#### Structered types
