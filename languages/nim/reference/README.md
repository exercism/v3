# Nim Reference

## Concepts

(Taken in large part from [the manual](https://nim-lang.org/docs/manual.html))

### Types

#### General Type Related Concepts

- user types are capitalized
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

  - derived from base type
  - does not imply relation between base type and subtype
  - `distinctBase`
  - type conversion
  - retain ordinality

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
- `uint` is unsigned
  - constructed with `'u` or `'uXX`
- operators
  - `+`, `-`, `/`, `*`, `div`, `mod

#### float

- `float` is `float 64`
- conform to IEEE standard
- exceptions for floats
- same as `int` for construction
- most same operators

#### Subrange

- range of `int` or `float`
- hold type of base type
- `range[(low end of range)..(high end of range)]
- as with ordinal type, static error or panic is raised

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
- one character long

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
- stringification and ordinal value can both be set with a tuple with the ordinal value first
  - can be mixed and matched for different entries in a enum declaration
- `{.pure.}` (probably out of scope)

#### string

- similar to seq of char
  - accessing an character of a string is a `char` type
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
- `string` is implicitly converted to `cstring` when necessary
  - not GC safe
- use `$` to convert to nim string

#### Structured types

- `array`, `seq`, `varargs`, `openArray` are a homogeneous type (elems of same type
  - type is inferred by
- lower bound and higher bound with `low` & `high` respectively
- `len`
- element access, `x[i]` for the i-th element

#### `array`

- fixed length
- array constructor `[]`
- indexed by ordinal type
  - if it is `openArray` then indexed from `0..len(array)-1`
  - explicit indexing `[enum1: "A", enum2: "B", enum3: "C"]`
  - `succ(lastIndex)` if index is left out

#### `seq`

- simlar to array
- dynamic length
- indexed from `0..len(seq)-1`
- constructed with an `openArray` (either a variable or a constructor) and the sequence operator, `@`, or the `newSeq` proc
- `add`, `&`, `&=`, pop

#### `openArray`

- can refer to all forms of array like types (`string`, `seq`, `array`)
- `len`, `low`, `high`
- nested `openArray` is not Supported

#### `varargs`

- variable number of arguments that can be passed to a `proc` that are converted to an `array`
- last parameter in the procedure header
- type conversions
  - second parameter in type instantiation as a reference to a `proc`

#### `UncheckedArray`

- bounds aren't checked
- translated to C array of undetermined size
- the base type may not contain GC'ed memory (unverified)

#### `object` & `tuples`

- heterogeneous storage containers
- name fields of a type
- assignment operator `=` copies each component by default
- constructed with `()`

#### `tuples`

- fields are ordered
- "Tuples are meant for heterogeneous storage types with no overhead and few abstraction possibilities" from manual
  - order matter for construction
- tuples are equivalent if they have the same fields in the same order with identical names
- tuple types defined in 2 ways:
  - `type x = tuple[x: int, y: char]`
  - or

```nim
type
  Person = tuple
    name: string
    age: natural
```

- unnamed tuple are defined with `(type1, type2...)`
  - tuples with one unnamed field can be defined with a trailing comma `(type1)`

#### `object`

- inheritance
- information hiding `*`
- definition
- `of` (similar to Java's `instanceof`)
- construction requires names for the fields

##### Object variants

- "Object variants are tagged unions discriminated via a enumerated type used for runtime type flexibility, mirroring the concepts of sum types and algebraic data types (ADTs) as found in other languages" (from manual)
  - an object that is of a single type that has varying (also number) of fields discriminated via an `enum` type
- advantages
  - no casting is needed between variations
  - exceptions are raise for incorrect field access
- discriminated by a `case` statement
- the discriminating field has some rules:
  - assignment restricted such that a change can't change the `of` branch of the case statement
  - it's address can't be taken
- discriminator has to be statically (compile-time) known for object construction
  - 2 way to "get around" this:
    - `case` statement where the discriminating type is the "subject" of the `case` statement and the branch of constructing the object variant has values that are of a single `of` branch in the original varying `case` statement
    - use a range of the `enum` type where the possible values are all in one branch of the varying `case` statement `range[low..high](value)`

#### `set`

- mathematical set
- basetype an only be certain ordinals (or their equivalents)
  - `int8`-`int16`
  - `uint8`/`byte`-`uint16`
  - `char`
  - `enum`
- max size (for integers) is `0..MaxSetElements-1` which is currently always 2<sup>16</sup>
- **high performance bit vectors**
- errors for larger set basetypes
- `{}` constructor
  - empty or with elements or ranges
- empty set is compatible with any basetype
- `+`, `*`, `-`, `==`, `<=`, `in`, `notin`, `contains`, `card`, `incl`, `excl`
- bit fields

  - parse enums in to set which has a integer representation because each enum is a power of 2 etc.

#### `ref` & `ptr`

- ref is a GC'ed heap allocated safe traced pointer
- ptr is a manually alloc/dealloc pointer that is unsafe
- `ref/ptr T`
- `[]` for dereferencing
- `addr` for accessing the address
  - always untraced
- `.` for accessing field & `[]` for indexing `openArray` perform implicit dereferencing
  - experimental feature for dereferencing when using UFCS
- recursive tuples are invalid `type Tup = tuple[a: ref Tup]`
- recursive pointer types are invalid `type Refer = ref Refer
- type can be a `ref` to an anonymous object
  - useful when _only_ the ref is used
- `new` for `ref` instantiation
  - returns the type if the passed in type is a `ref` type, else `ref T`
- `alloc`, `dealloc`, `realloc`
- to free untraced memory that contains traced memory, `reset` has to be called first

##### `nil`

- `nil` is the default value for all `ptr` & `ref` types
- "Dereferencing nil is an unrecoverable fatal runtime error (and not a panic)" (from the manual)
- `not nil` can be used to annotate a type to ensure that `nil` can't be used for that value

#### Procedural type (`proc`)

- internally a pointer to a `proc`
- `nil` is allowed
- calling conventions have to be the same
- available calling conventions are:
  - nimcall
    - default convention used for a Nim proc. It is the same as fastcall, but only for C compilers that support fastcall.
  - closure
    - default calling convention for a procedural type that lacks any pragma annotations. It indicates that the procedure has a hidden implicit parameter (an environment). Proc vars that have the calling convention closure take up two machine words: One for the proc pointer and another one for the pointer to implicitly passed environment.
  - stdcall
    - This is the stdcall convention as specified by Microsoft. The generated C procedure is declared with the `__stdcall` keyword.
  - cdecl
    - The cdecl convention means that a procedure shall use the same convention as the C compiler. Under Windows the generated C procedure is declared with the `__cdecl` keyword.
  - safecall
    - This is the safecall convention as specified by Microsoft. The generated C procedure is declared with the `__safecall` keyword. The word safe refers to the fact that all hardware registers shall be pushed to the hardware stack.
  - inline
    - The inline convention means the the caller should not call the procedure, but inline its code directly. Note that Nim does not inline, but leaves this to the C compiler; it generates `__inline` procedures. This is only a hint for the compiler: it may completely ignore it and it may inline procedures that are not marked as inline.
  - fastcall
    - Fastcall means different things to different C compilers. One gets whatever the C `__fastcal`l means.
  - syscall
    - The syscall convention is the same as `__syscall` in C. It is used for interrupts.
  - noconv
  - The generated C code will not have any explicit calling convention and thus use the C compiler's default calling convention. This is needed because Nim's default calling convention for procedures is fastcall to improve speed.
- Most calling conventions exist **only** for the Windows 32-bit platform.

#### `auto`

- used for return types and parameters
  - for return types it infers the type from the proc `body`
  - for parameters, generics are created

#### Type Equality

- structural equivalence
- except for `object`, `enum`, `distinct`
- subtype relation
- explicit/implicit convertibility
