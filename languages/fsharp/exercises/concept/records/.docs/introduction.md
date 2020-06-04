A record is a collection of fields (which can be of different types) that belong together. To define a record the `type` keyword is used. Record are created by specifying the record's field names and assigning them values between `{` and `}` characters. If each field is specified on a separate line,

A record instance's field values can be accessed using dot-notation. As records are immutable, once a record has been constructed, its field values can never change. If you'd like to change a record's values, the `with` keyword allows you to create a copy of an existing record, but with new values for one or more fields.

```fsharp
type Address =
    { Street: string
      HouseNumber: int
      PostalCode: string }

let oldAddress =
    { Street = "Main Street"
      HouseNumber = 17
      PostalCode = "12345 AB" }

let newAddress = { oldAddress with HouseNumber = 86 }

// Field is the same in both the old and new record
oldAddress.Street // => "Main Street"
newAddress.Street // => "Main Street"

// Field is changed in the new record
oldAddress.HouseNumber // => 17
newAddress.HouseNumber // => 86
```

Records have _structural equality_, which means that two instances of the same record with identical values are equivalent.

Besides being able to use dot-notation to access a record's fields, records can also be _deconstructed_ in bindings and in pattern matching:

```fsharp
type Address = { Street: string; HouseNumber: int; PostalCode: string }
let address = { Street = "Main Street"; HouseNumber = 17; PostalCode = "12345 AB" }

// Use deconstruction to access the HouseNumber and PostalCode fields
let { HouseNumber = houseNumber; PostalCode = postalCode } = address

let describe number =
    match number with
    | Integer i -> sprintf "Integer: %d" i
    | Double d  -> sprintf "Double: %d" i
    | Invalid   -> "Invalid"
```

As
