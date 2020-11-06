## records

A record is a collection of fields (which can be of different types) that belong together. To define a record the `type` keyword is used. A record's fields are defined between `{` and `}` characters, and each field has a name _and_ a type. To create a record, specify the names of the fields and assign a value to them between `{` and `}` characters. All fields must be assigned a value when creating a record. A record instance's field values can be accessed using dot-notation.

When defining/create a record, each field must either be on a separate line or separated by semicolons (`;`) when on a single line.

```fsharp
// Define a record
type Address =
    { Street: string
      HouseNumber: int }

// Create a record
let oldAddress: Address =
    { Street = "Main Street"
      HouseNumber = 17 }

// Single-line alternative
type ConciseAddress = { Street: string; HouseNumber: int }
let conciseAddress: ConciseAddress = { Street = "Main Street"; HouseNumber = 17 }
```

As records are immutable, once a record has been constructed, its field values can never change. If you'd like to change a record's values, the `with` keyword allows you to create a copy of an existing record, but with new values for one or more fields.

```fsharp
// Copy the old address but change the house number
let newAddress: Address = { oldAddress with HouseNumber = 86 }
newAddress.Street      // => "Main Street"
newAddress.HouseNumber // => 86
```

Records have _structural equality_, which means that two instances of the same record with identical values are equivalent.

Besides being able to use dot-notation to access a record's fields, records can also be _deconstructed_ in bindings and in pattern matching:

```fsharp
let myAddress: Address = { Street = "Broadway"; HouseNumber = 123 }
let { Street = myStreet; HouseNumber = myHouseNumber } = myAddress

match myAddress with
| { HouseNumber = 1 } -> printfn "First house"
| { HouseNumber = houseNumber; Street = street } -> printfn "House number %d on %s" houseNumber street
// => "House number 123 on Broadway
```
