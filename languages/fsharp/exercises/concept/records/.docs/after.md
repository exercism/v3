A [record][records] is a collection of fields (which can be of different types) that belong together. To [define a record][define] the `type` keyword is used. A record's fields are defined between `{` and `}` characters, and each field has a name _and_ a type.

```fsharp
type Address =
    { Street: string
      HouseNumber: int }
```

To [create a record][create], specify the names of the fields and assign a value to them between `{` and `}` characters. All fields of a record must be assigned a value when creating it. The fields can be specified in any order, they don't have to match the ordering of the record type definition. To access a record instance's field values, dot-notation is used.

```fsharp
let oldAddress: Address =
    { Street = "Main Street"
      HouseNumber = 17 }

oldAddress.Street      // => "Main Street"
oldAddress.HouseNumber // 17
```

When defining/create a record, each field must either be on a separate line or separated by semicolons (`;`) when on a single line:

```fsharp
type Address = { Street: string; HouseNumber: int }

let oldAddress: Address = { Street = "Main Street"; HouseNumber = 17 }
```

If a record's field names are unique, type annotations are optional as the compiler will be able to infer the type automatically. If the combination of a field names is _not_ unique, you can help the compiler choose the correct record by either using an explicit type for the record binding or by prefixing one or more of the fields with the record type:

```fsharp
type Boat = { Weight: int; Length: int }
type Car = { Weight: int; Length: int }

// Let compiler infer type. As there are multiple matching record types,
// the last matching type is used (Car)
let typeEqualsLastMatch = { Weight = 5000; Length = 15 }

// Use type annotation on binding to force type to Boat
let typeAnnotation: Boat = { Weight = 5000; Length = 15 }

// Use fully qualified name for field to force type to Boat
let fullyQualifiedField = { Boat.Weight = 5000; Length = 15}
```

As records are immutable, once a record has been constructed, its field values can never change. If you'd like to change a record's values, the `with` keyword allows you to [create a copy of an existing record, but with new values for one or more fields][create].

```fsharp
// Copy the old address but change the house number
let newAddress = { oldAddress with HouseNumber = 86 }
newAddress.Street      // => "Main Street"
newAddress.HouseNumber // => 86
```

Records have [_structural equality_][equality], which means that two instances of the same record with identical values are equivalent:

```fsharp
let address1 = { Street = "Main Street"; HouseNumber = 17 }
let address2 = { Street = "Main Street"; HouseNumber = 17 }
let address3 = { Street = "Broadway"; HouseNumber = 17 }

address1 = address2 // => true
address1 = address3 // => false
```

Besides being able to use dot-notation to access a record's fields, records can also be _deconstructed_ in bindings and in pattern matching using the [record pattern][record-patterns]:

```fsharp
let myAddress = { Street = "Broadway"; HouseNumber = 123 }
let { Street = myStreet; HouseNumber = myHouseNumber } = myAddress

match myAddress with
| { HouseNumber = 1 } -> printfn "First house"
| { HouseNumber = houseNumber; Street = street } -> printfn "House number %d on %s" houseNumber street
// => "House number 123 on Broadway
```

[records]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records
[define]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records#remarks
[create]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records#creating-records-by-using-record-expressions
[equality]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records#differences-between-records-and-classes
[pattern-matching]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching
[record-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#record-pattern
[guards]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions#guards-on-patterns
