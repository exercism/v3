In Go, a `struct` is a sequence of named elements called *fields*, each field having a name and type. The name of a field must be unique within the struct. `Structs` can be compared with the *class* in the Object Oriented Programming paradigm. 

You create a new struct by using the `struct` keyword and explicty define the name and type of the fields as shown un the example below. An `empty struct` is a struct with all fields set to their own zero values.

```go
type StructName struct{
    field1 fieldType1
    field2 fieldType2
}
```

To access the value of a field from a struct, we us the `.` operator. This way you can set or get values of a struct field.

```go
type Animal struct{
    name string
    age int
}

// create a new variable 
var dog Animal

// assign a values to each field by using the . operator
dog.name = "Rex"
dog.age = 10

// or you can assign the values to its fields individually
dog := Animal{
    name: "Bronson", 
    age: 7,
}
```

Fields that don't have a initial value assigned, will have their zero value.

To dive more deep into this type, you can check these resources: [Go by example: Structs], [A Tour of Go] or [Structures in Go (structs)]



[Go by example: Structs]: https://gobyexample.com/structs
[Structures in Go (structs)]:  https://medium.com/rungo/structures-in-go-76377cc106a2
[A Tour of Go]: https://tour.golang.org/moretypes/2

