In Go, a `struct` is sequence of named elements called *fields*, each field having a name and type. The name of a field must be unique within the struct. `Structs` can be compared with the *class* in the Object Oriented Programming paradigm. 

You create a new struct by using the `struct` keyword, a ***built-in type*** and explicty define the name and type of the fields as shown un the example below. 


```go
type StructName struct{
    field1 fieldType1
    field2 fieldType2
}
```

Struct fields are accessed using a `.` notation.