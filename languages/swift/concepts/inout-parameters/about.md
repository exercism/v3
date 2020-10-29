## In-out parameters

Within the body of a function, parameters are treated as constants, not variables; trying to modify the value of a parameter will raise a compile-time error. If a function wishes to modify the value of a parameter, it must use an [_in-out parameter_][in-out-parameters] to make this mutability explicit.

To use in-out parameters, a programmer must be sure of three things:

1. The function definition must explicitly mark in-out parameters by writing the keyword `inout` between the `:` and the type name in the parameter's type annotation.
2. The value passed in to the in-out parameter must be declared as a variable, not a constant. Similarly it cannot be a literal.
3. The variable name must be preceded by an `&` in the call to the function, explicitly marking it as modifiable.

When this is done, the function will make an internal copy of the in-out parameter, modify it as directed in the body of the function and then copy the modified value back into the variable that was passed in when the function returns.

```swift
func updateVersion(_ record: inout (version: Int, title: String)) {
  record.version += 1
}

var dbRecord = (version: 2, title: "Exercism")
updateVersion(&dbRecord)

// dbRecord is now (3, "Exercism")
```

There are a couple of extra rules one should be aware of regarding in-out parameters.

1.  Inside a function with in-out parameters, you are not allowed to reference the variable that was passed in as the in-out parameter.
2.  The same variable cannot be passed as multiple in-out parameters in the same function.

```swift
func inoutFunc(_ ioVar1: inout Int, _ ioVar2: inout Int) {
  ioVar1 += 1
  ioVar2 += 2
}

var mutVar = 0
inoutFunc(&mutVar, &mutVar)
// raises a compiler error: "Inout arguments are not allowed to alias each other"
```

[in-out-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID173
