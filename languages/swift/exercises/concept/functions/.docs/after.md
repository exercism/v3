Great job! Functions are at the heart of Swift programming, so understanding them is vital to becoming fluent in Swift.

## Key Takeaways from this Exercise

- [Functions][functions] in Swift are defined using the `func` keyword followed by parentheses enclosed list of parameter names where these names include an [optional argument label and a parameter name][argument labels] followed by a [type annotation][type annotations] and the body of the function enclosed in curly braces.
- [Multiple values can be returned][multiple-return-values] from Swift functions by creating and returning a tuple from the different values.
- In cases where the entire body of a function is a single expression, [the `return` keyword may be omitted][implicit-returns].
- [Default parameter values][default-parameter-values] can be supplied for any of a function's parameters by assigning a value to the parameter in the parameter list following the parameter's type annotation.
- When a default parameter value is specified, the caller of the function can omit that parameter when calling the function and the default value will be used instead.
- [Variadic parameters][variadic-parameters] in Swift allow zero or more values of the same type to be passed into a single parameter in a function. This is indicated by appending `...` to the type annotation of the parameter.
- The values passed in to a variadic parameter will be automatically grouped into an array with elements of the same type as the type of the variadic parameter.
- Swift only allows functions to have one variadic parameter.
- If a function has parameters that follow the variadic parameter in the definition, the first parameter following the variadic parameter is _required_ to have an argument label.
- Within the body of a function, parameters are treated as constants, not variables; trying to modify the value of a parameter will raise a compile-time error.
- If a function wishes to modify the value of a parameter, it must use an [_in-out parameter_][in-out-parameters] to make this mutability explicit.
- To use in-out parameters, a programmer must be sure of three things:

1.  The function definition must explicitly mark in-out parameters by writing the keyword `inout` between the `:` and the type name in the parameter's type annotation.
2.  The value passed in to the in-out parameter must be declared as a variable, not a constant. Similarly it cannot be a literal.
3.  The variable name must be preceded by an `&` in the call to the function, explicitly marking it as modifiable.

- There are a couple of extra rules one should be aware of regarding in-out parameters.

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

- Functions may be defined [inside of other functions][nested-functions].
- These nested functions are not visible outside the enclosing function.

[functions]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html
[type annotations]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID312
[argument labels]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID166
[multiple-return-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID164
[implicit-returns]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID607
[default-parameter-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID169
[variadic-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID171
[in-out-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID173
[nested-functions]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID178
