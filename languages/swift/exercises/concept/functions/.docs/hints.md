## 1. Allow changing the expected oven time

- [Default parameter values][default-parameter-values] can be specified in the function's signature.
- The default value specification is placed after the parameter's type annotation.

## 2. Allow differing numbers of layers

- [Variadic parameters][variadic-parameters] can be specified in the function's signature.
- The variadic parameter notation is placed after the parameter's type name.
- The variadic parameter for a type _T_ will be accessible in the body of the function as an array with elements of type _T_.
  \_ You can get the number of elements in an array named `items` by accessing the count property. I.e. `items.count`.

## 3. Compute amounts of noodles and sauce

- [Multiple values can be returned from a function][multiple-return-values] by using a tuple to combine them into a single compound value.

## 4. Convert liters to fluid ounces in place

- [In-out parameters][in-out-parameters] can be specified by placing `inout` before the type name of the parameter.
- The new value needs to be assigned to the parameter before the function returns so that it can be copied into the variable that was passed in.

## 5. Determine the best wine to serve

- Functions can be [nested][nested-functions] inside another function by declaring a function, as normal inside of another function.
- Nested functions can be called as normal inside the functions the are defined within.
- Nested functions have access to all of the parameter, constants, and variables of the surrounding function. There is no need to pass them in to the nested function.

[functions]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html
[type annotations]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID312
[argument labels]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID166
[multiple-return-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID164
[implicit-returns]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID607
[default-parameter-values]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID169
[variadic-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID171
[in-out-parameters]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID173
[nested-functions]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID178
