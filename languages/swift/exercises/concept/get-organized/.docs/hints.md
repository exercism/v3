## General

- To define a [generic function][generic-functions], one needs to use a [type parameter][generic-type-parameters] as a stand-in for a concrete type.
- The type parameters must be listed between angle brackets between the function name and its parameters.

## 1. Swap two elements in any array

- Since the modified array is being returned via the `inout` parameter, the function itself does not need to return a value.

## 2. Bubble sort any array

- Values passed into functions as `inout` parameters _must_ be defined as a _var_.
- Values passed into functions as `inout` parameters _must_ be prefixed with an `&`.

[generic-functions]: https://docs.swift.org/swift-book/LanguageGuide/Generics.html#ID181
[generic-type-parameters]: https://docs.swift.org/swift-book/LanguageGuide/Generics.html#ID182
