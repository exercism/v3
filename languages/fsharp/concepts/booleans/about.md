Booleans in F# are represented by the `bool` type, which values can be either `true` or `false`.

F# supports three [boolean operators][operators]: `not` (NOT), `&&` (AND), and `||` (OR). The `&&` and `||` operators use _short-circuit evaluation_, which means that the right-hand side of the operator is only evaluated when needed.

```fsharp
true || false // => true
true && false // => false
```

The three boolean operators each have a different [_operator precedence_][precedence]. As a consequence, they are evaluated in this order: `not` first, `&&` second, and finally `||`. If you want to 'escape' these rules, you can enclose a boolean expression in parentheses (`()`), as the parentheses have an even higher operator precedence.

```fsharp
not true && false   // => false
not (true && false) // => true
```

[operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/boolean-operators
[precedence]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#operator-precedence
