# `function`

The `function` keyword can be used to define a [function][concept-functions] inside an [expression][concept-expressions].

```dart
const greet = function(name) {
  return `Hello $name`;
};
```

You can also define functions using the `Function` constructor and a function declaration.

## Function declaration

The function declaration (function statement) defines a function with the specified parameters.

```dart
function greet(name) {
  return `Hello $name`;
}
```

## `Function` constructor

The `Function` constructor creates a new Function **object**. Calling the constructor directly can create functions dynamically, but suffers from security and similar (but far less significant) performance issues to eval. However, unlike `eval`, the Function constructor creates functions which execute in the [global scope][concept-scope] only.

```dart
const greet = new Function("name", "return `Hello $name`");
```

[concept-scope]: ../../../concepts/scope.md
[concept-expressions]: ../../../concepts/expressions.md
[concept-functions]: ../../../concepts/functions.md
