# `function`

The `function` keyword can be used to define a [function][concept-functions] inside an [expression][concept-expressions].

```dart
const greet = function(name) {
  return `Hello $name`;
};
```

You can also define functions using a function declaration.

## Function declaration

The function declaration (function statement) defines a function with the specified parameters.

```dart
function greet(name) {
  return `Hello $name`;
}
```

[concept-scope]: ../../../../reference/concepts/scope.md
[concept-expressions]: ../../../../reference/concepts/expressions.md
[concept-functions]: ../../../../reference/concepts/functions.md
