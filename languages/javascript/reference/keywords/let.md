# `let`

The `let` keyword declares a block scope local [variable](https://developer.mozilla.org/en-US/docs/Glossary/Variable), optionally initializing it to a value.
```javascript
let arrow;
arrow = "->";

// or

let arrow = "->";
```

Variables declared using the `let` keyword have [scope](../info/scope.md) in the block in which they are defined, and any contained sub-blocks. The main difference between `let` and [`var`](var.md) is that the scope of `var` is the entire function in which it was declared.
```javascript
function bob() {
  {
    let name = "Bob";
  }
  console.log(name); // Variable name is undefined
}
```

```javascript
function steve() {
  {
    var name = "Steve";
  }
  console.log(name); // Variable name is defined, as "Steve"
}
```

The value held by a variable declared using `let` can only be referenced *after* it has been defined. This is known as the [Temporal Dead Zone](../info/scope#temporal-dead-zone).
