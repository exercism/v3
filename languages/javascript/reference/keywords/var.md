# `var`

The `var` keyword declares a [variable](https://developer.mozilla.org/en-US/docs/Glossary/Variable), optionally initializing it to a value.
```javascript
var arrow;
arrow = "->";

// or

var arrow = "->";
```

The [scope](../info/scope.md) of [variables](https://developer.mozilla.org/en-US/docs/Glossary/Variable) declared using the `var` keyword is either [global](../info/scope.md#global-scope) (if it was not declared in a [function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions)) or local to the entire function it was declared in. The main difference between `var` and [`let`](let.md) is that the scope of `var` is the entire function, whereas the scope of `let` is the block it was declared in as well as any contained sub-blocks.
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
