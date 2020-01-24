# `var`

The `var` keyword declares a [variable][concept-variable], optionally initializing it to a value.
```javascript
var arrow;
arrow = "->";

// or

var arrow = "->";
```

The [scope][concept-scope] of variables declared using the `var` keyword is either [global][concept-scope#global-scope] (if it was not declared in a [function][keyword-function]) or local to the entire function it was declared in. The main difference between `var` and [`let`][keyword-let] is that the scope of `var` is the entire function, whereas the scope of `let` is the block it was declared in as well as any contained sub-blocks.
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

[keyword-function]: ./function.md
[keyword-let]: ./let.md
[concept-scope]: ../info/scope.md
[concept-variable]: ../../../../reference/concepts/variables.md
