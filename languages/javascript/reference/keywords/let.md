# `let`
The `let` keyword declares and initializes a new [variable](https://developer.mozilla.org/en-US/docs/Glossary/Variable). It can be optionally defined to a value.
```javascript
let arrow;
arrow = "->";

// or

let arrow = "->";
```

Variables declared using the `let` keyword have [scope](https://developer.mozilla.org/en-US/docs/Glossary/Scope) in the block in which they are defined, and any contained sub-blocks. The main difference between `let` and [`var`](var.md) is that the scope of `var` is the entire function in which it was declared.
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
