# `var`
The `var` keyword declares and initializes a new variable. It can be optionally defined to a value.
```javascript
var arrow;
arrow = "->";

// or

var arrow = "->";
```

The scope of variables declared using the `var` keyword is either global (if it was not declared in a function) or local to the entire function it was declared in. The main difference between `var` and [`let`](https://github.com/exercism/v3/blob/master/languages/javascript/keywords/let.md) is that the scope of `var` is the entire function, whereas the scope of let is the block it was declared in as well as any contained sub-blocks.
```javascript
function bob() {
  console.log(name); // Variable name is undefined
  {
    let name = "Bob";
  }
}
```

```javascript
function steve() {
  console.log(name); // Variable name is defined, as "Steve"
  {
    var name = "Steve";
  }
}
```
