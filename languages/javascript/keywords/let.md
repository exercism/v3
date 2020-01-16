# `let`
The `let` keyword declares and initializes a new variable. It can be optionally defined to a value.
```javascript
let arrow;
arrow = "->";

// or

let arrow = "->";
```

Variables declared using the `let` keyword have scope in the block in which they are defined, and any contained sub-blocks. The main difference between `let` and [`var`](https://github.com/exercism/v3/blob/master/languages/javascript/keywords/var.md) is that the scope of `var` is the entire function.
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

# `let`
The `let` keyword declares and initializes a new variable. It can be optionally defined to a value.
```javascript
let arrow;
arrow = "->";

// or

let arrow = "->";
```

The scope of variables declared using the `let` keyword is the block it was declared in as well as any contained sub-blocks. The main difference between `let` and [`var`](https://github.com/exercism/v3/blob/master/languages/javascript/keywords/var.md) is that the scope of `let` is the block it was declared in as well as any contained sub-blocks, whereas the scope of `var` is locally in the entire function it was declared in.
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
