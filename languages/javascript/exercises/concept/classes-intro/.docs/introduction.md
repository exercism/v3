In general object-oriented programming, a class is an extensible template for storing associated values and methods.

In JavaScript, a class is a special type of function that can bind values and methods to itself using the `this` keyword.

Initialize classes by using the `class` keyword:

```js

class MyClass() {
    //...
}

```

To add a method (a function of a class) to a class, just write the function definition without the `function` keyword in the class body:

```js
class FunctionDemo {
  function1() {
    console.log('Hello classes!')
  }

  function2() {
    this.function1()

    console.log('Hello second function!')
  }
}
```

The constructor function is a special kind of function that is run upon class initialization. This is where you can initialize and store class properties:

```js
class MyConstructorClass {
  constructor(property1, property2) {
    this.property1 = property1
    console.log(property2)
  }
}
```

The `this` keyword is a way to access properties and methods on a class's _prototype_. The prototype is a JavaScript object where all properties and methods of a class are stored.

You can access the `this` keyword from anywhere inside any function:

```js
class ThisDemo {
  constructor(property1, property2) {
    // This line binds the value of the property1 argument to the property1 property on the class.
    this.property1 = property1
  }

  useProperty() {
    // We can use properties that are stored on the prototype anywhere
    console.log(this.property1)
  }

  myFunction() {
    //Because functions are normally (not always) stored on the prototype, you can also use functions declared elsewhere in the class!
    this.useProperty()
  }
}
```
