C++ is a statically-typed language, which means that everything has a type at compile-time. Assigning a value to a name is referred to as defining a variable. A variable can be defined either by explicitly specifying its type, or by letting the C++ compiler infer its type based on the assigned value (known as _type inference_). Therefore, the following two variable definitions are equivalent:

```cpp
int explicitVar = 10; // Explicitly typed
auto implicitVar = 10; // Implicitly typed
```

Updating a variable's value is done through the `=` operator. Once defined, a variable's type can never change.

```cpp
auto count = 1; // Assign initial value
count = 2;     // Update to new value
```

C++ is a multi paradigm language language and functions can be defined in a _class_ or outside. The `class` keyword is used to define a class.

```cpp
class Calculator {
    // ...
};
```

A function within a class is refered to as a _method_. Each method can have zero or more parameters. All parameters must be explicitly typed, there is no type inference for parameters. Similarly the return type must also be made explicit. There are exceptions to the previous two rules but we'll talk about them in the future. Values are returned from functions using the `return` keyword. To allow a method to be called by code in other files, the `public` access modifier must be added.

```cpp
class Calculator {
public:
  int add(int x, int y) {
    return x + y;
  }
};
```

Invoking a method is done by specifying the name of an instance of that class, called an object, the method name and passing arguments for each of the method's parameters.

```cpp
Calculator calc;
auto sum = calc.add(1, 2);
```

Scope in C++ is defined between the `{` and `}` characters.

C++ supports two types of comments. Single line comments are preceded by `//` and multiline comments are inserted between `/*` and `*/`.
