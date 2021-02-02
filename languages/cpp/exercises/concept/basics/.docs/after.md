C++ is a statically-typed language, which means that everything has a type at compile-time. Assigning a value to a name is referred to as defining a variable. A variable can be defined either by explicitly specifying its type, or by using the [`auto` keyword][auto] to have the C++ compiler infer its type based on the assigned value.

```cpp
int explicitVar = 10; // Explicitly typed
auto implicitVar = 10; // Implicitly typed
```

Updating a variable's value is done through the [`=` operator][assignment-operators]. Once defined, a variable's type can never change.

```cpp
auto count = 1; // Assign initial value
count = 2;     // Update to new value

// Compiler error when assigning different type
// count = false;
```

C++ is a multi paradigm language and functions can be defined in a _class_ or outside. The [`class` keyword][class] is used to define a class. A [function][function] within a class is refered to as a [_method_][methods]. Each function can have zero or more parameters. All parameters must be explicitly typed, there is no type inference for parameters. Similarly the return type must also be made explicit. There are exceptions to the previous two rules but we'll talk about them in the future. Values are returned from functions using the [`return` keyword][return]. To allow a method to be called by code in other files, the `public` access modifier must be added.

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

If the method to be called is defined in the same class as the method that calls it, the class name can be omited.

If a method does not use any class _state_ (which is the case in this exercise), the method can be made _static_ using the [`static` modifier][static].

```cpp
class Calculator {
public:
  static int multiply(int x, int y) {
    return x * y;
  }
};
```

A static method _must_ be called directly using the class name without creating an instance, that is done using the scope resolution operator `::`. The below snippets searches for a `multiply` _static_ method that accepts two `int`s as argument inside the `Calculator` _class_ scope. If the method is not declared _static_ the usual syntax with the `.` must be used to call it given an instance of the class that declares it, like we showed previously with the `add()` method.

```cpp
auto result = Calculator::multiply(10, 5);
```

[Scope][scope] in C++ is defined between the `{` and `}` characters.

C++ supports two types of [comments][comments]. Single line comments are preceded by `//` and multiline comments are inserted between `/*` and `*/`.

Integer values are defined as one or more (consecutive) digits and support the [default mathematical operators][operators].

[auto]: https://en.cppreference.com/w/cpp/language/auto
[assignment-operators]: https://en.cppreference.com/w/cpp/language/operator_assignment
[class]: https://en.cppreference.com/w/cpp/language/class
[function]: https://en.cppreference.com/w/cpp/language/functions
[methods]: https://en.cppreference.com/w/cpp/language/member_functions
[return]: https://en.cppreference.com/w/cpp/language/return
[static]: https://en.cppreference.com/w/cpp/language/static
[scope]: https://en.cppreference.com/w/cpp/language/scope
[comments]: https://en.cppreference.com/w/cpp/comment
[operators]: https://en.cppreference.com/w/cpp/language/operator_arithmetic
