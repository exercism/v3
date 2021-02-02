C++ is a statically-typed language, which means that everything has a type at compile-time. Creating a name is referred to as _declaring_ a variable, assigning a value to a declared variable is referred to as _defining_ a variable. Declaration and definition can be performed separately or as part of a single statement. Remember that a declared variable doesn't hold a valid value until defined explicitly but can still be used around the code.

```cpp
int myVar; // Declaration
myVar = 10; // Definition
int anotherVar = 20 // Declaration and definition
```

A variable can be defined either by explicitly specifying its type, or by letting the C++ compiler infer its type based on the assigned value (known as _type inference_). Therefore, the following two variable definitions are equivalent:

```cpp
int explicitVar = 10; // Explicitly typed
auto implicitVar = 10; // Implicitly typed
```

Updating a variable's value is done through the `=` assignment operator. Once defined, a variable's type can never change.

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
// calculator.h
class Calculator {
public:
  int add(int x, int y);
};
```

Usually C++ classes have their definition and implementation split into two separate files, header and source respectively, the first tells the compiler which methods and members the class has, the second what they do. The source file of our `Calculator` class would look like the following snippet.

```cpp
// calculator.cpp
#include "calculator.h"

int Calculator::add(int x, int y) {
  return x + y;
}
```

The `#include "calculator.h"` tells the compiler that we're writing the implementation of the `Calculator` class defined inside the `calculator.h` file. By convention header and source files are named after the class they define and implent, in this case we have `calculator.h` as our header and `calculator.cpp` as our source.

Invoking a method is done by specifying the name of an instance of that class, called an object, the method name and passing arguments for each of the method's parameters.

```cpp
Calculator calc;
auto sum = calc.add(1, 2);
```

Scope in C++ is defined between the `{` and `}` characters.

C++ supports two types of comments. Single line comments are preceded by `//` and multiline comments are inserted between `/*` and `*/`.
