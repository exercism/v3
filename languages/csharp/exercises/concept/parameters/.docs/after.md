The coding exercise illustrates a number of properties of parameters:

- Parameters [passed][passing-parameters] without a modifier (such as `out` or `ref`) are passed by value. That is to say that the parameter as seen in the calling method cannot be changed by the called method.
- When a reference to an array or an instance of a class is a parameter the elements/fields of that instance can be changed by the called method and the changes seen by the caller. This gives the appearance of passing by reference but it is not. This confusion (albeit as it affects Java) is discussed in this [Stack Overflow thread][so-java-parameters]. The parameter (variable containing a reference to an object) itself is passed by value.
- A parameter with the [`out`][out-parameter] modifier conveys a value back from the called method to the caller. The parameter can be passed to the called method without being initialized and in any case it is treated within the called method as if, on entry, it had not been initialized. The behavior and rules regarding parameter modifiers are most easily understood through examples (see below) and compiler messages.

```csharp
void Foo(out int val)
{
    val = 42;
}
void Bar(int val)
{
    val = 42;
}

int importantValue = 1729;
Foo(out importantValue);
var result = importantValue == 42;
// => true

importantValue = 1729;
Foo(inportantValue);
result = importantValue == 1729;
// => true
```

- `out` parameters must be assigned to within a called method.

- A parameter with the [`ref`][ref-parameter] modifier passes a value into a called method. When the method returns the caller will find any updates made by the called method in that parameter.

```csharp
void Foo(out int val)
{
    val *= 7;
}

int importantValue = 6;
Foo(out importantValue);
return importantValue;
// => 42

```

`ref` parameters must be variables as the called method will be operating directly on the parameter as seen by the caller.

- The `out` and `ref` modifiers are required in the called method signature and at the call site.
- `out` parameters can be declared inline at the call site viz: `Foo(out int importantValue)`
- If you make a call to a method which has `out` parameters but you are not interested in the value assigned to one or more of them then you can use the [discard][discard] dummy variable `_`, as in: `Foo(out int _);`

All the rules regarding `ref` and `out` parameters are enforced by the compiler.

[`in`][in-parameter] parameters also exist but they are principally a performance optimisation and are discussed in the `structs` exercise where they are most relevant.

The documentation linked to above is summarised below:

- [passing-parameters][passing-parameters]: explains how values can be passed as arguments. Note that the subject of structs+parameters is discussed in the `structs` exercise.
- [ref-parameter][ref-parameter]: describes how `ref` parameters work.
- [out-parameter][out-parameter]: describes how `out` parameters work.
- [in-parameter][in-parameter]: describes how `in` parameters work.

When studying the documentation note that the documentation uses the following terms:

- parameter / formal parameter: refers to the parameter as seen by the called method.
- argument: refers to the parameter as seen by the caller.

Much of the value of `out` parameters has been eliminated by the introduction of `tuples` in C# 7. The `tuples` exercise shows `tuples` being used to return multiple values in the same way as `out` parameters.

Whilst `ref` is easy to use and has no performance penalties it is worth seeing how the problems it addresses are dealt with in a particular code base before using it widely. There are alternatives such as passing in by-value and using the value from the called method's return statement. Again, `tuples` can play a role.

You will see from the [documentation][ref-parameter] that `out` and `ref` cannot be used in certain situations but you can ignore them for now. The compiler will let you know whwn such situations arise.

Note that `optional parameters` and `named arguments` are discussed in the `method-overloading` exercise.

The related topics of [`ref local`][ref-local] and [`ref return`] are discussed elsewhere.

### Stack Allocations

The rules regarding parameters and their modifiers are sufficiently straightforward that you can take them at face value and understand them at their level of abstraction. However, if you are interested in the underlying mechanisms and why these keywords may make a performance difference at least in the case of `struct`s then you could start with this [_Wikipedia article_][calling-conventions], noting that C# uses `stdcall` on x86/64.

The essence of the story is that in the case of unmodified parameters the **value** of a variable is copied onto the program's stack to make it available to called routine whereas for `ref`, `out` and `in` parameters a pointer (**reference**) to the memory containing the value is placed on the program's stack and values are assigned to the memory pointed at by the reference.

[passing-parameters]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/passing-parameters
[ref-parameter]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref#passing-an-argument-by-reference
[in-parameter]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/in-parameter-modifier
[out-parameter]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/out-parameter-modifier
[discard]: https://docs.microsoft.com/en-us/dotnet/csharp/discards
[calling-conventions]: https://en.wikipedia.org/wiki/X86_calling_conventions
[so-java-parameters]: https://stackoverflow.com/questions/40480/is-java-pass-by-reference-or-pass-by-value
[ref-local]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref#ref-locals
[ref-return]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/ref#reference-return-values
