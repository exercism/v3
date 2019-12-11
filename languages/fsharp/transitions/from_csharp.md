# Learning F# with a C# background

## New types

- Record type (looks like an immutable struct, but is an immutable class with structural semantics)
- Discriminated unions (similar to Enum values, but not an exact match)
- Option type (similar to Nullable<T> values, but used much more in the F# core library)
- Result type (no equivalent in C#)
- Maps (looks like Dictionary<T>, but is a different type with other characteristics)
- Lists (looks like List<T>, but is a different type with other characteristics)
- Sets (looks like HashSet<T>, but is a different type with other characteristics)

## Already known types

- Arrays (same type as C# array)
- Tuples (only one version, not two as in C#; C# 6 introduced special tuple syntax)
- Unit type (similar to void)
- Sequences (same type as C# enumerable)

## Notes about learning types

- Doing something with F# types is usually done by passing the instance to a function. In C#, a method is called on the instance itself. That said, you can do some things through instance properties/methods, so it can be confusing when to use what.
- The record type is used where in C# class and struct types were used. Most C# programmers will probably consider it to be equivalent to a class, which is partly true, but the main difference is that an F# record has structural equality, whereas a C# class has reference equality.
- The record type is immutable, which is usually not the case with C# classes or structs. This means that “mutating” a record means returning another instance of the record (using special syntax). That said, it is not totally unusual for C# classes to be (partly) immutable, so it is not a completely foreign concept.
- Discriminated unions superficially look very similar to enums, so a C# developer might initially be tempted to just use enums. However, discriminated unions are significantly more powerful.
- Option types are superficially similar to nullable types, so a C# developer might initially be tempted to just use nullable types. However, option types are significantly more powerful, although with C# 8, there is less difference due to nullability of reference types being introduced.
- Maps, lists and sets all look like they are the same as their C# equivalent, but in fact they are not. This can trip up a C# programmer as the performance characteristics are different.
- Maps, lists and sets are immutable. Adding data to it means creating a new instance of that type. This goes against how the C# collections work, where you mutate an instance to add/update/remove data to it. C# programmers might also be concerned about the performance aspects of creating a new instance for each mutation (turns out to be not that much of an issue).
- Lists are the primary collection type, but data is usually prepended to the list (for performance reasons). In C#, lists usually add data to the end of the list, which is not very performant with F# lists.
- What is known as an Enumerable in C#, is known as a Sequence in F#. Thus, it is the same concept but with two different names.
- All built-in .NET framework types’ methods are called with tuples. This is usually not apparent to C# programmers, as F# tuples use a comma to separate their parts, whereas in C# a comma is used to separate parameters. This means that it looks like passing arguments to a built-in .NET framework type uses the same syntax in F# as it does in C#, but in fact it doesn’t. This can then trip somebody up when trying to use the same syntax to pass arguments to F# functions.
- The Option and Result types are both implemented as Discriminated unions, so it’s important to understand the latter first probably.
- The two core types (Records and Discriminated Unions) are quite expressive and concise; they have a very minimal syntax. Combining this minimal syntax and their expressiveness, F# developers introduce types far more often than C# developers. An example of this are “wrapper types,” that provide a small wrapper around primitive types.

## Teaching F# types

So how would one go about teaching F# types to a C# programmer? I think the core types to be taught first are:

- Records
- Discriminated unions
- Lists
- Tuples (perhaps skip this if the student is familiar with ValueTuple<T> in C#; don’t skip this if the student is only familiar with the “old” Tuple<T> type)

Once the student knows these basic types, learning the other types is probably relatively easy, as they build on the same foundations:

- Immutability
- Pattern matching
- Functions over methods

## New F# concepts

- These are concepts that, as far as I know, are either unique to F# or not shared with more than a few languages:
- Dependency order
- Computation expressions
- Type providers
- Active patterns
- Pipeline operator (related to pipeline-based transformations)

## New functional concepts

- Function composition
- Partial application
- Automatic generalisation (linked to type inference)
- Tail-recursion
- Scoping through indentation (significant whitespace)

## New other concepts

- Modules
- Many more characters allowed in names (using double backticks)
- Implicit returns

## Possibly new functional concepts

Some FP concepts may already be known, but have a slightly different or less extensive implementation:

- Immutability (can be done in C#, but F# makes it the default, which is the other way around for C#)
- REPL (available since C# 6, but rarely used in C# but far more in F#)
- Pattern matching (basic support in C# 7, more extensive support in C# 8)
- Pure functions (C# people will likely have written many pure (static) functions, but may not be aware of the concept itself)
- Higher-order functions (you can pass functions to other functions, through different means (delegates, Action<T>, Func<T> or even through reflection)
- Nested functions (available since C# 7, can be static sine C# 8)
- Type inference (available since C# 3, but only for variables. F# supports type inference for parameters and return types too)
- Expression-based (not statement based) (C# has always had expressions, but is slowly introducing expression-based variants of statement-based syntax, such as an expression-based switch and expression-bodied members)
- Recursion (not used often, but could be familiar)
- Enumeration (LINQ has been available since C# 3)
- Pipeline-based transformations (LINQ has been available since C# 3)

## Possibly new other concepts

- Ranges (available since C# 8)
- Slicing (available since C# 8, linked to ranges)

## Unlearn concepts

- Braces define scope (and whitespace is not significant)
- Everything is mutable by default
- Update state through mutation
- Functions are directly tied to a class, instead of functions taking a class instance as input
- Identifiers can only be defined using a (very) limited set of characters
- Using classes/structs as the primary data representation concept

## Syntax remapping

- Use whitespace for defining scope instead of curly braces
- Use whitespace to separate function arguments
- Omit types for function parameters and return type
- Omit semicolon for indicating end of expression/statement
- for-, while loops
- Defining classes
- Defining namespaces
- Chaining functions (pipeline operator)
- Equality, inequality and logical not operators (a C# programmer will get these wrong initially)
- Anonymous methods (lambda’s)

## General notes

- As seen, for some F# concepts there is a similar/equivalent C# counterpart. However, sometimes these C# counterparts were only introduced in later versions (e.g. pattern matching was introduced in C# 7). This means that it is very important to know which C# version a programmer is already familiar with, as some C# concepts might not be known to all C# programmers. This is why I’ve separated the concepts to learn in two categories: “New concepts” and “Possibly new concepts.”
- Many of the entries in the new concepts/unlearn concepts are their direct counterparts. For example, the new “Significant whitespace” concept is directly tied to unlearning the “Braces define scope” concept.

## Resources

- https://medium.com/@liman.rom/f-spoiled-me-or-why-i-dont-enjoy-c-anymore-39e025035a98
- https://www.guru99.com/c-sharp-dot-net-version-history.html
