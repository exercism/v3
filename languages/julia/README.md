# Julia

## Object-oriented concepts

- [Mutability](../../../reference/concepts/mutation.md)
- [Multiple Dispatch](../../../reference/concepts/multiple-dispatch.md)
- Constructors (external & internal)

## Functional concepts

- [Immutability](../../../reference/concepts/immutability.md)
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Nested functions](../../../reference/concepts/nested_functions.md)
- [Anonymous functions](../../../reference/concepts/anonymous_functions.md)
- [Type inference](../../../reference/concepts/type_inference.md)
- [REPL](../../../reference/concepts/repl.md)

## General concepts

- [Metaprogramming](../../../reference/concepts/metaprogramming.md)
  - [Macros](../../../reference/concepts/macros.md) (using/calling)
- Package Management
- Unicode identifiers
- Iterators
- Type Unions
- In-place modification
- Broadcasting
- Enumeration
- Type parameters
- Named arguments
- Optional arguments
- Operators are functions
- Error handling
- Ranges
- Benchmarking & Profiling
- Views
- Modules
  - `import` vs `using`
- Scopes
- Type stability
- Type piracy
- Language interoperability
- Debugging using a Debugger (non-stdlib)
- Macros (writing)
- Generated functions
- DataFrames (non-stdlib)
- Plotting (non-stdlib)
- Code inspection tools (`@code_llvm`, `@code_warntype`, `@code_native` etc)

## Types

- Abstract type
- Struct
- Tuple
- [Array](../../../reference/types/array.md)
  - 1-based indexing
  - Arbitrary indexing
  - Multidimensional arrays
- Dict
- Symbol
- `Expr`ession
- Nothing
- Missing
- Union
- [String](../../../reference/types/string.md)

## Resources used

- https://julialang.org/
- https://docs.julialang.org/en/v1/index.html
- Lauwens, B., & Downey, A. B. (2018). Think Julia: How to Think Like a Computer Scientist. Retrieved from https://benlauwens.github.io/ThinkJulia.jl/latest/book.html

## Addendum

### Why do people use Julia?

Knowing why people use Julia and what users (dis-)like about it can be helpful to determine what aspects of it should be taught, even if those aspects don't fit the definition of a concept.

The source for all information in this paragraph is the [Julia User & Developer Survey June 2019](https://julialang.org/blog/2019/08/2019-julia-survey). Aspects and features that aren't relevant to teaching/learning Julia have been omitted (e.g. an immature ecosystem).

#### Most popular technical features

1. Speed, performance
2. Ease of use
3. Open source code is available and can be modified
4. Multiple dispatch
5. Solves the two language problem<sup>1</sup>
6. Integrates well with other language(s)
7. 1-based indexing

#### Biggest technical problems

2. It takes too long to generate the first plot<sup>2</sup>
3. Slow compile times
4. 1-based indexing

#### If not for Julia, what language would users be using?

This can help in finding out which transitions are the most common.

1. Python
2. C, C++, C#<sup>3</sup>
3. MATLAB
4. R

Under 10% of respondents:

5. Bash/Shell
6. Fortran

Under 5% of respondents:

7. JavaScript
8. SQL
9. Scala

#### Footnotes

<sup>1</sup>The two language problem refers to the issue that performance-critical scientific or technical code is often written in a language like C/C++/Fortran but called from a higher-level but slower language like Python or MATLAB. Often this includes prototyping in Python/MATLAB and then translating code into a faster language.

<sup>2</sup>Also known as "time to first plot problem". Certain packages take a really long time to compile when first called. Subsequent calls are fast, therefore it's possible to adapt one's workflow to avoid this problem.

<sup>3</sup>Weird grouping, C# is probably far less common than the other two.
