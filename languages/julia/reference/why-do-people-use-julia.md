## Why do people use Julia?

Knowing why people use Julia and what users (dis-)like about it can be helpful to determine what aspects of it should be taught, even if those aspects don't fit the definition of a concept.

The source for all information in this paragraph is the [Julia User & Developer Survey June 2019](https://julialang.org/blog/2019/08/2019-julia-survey). Aspects and features that aren't relevant to teaching/learning Julia have been omitted (e.g. an immature ecosystem).

### Most popular technical features

<!-- prettier-ignore -->
1. Speed, performance
2. Ease of use
3. Open source code is available and can be modified
4. Multiple dispatch
5. Solves the two language problem<sup>1</sup>
7. Integrates well with other language(s)
9. 1-based indexing

### Biggest technical problems

<!-- prettier-ignore -->
2. It takes too long to generate the first plot<sup>2</sup>
4. Slow compile times
12. 1-based indexing

### If not for Julia, what language would users be using?

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

### Footnotes

<sup>1</sup>The two language problem refers to the issue that performance-critical scientific or technical code is often written in a language like C/C++/Fortran but called from a higher-level but slower language like Python or MATLAB. Often this includes prototyping in Python/MATLAB and then translating code into a faster language.

<sup>2</sup>Also known as "time to first plot problem". Certain packages take a really long time to compile when first called. Subsequent calls are fast, therefore it's possible to adapt one's workflow to avoid this problem.

<sup>3</sup>Weird grouping, C# is probably far less common than the other two.
