# Web Design Agency

## Story

Your design company has primarily been working with CSS transformations to build webpages. After some discussion, a decision is made
to start using JavaScript to perform some of the calculations dynamically. Some of your teammates are less experienced with JavaScript,
so you decide to use a function closure to create reusable transformation for `{x, y}` coordinate pairs.

## Tasks

- Create a function whose arguments are used as a closure in a returned function.

  - A function for a 2d translation, where the outer function specifies the amount to translate, and the returned function accepts an x, y coordinate, and returns the translated coordinates.
  - A function for a 2d scaling, where the outer function specifies the amount to scale, and the returned function accepts an x, y coordinate, and returns the translated coordinates.
  - A function that returns a function that composes two functions, effectively a function for `g(f(x))`

- Create a function which memoizes a function.

## Implementations

- [JavaScript: closures][implementation-javascript] (reference implementation)

[implementation-javascript]: ../../languages/javascript/exercises/concept/closures/.docs/instructions.md
