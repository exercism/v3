# Learning JavaScript with a [CoffeeScript][language-coffeescript] background

[CoffeeScript][language-coffeescript] is a programming language that compiles to JavaScript. It adds syntactic sugar inspired by [Ruby][language-ruby], [Python][language-python] and [Haskell][language-haskell] in an effort to enhance JavaScript's brevity and readability. Specific additional features include [list comprehension][concept-list-comprehension] and [destructuring assignment][concept-destructuring-assignment].

In order to transition from [CoffeeScript][language-coffeescript] to JavaScript, the syntactic sugar needs to be _understood_ and its equivalents need to be studied.

## Language difference

### Expressions

`if`, `switch` and `for` statements in [CoffeeScript][language-coffeescript] return a value as they are considered an expression, but don't in JavaScript.

### Variables

[CoffeeScript][language-coffeescript] does not denote variables using [`var`][javascript-keyword-var], [`let`][javascript-keyword-let] or [`const`][javascript-keyword-const], JavaScript _requires_ one of the three (in [strict mode][javascript-concept-strict-mode]).

In [CoffeeScript][language-coffeescript], lexical scoping and variable safety is done by the compiler. This means that `var` is automatically injected at the "correct" scope. But because the keywords are not accessible, [variable shadowing][code-smell-variable-shadowing] is not possible, and this can lead to subtle differences when learning JavaScript, where this _is_ possible.

### Array Slicing and Splicing

The syntax for [destructuring][concept-destructuring] (`...`) can be used in [CoffeeScript][language-coffeescript] to slice and splice arrays, similar to how [Ruby][language-ruby] does it. In JavaScript, this syntax can not (yet) be used to accomplish the same.

### Splats / Spread / Rest syntax

The syntax for [rest parameters][concept-rest-parameters] (`...`) in [CoffeeScript][language-coffeescript] has been adopted in JavaScript since the ES2015 standard. The only difference is that in CoffeeScript the `...` comes _after_ the token to spread, and in JavaScript it comes _before_ the token to spread.

### Loops

[CoffeeScript][language-coffeescript] knows the keyword [`until`][coffeescript-keyword-until], which needs to be translated to a [`while`][javascript-keyword-while] or `do (...) while`.

Then there is [`when`][coffeescript-keyword-when] and [`by`][coffeescript-keyword-by], two keywords to create [list comprehensions][concept-list-comprehension], which don't exist in JavaScript.

### Future syntax in JavaScript

[CoffeeScript][language-coffeescript] has the `?` keyword, which is similar to the following proposal at moment of writing: [tc39/optional-chaining][tc39-optional-chaining]. Similarly, there is the [tc39/nullish-coalescing][tc39-nullish-coalescing] proposal. Both can be used when making use of a tool such as [Babel][tool-babel], but are not a part of the standard yet.

### Functions

In [CoffeeScript][language-coffeescript], the [`function`][javascript-keyword-function] has been replaced by the `->` symbol, which is _similar_ to the JavaScript's [Arrow Function Expressions][javascript-concept-arrow-function-expression], but uses `=>` instead.

## Tooling

JavaScript does not have to be compiled, whereas [CoffeeScript][language-coffeescript] has a compiler. There are similar tools to transpile certain future features of JavaScript to the current implemented version, such as [babel][tool-babel], but those tools are not part of the language.

<!-- external links -->

[tc39-nullish-coalescing]: https://github.com/tc39/proposal-nullish-coalescing
[tc39-optional-chaining]: https://github.com/tc39/proposal-optional-chaining

<!-- links to languages and concepts -->

[coffeescript-keyword-by]: ../../coffeescript/keywords/by.md
[coffeescript-keyword-until]: ../../coffeescript/keywords/until.md
[coffeescript-keyword-when]: ../../coffeescript/keywords/when.md
[concept-destructuring]: ../../../reference/concepts/destructuring.md
[concept-destructuring-assignment]: ../../../reference/concepts/destructuring_assignment.md
[concept-list-comprehension]: ../../../reference/concepts/list_comprehension.md
[concept-rest-parameters]: ../../../reference/concepts/rest_parameters.md
[concept-variable-shadowing]: ../../../reference/concepts/variable_shadowing.md
[javascript-concept-arrow-function-expression]: ../../../languages/javascript/info/arrow_function_expression.md
[javascript-concept-strict-mode]: ../../../languages/javascript/info/strict_mode.md
[javascript-keyword-const]: ../keywords/const.md
[javascript-keyword-function]: ../keywords/function.md
[javascript-keyword-let]: ../keywords/let.md
[javascript-keyword-var]: ../keywords/var.md
[javascript-keyword-while]: ../keywords/while.md
[language-coffeescript]: ../../coffeescript/README.md
[language-haskell]: ../../haskell/README.md
[language-python]: ../../python/README.md
[language-ruby]: ../../ruby/README.md
[tool-babel]: ../../../tooling/babel.md
