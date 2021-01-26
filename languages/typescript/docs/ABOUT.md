[TypeScript](http://www.typescriptlang.org/) (TS) is a superset of JavaScript (JS), created at Microsoft
in response to frustration developing large-scale applictions in JS. In a large JS project, knowing
what properties your own objects have, what arguments your functions take (and what type they need to be)
can become difficult. Similarly, since there is no ability to intelligently inspect JS code, when you include
a package (like from `npm`), you have to keep the documentation up so you know what methods are available and
what arguments they take. TS solves these issues. It is currently an open-source project hosted on Github.
It supports tools for any browser as well as Node, for any host, on any OS. TTS compiles to readable,
standards-based JavaScript.

TS adds a flexible type system to JS, in addition to interfaces (custom types) and modifying the syntax of
some ECMAScript features such as classes. Types are optional and flexible (for example, you can specify an
argument is a string OR a number). Types allow tooling available in most code editors that improve the
development experience such as code completion and method detection, both in your own code and in packages
you use. It supports many upcoming ECMASCript features (such as async/await). TS can be written in OO or functional
styles. It is compatible with all existing JS packages. TS transpiles to clean, readable JS.

Try it out at the [playground](http://www.typescriptlang.org/Playground), and stay up to date via [the Typescript blog](https://blogs.msdn.microsoft.com/typescript) and [Twitter account](https://twitter.com/typescriptlang).
