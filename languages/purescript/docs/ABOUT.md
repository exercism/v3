PureScript is a purely functional, statically-typed programming language with global type inference.

**Functional** means that functions are first-class values.
Functions are an important and powerful tool for abstraction.
In PureScript, computation is modeled as the evaluation of expressions made up of function applications, rather than as the execution of a sequence of instructions.

**Purely Functional** means that it is possible to tell if a computation has side-effects or not, meaning that if a computation changes a global variable, reads from input, writes to a socket, etc. it will be reflected in the type of the computation.

**Statically-typed** means that all computations have a type that is known before running the program.

**Global type inference** means that the compiler can figure out the type of computations in the entire program without needing the programmer to specify the type.
