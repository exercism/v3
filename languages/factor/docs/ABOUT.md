Factor was originally designed by Slava Pestov in 2002, as a scripting language implemented on the JVM for game engines. The platform and ecosystem has come a long way since that time, as has the core of the language itself. Factor's standard library has grown enormous, and its implementation, including its self-hosting native code optimising compiler, is now written almost entirely in Factor.

Factor can create standalone, and even GUI applications that behave exactly the same on Linux, Windows and Mac OS.

Factor is a simple, yet powerful and expressive stack-oriented high-level language in the vein of Forth, Joy and Lisp. It proposes a concatenative (point-free and compositional) model of data flow, alongside extreme extensibility and a [CLOS](http://enwp.org/Common_Lisp_Object_System)-derived object system.

Homoiconicity is a large part of Factor's programming model. All values, including functions and blocks of Factor code, go on the same stack and can be manipulated in the same way. This is a simple, yet powerful paradigm that invites interesting solutions to problems, and indefinite extensibility.

Factor requires from the reader a mindset apart from C or Python. Because of its compositional programming model, each function's output is used as input to the next one. Functions can use other functions or blocks of literal Factor code as input. In this way, it's rather like a (one-directional) Unix pipeline, but far more advanced while being less complicated. It takes some getting used to, but interactive development and re-**Factor**ing are encouraged, and becomes quite fun and interesting to use.

The Factor programming language is open source, and you can find it in active development on [GitHub](https://github.com/factor/factor).

If you're even a little bit new to Factor, it's highly recommended you read the [Factor cookbook](http://docs.factorcode.org/content/article-cookbook.html) and [Your first program](http://docs.factorcode.org/content/article-first-program.html) sections of the Factor documentation before continuing.

#### Glossary of Factor terms

* **Word** Essentially a function, which takes its arguments from, and returns values to, the **stack**. All words must have a declared stack effect. All elements of Factor's syntax are **words**, which makes defining new syntax very easy.
* **Vocabulary** A collection of **words** organised in a directory or source-file, like a module or library in other languages.
* **Stack** A last-in-first-out list of references to dynamically-typed values used for all operations; the primary way of passing data between functions. It is an implementation detail; Factor could be mostly implemented using term rewriting.
* **Program** A series of **words** that manipulate the **stack** at runtime. This gives the language a powerful foundation which allows many abstractions and paradigms to be built on top.