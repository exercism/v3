Coq is a programming language and a logic system at the same time, based on [Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).
In order to be a meaningful logic system, the language is designed so that any program written in Coq is guaranteed to terminate.
Due to this, Coq is seldom used for general purposes; instead, it allows to develop *theories in mathematics* and write *certified programs*.

Coq is also an interactive proof assistant.
It does not automatically solve theorems, but helps the user build proofs via tactics.
The tactic language (Ltac) is a language of its own, which allows to automate parts of the proofs.
A well-written proof script resembles an informal proof written in prose.

The main application / research areas using Coq include:

* Mathematics (number theory, set theory, logic theory, computability theory, algebra, geometry, ...)
* Programming languages (compilers, execution models, compiler optimizations, type system, ...)
* Certified algorithms (correctness and termination of algorithms) and extraction into a general-purpose language (usually Ocaml or Haskell)

Notable Coq developments include:

* Machine-checked proof of [the Four Color Problem](https://madiot.fr/coq100/#32)
* [CompCert](http://compcert.inria.fr/compcert-C.html), a certified C compiler

If you are interested in Coq but haven't learned it yet, it is often advised to start with [Software Foundations](https://softwarefoundations.cis.upenn.edu/) series.
Especially the first few chapters (up to "IndProp") will give you the fundamentals you need before you can start working on more interesting concepts and theories.
You might find [other resources](https://coq.inria.fr/documentation) interesting too.

Discussion about Coq and developments using Coq are usually done at [Reddit /r/coq](https://www.reddit.com/r/Coq/) and [Discourse](https://coq.discourse.group/latest).
If you have questions, you can also get help at [StackOverflow](https://stackoverflow.com/questions/tagged/coq?sort=newest&pageSize=50); don't forget to tag your question with "coq".