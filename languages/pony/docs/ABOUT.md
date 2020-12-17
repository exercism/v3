[Pony](http://www.ponylang.org) is an object-oriented, actor-model, capabilities-secure programming language focused on geting stuff done.

It's object-oriented because it has classes and objects, like Python, Java, C++, and many other languages. It's actor-model because it has actors (similar to Erlang or Akka). These behave like objects, but they can also execute code asynchronously. Actors make Pony awesome.
When we say Pony is capabilities-secure, we mean a few things:

- It's type safe. Really type safe. There's a mathematical proof and everything.
- It's memory safe. Ok, this comes with type safe, but it's still interesting. There are no dangling pointers, no buffer overruns, heck, the language doesn't even have the concept of null!
- It's exception safe. There are no runtime exceptions. All exceptions have defined semantics, and they are always handled.
- It's data-race free. Pony doesn't have locks or atomic operations or anything like that. Instead, the type system ensures at compile time that your concurrent program can never have data races. So you can write highly concurrent code and never get it wrong.
- It's deadlock free. This one is easy, because Pony has no locks at all! So they definitely don't deadlock, because they don't exist.

Newcomers should start with the [tutorial](https://tutorial.ponylang.org/).
