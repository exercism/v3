Exercism provides exercises and feedback but can be difficult to jump into for those learning OCaml for the first time. These resources can help you get started:

* [Documentation for the Standard Library](http://caml.inria.fr/pub/docs/manual-ocaml/libref/index.html)
* [OCaml at JaneStreet](https://ocaml.janestreet.com/)
* [Documentation for the Core Library](https://ocaml.janestreet.com/ocaml-core/latest/doc/core/index.html)
* [Caml programming guidelines](http://caml.inria.fr/resources/doc/guides/guidelines.en.html)

OCaml's documentation is spread over multiple projects and can be hard to find because there is what is sometimes called the standard library (the rather minimal library that comes with the compiler) and a Core library (a separate project by Jane Street that aims to provide a more complete and consistent standard library).

Confusingly the standard library is sometimes referred to as the core library (though rarely as the Core library).

The Core library from Jane Street is required for a few exercises, but can be useful for all. It's divided into three - Base, Core_kernel, and Core - each extending the previous. 
Base is all you'll need for most exercises, but for a few (using dates for instance, Core_kernel is needed).
To use Core, put `open Base` or `open Core_kernel` at the top of your code.

For example, instead of [`List`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html) you get a shadow from `Base`.(https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base/List/) (`open Base` makes this available under the name `List`).

There are some subtle and not so subtle differences between the standard library `List` module and `Core`'s `List`, for example in the standard library `List.for_all` has signature

```ocaml
val for_all : ('a -> bool) -> 'a list -> bool
```

whereas in the Core library it has

```ocaml
val for_all : 'a t -> f:('a -> bool) -> bool
```

The consequence is that to check if all numbers in a list are lower than 10 you have to write the following with the compiler core List module:

```ocaml
List.for_all (fun x -> x < 10) list
```

and with the Core List module you need to write:

```ocaml
List.for_all list (fun x -> x < 10)
```

or (preferably):

```ocaml
List.for_all ~f:(fun x -> x < 10) list
```

A piece of advice: focus on the Core library and ignore the standard library unless you really can't find what you need in Core.
