[Lists][hyper-cons-as-list] are a very common data type in Common Lisp. They are made up of a sequence of [cons][../cons/about.md] cells. Each `car` is an element of the list and every `cdr` is a either the next cons cell or a terminating atom.

A list which terminates with the empty list is called a "proper list".

A list which terminates with an atom that is not th empty list is called a "dotted list" (based upon how it is printed: `(cons 'a 'b) ;=> (a . b)`).

A list can also be circular if some cons cell in the list `cdr` of a later cons cell.

[hyper-cons-as-list]: http://l1sp.org/cl/14.1.2
