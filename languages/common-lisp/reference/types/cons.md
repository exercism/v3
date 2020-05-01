# Cons Cells

A [cons cell][cons] (also known as a dotted pair due its printed form) is a data structure consisting of a pair of two objects. Cons cells are created with the function `cons` and the items of a cons cell are accessed with `car` and `cdr`. (One can also use `first` and `rest` instead of `car` and `cdr`.)

(The names of the accessor methods are a [historical artifact][why-car-cdr]. The cons cells of the original Lisp implementation were implemented in registers of the IBM 704 computer and the operations used to access them were the `CAR` operation to access the "**C**ontents of the **A**ddress part of the **R**egister" and `CDR` operation to access the "**C**ontents of the **D**ecrement part of the **R**egister".)

The items in a cons cell can be of any type, including other cons cells. This is how they can be used to implement other data types such as lists and tree.

[cons]: https://riptutorial.com/common-lisp/example/8701/what-is-a-cons-cell-
[why-car-cdr]: https://en.wikipedia.org/wiki/CAR_and_CDR#Etymology
