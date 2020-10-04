## Summary

Lists are a very common datatype in Common Lisp. They can be constructed via `list` or `cons` or even just quoting `'(a b c)`. Elements of a list can be accessed by index via `nth`, the helper functions `first` through `tenth`.

While lists can be though of as simply sequences of values they are also a recursive data structure in that each element of a list may itself be a list.

## Diving Deeper

### `car` and `cdr` concatenation

Back before more specialized data structures were added to the Lisp family of languages lists were used for everything. By creating abstractions (such as getter and setter functions) around list structures (and their nested values) one can uses lists to implement complex data structures. To simplify the accessing of data in nested lists the language can parse the use of "composed" `car` and `cdr` calls.

The language understands the arbitrary composition of up to 4 `car` or `cdr` calls. These functions have names which begin with `c` and end with `r` and between which are up to 4 `a` or `d` characters. The language interprets those functions as if they were a functional composition of the respective `car` or `cdr` calls. For example:

```lisp
(caar '((a b) (c d))) ; => A
(cdar '((a b) (c d))) ; => B
(cadr '((a b) (c d))) ; => (C D)
(caadr '((a b) (c d))) ; => C
(cdadr '((a b) (c d))) ; => (D)
(cadadr '((a b) (c d))) ; => D
```

## Reference

[Tutorialspoint][tutorialspoint] has a good summary of list functions and [Practical Common Lisp][pcl] is a good reference with more details.

The [Hyperspec][hyperspec] is the ultimate reference to the language. Its section on [conses][hyper-conses] describes all functions which can be used on lists. Its section on [sequences][hyper-sequences] describes those functions which can be used on sequences (which lists are).

[hyper-conses]: http://www.lispworks.com/documentation/HyperSpec/Body/14_.htm
[hyper-seqs]: http://www.lispworks.com/documentation/HyperSpec/Body/17_.htm
[hyperspec]: http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
[pcl]: http://www.gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html
[tutorialspoint]: https://www.tutorialspoint.com/lisp/lisp_lists.htm
