## 1. Refresh Lenny on symbols

If you are having trouble returning a symbol from a function, you might need to
take a look at [quoting][so-quoting]! Recall that keywords are slightly
different from regular symbols, and [must be preceded by a different
character][clhs-keywordp].

## 2. Help Lenny distinguish between atoms and conses

Common Lisp contains predicate functions for determining many things. In
particular there are two that are very relevant [here][clhs-conses]. Look for
functions with "atom" or "cons" in the name.

Note: in general predicate functions end with `p` or `-p`. The naming of the
atom predicate is an unfortunate inconsistency.

## 3. Help Lenny split up his conses

When it comes to getting the first element and the rest of an sexpr, there are a
couple of options [here][clhs-conses]. Look specifically for _Accessors_ as
opposed to functions. A couple options might have more intuitive names than
you'd expect!

[so-quoting]: https://stackoverflow.com/questions/134887/when-to-use-or-quote-in-lisp
[clhs-keywordp]: http://clhs.lisp.se/Body/f_kwdp.htm#keywordp
[clhs-conses]: http://clhs.lisp.se/Body/c_conses.htm
