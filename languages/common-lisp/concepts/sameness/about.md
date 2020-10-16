## Summary

In this exercise you've learned about the four generic equality predicates in Common Lisp: `eq`, `eql`, `equal`, `equalp`. You've learned how they build upon each other and go from a very strict definition of equality (`eq`) to a loose definition of equality (`equalp`).

## Diving Deeper

### The gotcha of `eq`:

There is one very important detail about `eq` that trips a lot of people up. Because the Common Lisp specification allows for an implementation to copy numbers and characters in memory if they choose to and because `eq` does a strict object identity check that means `(eq 42 42)` and `(eq #\x #\x)` may both evaluate to `NIL`! So while these may evaluate to `T` lots of times, it may fail when you least expect it and it will be very difficult to debug.

So if you know you might be checking equality of numbers and characters you should use `eql`. Even better if you know you are checking only characters or only numbers you should use `char=` and `=` respectively as they are type specific.

### Default equality tests

Some functions in Common Lisp may take a `test` keyword argument. For example `find`, `count`, `pushnew` or `assoc`. The language specifies that the default value for these is `eql`.

For example if you want to found how many times a specific string happens in a list of strings you could use `count` but this would not work:

`(count "foo" (list "foo" "bar" "baz" "foo")) ; => 0`

Since `(eql "foo" "foo") ; => NIL`.

In this case you need to provide a `test` argument such as `string=`:

`(count "foo" (list "foo" "bar" "baz" "foo") :test #'string=) ; => 2`

The language also has some conditional expressions (such as `case`) which are defined to use `eql` as their equality predicate, which may limit their utility in some circumstances.

## Reference

[Eli Bendersky's page][eli-lisp-equality] on Common Lisp Equality predicates is a good "plain language" reference. For the language reference on each of the functions you can find them in the [Hyperspec][hyperspec] which has reference pages for [`eq`][hyper-eq], [`eql`][hyper-eql], [`equal`][hyper-equal], [`equalp`][hyper-equalp].

[hyperspec]: http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
[hyper-eq]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm
[hyper-eql]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm
[hyper-equal]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm
[hyper-equalp]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm
[eli-lisp-equality]: https://eli.thegreenplace.net/2004/08/08/equality-in-lisp
