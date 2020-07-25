## General

- Eli Bendersky has written [a good page][eli-lisp-equality] explaining the differences between the generic equality predicates in Common Lisp.
- The [Hyperspec][hyperspec] is also a good reference and has pages defining each of the predicates: [`eq`][hyperspec-eq], [`eql`][hyperspec-eql], [`equal`][hyperspec-equal], and [`equalp`][hyperspec-equalp]
- Remember that the four predicates range from most restrictive to most permissive in order: `eq`, `eql`, `equal`, `equalp`.

## 1. The maze of object equality

- This maze needs the most restrictive equality predicate available.
- [Eli Bendersky's page about Common Lisp equality][eli-lisp-equality] is a good explanation.
- [The Hyperspec reference page for `eq`][hyperspec-eq] may be helpful.

## 2. The maze of numbers

- The two functions needed for this will not necessarily use the same equality predicates. The first room needs one that is more restrictive than the second.
- [Eli Bendersky's page about Common Lisp equality][eli-lisp-equality] is a good explanation.
- [The Hyperspec reference page for `eql`][hyperspec-eql] may be helpful for the first part
- [The Hyperspec reference page for `equalp`][hyperspec-equalp] may be helpful for the second part

## 3. The maze of characters

- The two functions needed for this will not necessarily use the same equality predicates. The first room needs one that is more restrictive than the second.
- [Eli Bendersky's page about Common Lisp equality][eli-lisp-equality] is a good explanation.
- [The Hyperspec reference page for `eql`][hyperspec-eql] may be helpful for the first part
- [The Hyperspec reference page for `equalp`][hyperspec-equalp] may be helpful for the second part

## 4. The maze of strings

- The two functions needed for this will not necessarily use the same equality predicates. The first room needs one that is more restrictive than the second.
- [Eli Bendersky's page about Common Lisp equality][eli-lisp-equality] is a good explanation.
- [The Hyperspec reference page for `equal`][hyperspec-equal] may be helpful for the first part
- [The Hyperspec reference page for `equalp`][hyperspec-equalp] may be helpful for the second part

## 5. The maze of conses

- This keys for these rooms will need to use equality predicates which are defined to check the contents of conses.
- [Eli Bendersky's page about Common Lisp equality][eli-lisp-equality] is a good explanation.
- [`equal`][hyperspec-equal] is the first of the predicates that checks for equality of items _inside_ of a cons.
- As you have learned [`equalp`][hyperspec-equalp] allows for more permissive equality including case-insensitive equality checking.

## 6. The maze of arrays

- This keys for these rooms will need to use equality predicates which are defined to check the contents of arrays.
- [Eli Bendersky's page about Common Lisp equality][eli-lisp-equality] is a good explanation.
- For the first part one needs to choose a predicate that does not consider the contents of an array. [`eq`][hyperspec-eq] and [`eql`][hyperspec-eql] could both be helpful.
- [`equalp`] is the first predicate that checks for equality of the contents of an array.

[eli-lisp-equality]: https://eli.thegreenplace.net/2004/08/08/equality-in-lisp
[hyperspec]: http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
[hyperspec-eq]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm
[hyperspec-eql]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm
[hyperspec-equal]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm
[hyperspec-equalp]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm
