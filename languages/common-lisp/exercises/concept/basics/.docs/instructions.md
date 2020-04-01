You! Yes you, brave lisper! [Lenny the lisp alien][alien] needs your help! He
wants to go hang out with all the other lisp aliens, but his mom has told him
that he's not going anywhere until his room is clean.

The issue is, Lenny can hardly see the floor! He can't tell his atoms from his
conses, his symbols from his keywords, his `car`s from his `cdr`s!

Can you help Lenny sort this mess out? Don't forget [your parentheses][xkcd],
you'll be needing them!

## Symbols and Quoting

Before Lenny can get started, he needs a refresher on what symbols and keywords
look like – it's been a while since he last tidied up... Help Lenny out by
defining two functions: one that returns a symbol, and another that returns a
keyword. More specifically:

```lisp
(lennys-favorite-food) ; => LASAGNA
```

```lisp
(lennys-secret-keyword) ; => :ALIENS-ARE-REAL
```

## S-Expressions

Once Lenny has had a refresher on symbols and keywords, it's time to get
cleaning! To sort through all of the S-Expressions laying around his room, Lenny
needs help defining four functions: one for testing if things are atoms, another
for conses, and one for getting each part of a cons. For example:

```lisp
(is-an-atom-p 'one-thing)    ; => T
(is-an-atom-p '(two things)) ; => NIL
```

```lisp
(is-a-cons-p 'one-thing)    ; => NIL
(is-a-cons-p '(two things)) ; => T
```

```lisp
(first-thing '(first second third)) ; => FIRST
(rest-of-it  '(first second third)) ; => (SECOND THIRD)
```

For this section, there are a number of built-in lisp functions that you may
find useful. Additionally, for now, you can simply consider `T` as true and
`NIL` as false.

[alien]: http://www.lisperati.com/logo.html
[xkcd]: https://xkcd.com/297/
