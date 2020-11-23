You! Yes you, brave lisper! [Lenny the lisp alien][alien] needs your help! He
wants to go hang out with all the other lisp aliens, but his parents have told him
that he's not going anywhere until his room is clean.

The issue is, Lenny can hardly see the floor! He can't tell his atoms from his
conses, his symbols from his keywords, his `car`s from his `cdr`s!

Can you help Lenny sort this mess out? Don't forget [your parentheses][xkcd],
you'll be needing them!

## 1. Refresh Lenny on symbols

Before Lenny can get started, he needs a refresher on what symbols and keywords
look like – it's been a while since he last tidied up... Help Lenny out by
defining two functions: `lennys-favorite-food` which evaluates to some symbol,
and `lennys-secret-keyword` which evaluates to some keyword. For example:

```lisp
(lennys-favorite-food) ; => LASAGNA
```

```lisp
(lennys-secret-keyword) ; => :ALIENS-ARE-REAL
```

Note that you can return any symbol or keyword you'd like (if you're not a fan
of lasagna or aren't a believer in aliens).

## 2. Help Lenny distinguish between atoms and conses

Once Lenny has had a refresher on symbols and keywords, it's time to get
cleaning! To sort through all of the S-Expressions laying around his room, Lenny
needs help determining what is a cons and what is an atom. You can help by
implementing two functions, `is-an-atom-p` and `is-a-cons-p`:

```lisp
(is-an-atom-p 'one-thing)    ; => T
(is-an-atom-p '(two things)) ; => NIL
```

```lisp
(is-a-cons-p 'one-thing)    ; => NIL
(is-a-cons-p '(two things)) ; => T
```

For this section, there are a number of built-in lisp functions that you may
find useful. Additionally, for now, you can simply consider `T` as true and
`NIL` as false.

## 3. Help Lenny split up his conses

Finally, Lenny's conses are too bulky to put away neatly, so he needs to split
them up into smaller parts. Can you help by defining two more functions to break
up his conses (`first-thing` and `rest-of-it`)?

```lisp
(first-thing '(first second third)) ; => FIRST
(rest-of-it  '(first second third)) ; => (SECOND THIRD)
```

Again, some of Common Lisp's built-in functions might come in handy here.

[alien]: http://www.lisperati.com/logo.html
[xkcd]: https://xkcd.com/297/
