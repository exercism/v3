You! Yes you, brave lisper! [Lenny the lisp alien][alien] needs your help! Lenny
wants to go hang out with all the other lisp aliens, but their parents have told Lenny
that they're not going anywhere until their room is clean.

The issue is, Lenny can hardly see the floor! Lenny can't tell their atoms from their
conses, their symbols from their keywords, their `car`s from their `cdr`s!

Can you help Lenny sort this mess out? Don't forget [your parentheses][xkcd],
you'll be needing them!

## 1. Refresh Lenny on symbols

Before Lenny can get started, they need a refresher on what symbols and keywords
look like – it's been a while since they last tidied up... Help Lenny out by
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
cleaning! To sort through all of the S-Expressions laying around their room, Lenny
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

## 3. Help Lenny split up their conses

Finally, Lenny's conses are too bulky to put away neatly, so they need to split
them up into smaller parts. Can you help by defining two more functions to break
up their conses (`first-thing` and `rest-of-it`)?

```lisp
(first-thing '(first second third)) ; => FIRST
(rest-of-it  '(first second third)) ; => (SECOND THIRD)
```

Again, some of Common Lisp's built-in functions might come in handy here.

[alien]: http://www.lisperati.com/logo.html
[xkcd]: https://xkcd.com/297/
