When the Lisp aliens first came to Earth, they encountered countless weird and
wonderful things, but one of their favorite discoveries was that of
"pets". Let's be honest, what sort of alien wouldn't gush over a puppy or
kitten?

Ludwig, after his recent holiday to earth, has decided he wants a pet of his
own! The only issue? Far too many choices! Perhaps you could write a program to
help Ludwig find and care for his next cuddly friend?

## 1. Picking a Pal
Though Ludwig is set on getting a pet, that's about as far as he's gotten in
thinking things through. The first step, then, is deciding what sort of pet to
get!

Ludwig wants to find a pal with a personality that complements his own. Could
you write Ludwig a function that takes some personality trait (a keyword) and
returns the type of pet (a string) with the given trait?

In this particular case:

  - `:lazy` -> `"Cat"`
  - `:energetic` -> `"Dog"`
  - `:quiet` -> `"Fish"`
  - `:hungry` -> `"Rabbit"`
  - `:talkative` -> `"Bird"`

For other, unknown personalities, your function should evaluate to: `"I don't
know... A dragon?"`

```lisp
(pal-picker :quiet) ; => "Fish"
```

## 2. In Their Natural Habitat
Now that Ludwig has a new friend, they'll need a place to stay! Can you help
Ludwig select the right size bed / tank / cage for his pet?

Here, Ludwig needs a function for selecting the proper habitat size (a keyword)
from his pet's weight in kilograms (a integer).

The habitats needed by each size pet are:

  - More than or equal to 40kg -> `:massive`
  - 20kg to 39kg inclusive -> `:large`
  - 10kg to 19kg inclusive -> `:medium`
  - 1kg to 9kg inclusive -> `:small`
  - Less than or equal to 0kg -> `:just-your-imagination`

```lisp
(habitat-fitter 42) ; => :MASSIVE
```
