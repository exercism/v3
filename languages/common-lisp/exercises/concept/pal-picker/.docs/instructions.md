When the Lisp aliens first came to Earth, they encountered countless weird and
wonderful things, but one of their favorite discoveries was that of
"pets". Let's be honest, what sort of alien wouldn't gush over a puppy or
kitten?

Ludwig, after their recent holiday to Earth, has decided they want a pet of their
own! The only issue? Far too many choices! Perhaps you could write a program to
help Ludwig find and care for their next cuddly friend?

## 1. Picking a Pal

Though Ludwig is set on getting a pet, that's about as far as they've gotten in
thinking things through. The first step, then, is deciding what sort of pet to
get!

Ludwig wants to find a pal with a personality that complements their own. Could
you write Ludwig a function called `pal-picker` that takes some personality
trait (a keyword) and returns the type of pet (a string) with the given trait?

In this particular case:

- `:lazy` -> `"Cat"`
- `:energetic` -> `"Dog"`
- `:quiet` -> `"Fish"`
- `:hungry` -> `"Rabbit"`
- `:talkative` -> `"Bird"`

For other, unknown personalities, your function should evaluate to: `"I don't know... A dragon?"`

```lisp
(pal-picker :quiet) ; => "Fish"
```

## 2. In Their Natural Habitat

Now that Ludwig has a new friend, they'll need a place to stay! Can you help
Ludwig select the right size bed / tank / cage for their pet?

Here, Ludwig needs a function called `habitat-fitter` for selecting the proper
habitat size (a keyword) from their pet's weight in kilograms (a integer).

The habitats needed by each size pet are:

- More than or equal to 40kg -> `:massive`
- 20kg to 39kg inclusive -> `:large`
- 10kg to 19kg inclusive -> `:medium`
- 1kg to 9kg inclusive -> `:small`
- Less than or equal to 0kg -> `:just-your-imagination`

```lisp
(habitat-fitter 42) ; => :MASSIVE
```

## 3. And Now, We Feast

One thing all earthling pets have in common is their need for food! This concept
is somewhat alien to Ludwig, however, as they are prone to forgetting to refill their
pet's food-bowl.

Ludwig could use a simple function called `feeding-time-p` to alert them when the
bowl needs refilling. The function would take a percent fullness (an integer)
and return a message in the form of a string.

If the food level is:

- Above 20% -> `"All is well."`
- 20% or below -> `"It's feeding time!"`

```lisp
(feeding-time-p 15) ; => "It's feeding time!"
```

## 4. A Code of Conduct

With all of the basics sorted, Ludwig is looking forward to trying out a number
of exciting things like "petting" and playing "fetch". With that being said, not
every pet is suitable for these activities.

Ludwig would like a pair of functions – `pet` and `play-fetch` – that take the
type of pet (as a string) and return either nothing or a message like: `"Maybe not with this pet..."` if the action is unfitting.

Assume that only `"Dog"`s will play fetch and that all pets except `"Fish"` can
be pet.

```lisp
(pet "Dog")         ; => NIL
(play-fetch "Fish") ; => "Maybe not with this pet..."
```
