Lilly the Lisp Alien is back and doing more cooking! This time they
are going to create Lisp Alien Lasagna which is not entirely unlike
Human Lasagna.

Lilly will need some functions written to help figure out the timing
of the cooking of the lasagna. And each one needs to have good
documentation so Lilly won't forget what each one does.

Can you help Lilly write functions to help them time the cooking of
the Lasagna?

## 1. Define the expected oven time in minutes

Define the `expected-minutes-in-oven` function that does not take any
parameters and returns how many minutes the lasagna should be in the
oven. According to the Lisp Alien tradition (just like Lilly's
parental-unit used to cook), the expected oven time in minutes is 337:

```lisp
(expected-minutes-in-oven) ;; => 337
```

## 2. Calculate the remaining oven time in minutes

Define the `remaining-minutes-in-oven` function that takes the actual
minutes the lasagna has been in the oven as a parameter and returns
how many minutes the lasagna still has to remain in the oven, based on
the expected oven time in minutes from the previous task.

```lisp
(remaining-minutes-in-oven 100) ;; => 237
```

## 3. Calculate the preparation time in minutes

Define the `preparation-time-in-minutes` function that takes the
number of layers Lilly added to the lasagna as a parameter and returns
how many minutes Lilly spent preparing the lasagna, assuming each
layer takes 19 minutes to prepare.

```lisp
(preparation-time-in-minutes 3) ;; => 57
```

## 4. Calculate the elapsed time in minutes

Define the `elapsed-time-in-minutes` function that takes two
parameters: the first parameter is the number of layers Lilly added to
the lasagna, and the second parameter is the number of minutes the
lasagna has been in the oven. The function should return how many
minutes Lilly has worked on cooking the lasagna, which is the sum of
the preparation time in minutes, and the time in minutes the lasagna
has spent in the oven at the moment.

```lisp
(elapsed-time-in-minutes 3 100) ;; => 157
```
