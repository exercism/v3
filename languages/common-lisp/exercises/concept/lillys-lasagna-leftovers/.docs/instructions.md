Lilly the Lisp Alien has definitely made a lot of Lasagna with your
help. They want to thank you by giving you some leftovers. But first
they have some feedback about one of the functions you helped them
with.

In this exercise you are going to write some more functions to help
Lilly with their Lasagna cooking as well as helping them split the
leftovers with you.

You have six tasks, three about improving the existing Lasagna cooking
functions and three about splitting the leftovers.

## 1. Make `preparation-time-in-minutes` easier to use

Lilly says they found using your `preparation-time-in-minutes` a
little awkward to use since they had to count all the layers before
using the functions. Lilly wonders if you could make a version of the
function that takes the layers (any number of them) as arguments and
then calculate the preparation time based upon how many layers there
were? As a reminder, it takes 19 minutes to prepare a single layer.

```lisp
(preparation-time 'sauce 'cheese 'right-handed-macaroni 'cheese 'sauce
                  'left-handed-macaroni 'sauce 'sauce 'cheese 'cheese)
                  ;; => 190 (because there are 10 layers.
```

## 2. Allow changing the expected oven time

While Lilly's parental-units always cooked their lasagna for 337
minutes, Lilly does admit that other Lisp Aliens like to cook their
lasagna for longer or shorter amounts of time.

So it would be good if `remaining-minutes-in-oven` would not use
`expected-minutes-in-oven` but instead could take an _optional_
parameter to define if the time should be longer, very long, shorter
or very short. Longer or shorter means add or subtract 100 from the
default time of 337. Very long or very short means add or
subtract 200. If no parameter is provided, or the argument is 'normal'
then the default value of 337 should be returned.

```lisp
(remaining-minutes-in-oven)             ;; => 337
(remaining-minutes-in-oven :normal)     ;; => 337
(remaining-minutes-in-oven :shorter)    ;; => 237
(remaining-minutes-in-oven :longer)     ;; => 437
(remaining-minutes-in-oven :very-long)  ;; => 537
```

## 3. Lilly remembers another preferred cooking style

Just after you finished that change Lilly remembers that there are
some Lisp Aliens that like _really short_ cooking times. Effectively
they want it raw. So now the function should return 0 if `NIL` was the
argument as well as the same values as in the previous task.

```lisp
(remaining-minutes-in-oven)     ;; => 337
(remaining-minutes-in-oven nil) ;; => 0
```

## 4. Splitting with the leftovers

Now that you've helped improve the lasagna cooking functions there is
_a lot of lasanga_! Lilly wants to repay you for all your help by
splitting the leftovers with you. You will need to write a function
that takes three parameters: the total amount of leftovers, the number
of containers for leftovers Lilly found in their cupboards and the
same for you. Because searching for leftover containers may take
different amounts of time, and the exact amount of leftovers may be
determined while yourself and Lilly are out searching your cupboards
you both agree that the function should take the arguments in any
order. So as each number is determined you can write it down. When you
have all three numbers you can just close the parenthesis on the
function call and evaluate it.

The function should return the amount of leftovers which are leftover,
that is, the difference between the weight of left-overs and the total
number of containers that you and Lilly found.

```lisp
(split-leftovers :weight 20 :human 10 :alien 5) ;; => 5
(split-leftovers :weight 20 :alien 10 :human 2) ;; => 8
(split-leftovers :alien 12 :weight 20 :human 4) ;; => 4
```

## 5. Making assumptions

After a few trips back and forth to your respective cupboards, you
both agree that it would be great if the function could just _assume_
that you or Lilly had... say 10 containers if it were not specified.

```lisp
(split-leftovers :weight 20 :human 5) ;; => 5
(split-leftovers :weight 20 :alien 5) ;; => 5
(split-leftovers :weight 20)          ;; => 0
```

## 6. Standard amount of leftovers

Even better the function could _assume_ that there is enough leftovers
to fit into the containers. So if the weight is not provided the
function should return `'just-split-it`. _However_ if the weight is
provided and it is `nil` or `0` then it should return
`'looks-like-someone-was-hungry`.

```lisp
(split-left-overs :human 5 :alien 5) ;; => 'JUST-SPLIT-IT
(split-left-overs :weight NIL :human 5 :alien 5) ;; => 'LOOKS-LIKE-SOMEONE-WAS-HUNGRY
```
