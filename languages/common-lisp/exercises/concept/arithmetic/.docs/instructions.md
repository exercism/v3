Lilly, a culinarily-inclined Lisp Alien, wants to throw a pizza party for
herself and some friends but, wanting to make the most of her ingredients, would
like to know exactly how much of each component she'll need before starting.

To solve this problem, Lilly has drafted a program to automate some of the
planning but needs your help finishing it. Will you help Lilly throw the
proportionally perfect pizza party?

## 1. A Dough Ratio

First things first, every great pizza starts with a delicious dough! Lilly is a
fan of thin, crispy pizzas with a thicker crust, so the amount of dough needed
for the middle of the pizza remains relatively constant (200g) but the amount
needed for the crust increases as the pizza's size does. Every 20cm of crust
requires 45g of dough.

Lilly is looking to write a function that takes the diameter and number of
pizzas to make and returns the exact amount of dough (to the nearest gram) that
she'll need. For example, to make 4 pizzas 30cm in diameter:

```lisp
(dough-calculator 4 30) ; => 1648
```

## 2. A Splash of Sauce

Lilly is astonishingly meticulous when it comes to her sauce application and
always applies exactly 3ml of sauce to every 10 square centimetres of
pizza. Ironically, the size of her pizzas can be incredibly inconsistent. Lilly
needs your help defining a function that calculates the pizza size from the
amount of sauce applied. For example, given Lilly has used 250ml of sauce:

```lisp
(size-from-sauce 250) ; => 16.29
```

## 3. Some Cheese, Please

On Lilly's planet, all cheese comes in perfect cubes and is sold by size. (What
were you expecting? This is an alien planet after all...) Your task is to help
Lilly calculate how many pizzas she can make using any given cheese
cube. Mozzarella cheese has a density of 0.5 grams per cubic centimetre and
every pizza needs 3 grams of cheese per square centimetre. Given the side-length
of some cheese cube and the pizzas' diameter, calculate the number of pizzas
that can be made (always rounded down). For example, given a 25x25x25cm cheese
cube and pizzas 30cm in diameter:

```lisp
(pizzas-per-cube 25 30) ; => 3
```

## 4. A Fair Share

Finally, Lilly wants to be sure that her pizzas (each made up of 8 slices) can
be evenly divided between her friends. Your task is to define a function
that takes a number of pizzas and number of friends then returns `T` if the
pizza can be evenly divided between friends and `NIL` otherwise. For example:

```lisp
;; For splitting 3 pizzas between 4 friends
(fair-share-p 3 4) ; => T
;; For splitting 2 pizzas between 3 friends
(fair-share-p 2 3) ; => NIL
```
