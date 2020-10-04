Leslie the Lisp Alien needs to do some shopping. It is very important to have a shopping list. One needs to add things to it, remove things from it, and even look if something is on the list for instance.

Of course simple pen and paper will not do for a Lisp Alien. "List" is most of the word "Lisp" even!. There must be some functions written to help keep track of the shopping.

Can you help Leslie keep track of the shopping list?

## 1. Making a new list

First thing is that Leslie needs to create a new empty list. A function called `empty-list` would be perfect for that.

```lisp
(empty-list) ; => ()
```

Oh no... Leslie actually has a few things in mind already so she needs a function that takes a three items (luckily Leslie never creates a list if she has less or more than three items) and creates a new shopping list with those things. Write a function called `list-of-things` which will take three items and makes a list of them.

```lisp
(list-of-things 'bread 'milk 'butter) ; => '(bread milk butter)
```

## 2. Add things to the list.

Before going to the store Leslie looks in the pantry to see what they need. Help them out by writing the function `add-to-list` which adds an item to the beginning of a list.

```lisp
(add-to-list 'butter '(bread)) ; => '(butter bread)
```

## 3. What's next thing(s) on the list?

When they are out shopping Leslie wants to know what to look for next. But they also like to peek ahead at the list to see what the second, or third items are. Other times they want to check the 23rd item (their lucky number).

- `first-thing` will evaluate to the first thing on the list
- `second-thing` will evaluate to the second thing
- `third-thing` will evaluate to the third thing
- `twenty-third-thing` will evaluate to the twenty-third thing

```lisp
(first-thing '(bread butter milk)) ; => 'bread
(second-thing '(bread butter milk)) ; => 'butter
(third-thing '(bread butter milk)) ; => 'milk
```

## 4. Removing a thing from the list

When Leslie finds the first thing on the list on the shelf then want to remove the item from the list. Help them out by writing a function `remove-first-item` which will evaluate to a list with everything but the first thing on the input list.

```lisp
(remove-first-item '(bread butter milk)) ; => '(butter milk)
```

## 5. Bigger lists out of smaller lists

Leslie realized they accidentally made two shopping lists not one! Write a function called `list-append` which adds all the items from the second list provided to the end of the first list.

```lisp
(list-append '(bread salt) '(butter milk)) ; => '(bread salt butter milk)
```

## 6. How much longer?

Leslie is starting to get worried that this shopping trip is going to take quite a while. Just how many things are on this list? Write a function `just-how-long` to tell them just how long their list is.

```lisp
(list-append '(bread milk butter salt)) ; => 4
```
