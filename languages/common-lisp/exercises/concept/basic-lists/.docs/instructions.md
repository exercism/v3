Leslie the Lisp Alien needs to do some shopping. It is very important to have a shopping list. One needs to add things to it, remove things from it, and even look if something is on the list for instance.

Of course simple pen and paper will not do for a Lisp Alien. "List" is most of the word "Lisp" even!. There must be some functions written to help keep track of the shopping.

Can you help Leslie keep track of the shopping list?

## 1. Making a new list

First thing is that Leslie needs to create a new empty list. A function called `empty-list` would be perfect for that.

Oh no... Leslie actually has a few things in mind already so she needs a function that takes a few (or a lot) of items and creates a new shopping list with those things. Write a function called `list-of-things` which will take zero or more things and makes a list of them.

## 2. Add things to the list.

Before going to the store Leslie looks in the pantry to see what they need. Help them out by writing the function `add-to-list` which adds an item to the beginning of a list.

## 3. What's next thing(s) on the list?

When they are out shopping Leslie wants to know what to look for next. But they also like to peek ahead at the list to see what the second, or third items are. Other times they want to check the 23rd item (their lucky number). Finally they sometimes like to see everything on the list after they buy the next thing.

- `first-thing` will evaluate to the first thing on the list
- `second-thing` will evaluate to the second thing
- `third-thing` will evaluate to the third thing
- `twenty-third-thing` will evaluate to the twenty-third thing
- `not-the-first-thing` will evaluate to everything but the first thing.

## 4. Removing a thing from the list

When Leslie finds the first thing on the list on the shelf then want to remove the item from the list. Help them out by writing a function `remove-first-item` which will remove that first item from the list.

## 5. Are you a member?

While out shopping Leslie remembered they need something. But maybe it is already on the list? Write a function `on-the-list-p` which checks to see if a thing is a member of the list.

## 6. Bigger lists out of smaller lists

Leslie realized they accidentally made two shopping lists not one! Write a function called `list-append` which adds all the items from the second list provided to the end of the first list.

## 7. How much longer?

Leslie is starting to get worried that this shopping trip is going to take quite a while. Just how many things are on this list? Write a function `just-how-long` to tell them just how long their list is.

## 8. Divide and conquer.

The list seems pretty long. Leslie programs some robots to help her shop. But each one needs part of the list. Write a function `part-of-list` which takes a list, and two numbers, the first number is the index of the first item to put into the new list and the second is the number of items to put into that list.

## 9. Meeting in the middle.

As well as giving the robots part of the shopping list, Leslie wants to have them work from the other end of the store. Leslie created the list so that the items were in the order found on the store shelves, so they want to put the list given to the robots in reverse order so they will start on the other end of the store and Leslie and the robots can meet in the middle. Help them out by writing a function `list-reverse` that will evaluate to the input list but in the opposite order.
