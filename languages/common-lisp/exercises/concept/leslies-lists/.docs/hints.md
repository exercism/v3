## 1. Making a new list

- Common Lisp has a [rather descriptively named function][hyper-list] for making a _LIST_.

## 2. Add things to the list.

- To add something to the beginning of a list it is idiomatic to *CONS*truct a new list with the item as the `car` and the list as the `cdr`. In the Hyperspec can be found a [page][hyper-cons] that describes this function.

## 3. What's next thing(s) on the list?

- Common Lisp has a general purpose function to get an item [from any index of a list][hyper-nth].
- It also has [several helpers][hyper-first-tenth] for accessing indexes under 11
- List indexes are zero based.

## 4. Removing a thing from the list

- An idiomatic way to remove the first item of a list is to use a function which can separate the `cdr` of the list from the `car` of the list. In the Hyperspec one can find a [page][hyperspec-rest] describing one of the functions to do this.

## 5. Bigger lists out of smaller lists

- Common Lisp has a function [specifically for adding one list to front of another][hyper-append]

## 6. How much longer?

- Common Lisp has an [obviously named][hyper-length] function to get the length of a list.

[hyper-append]: http://www.lispworks.com/documentation/HyperSpec/Body/f_append.htm
[hyper-cons]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cons.htm
[hyper-first-tenth]: http://www.lispworks.com/documentation/HyperSpec/Body/f_firstc.htm
[hyper-length]: http://www.lispworks.com/documentation/HyperSpec/Body/f_length.htm
[hyper-list]: http://www.lispworks.com/documentation/HyperSpec/Body/f_list_.htm
[hyper-nth]: http://www.lispworks.com/documentation/HyperSpec/Body/f_nth.htm
[hyper-rest]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rest.htm
