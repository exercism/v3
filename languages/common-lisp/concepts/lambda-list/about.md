In Common Lisp a parameter list (such as for defining a function) is also known as a (lambda list)[lambda-list]. Lambda lists are also used for defining macros and destructuring.

Lambda lists can contain (lambda list keywords)[lambda-list-keyword] such as `&rest`, `&optional`, `&key` and others.

While multiple types of parameters can be combined with other types of parameters (optional and keyword arguments) this can be be problematic and should be done carefully. See the section on ("Mixing Different Parameter Types")(pcl-function) in Practical Common Lisp.

--

[lambda-list]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list
[lambda-list-keyword]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list_keyword
[pcl-function]: http://www.gigamonkeys.com/book/functions.html
