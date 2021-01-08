In Common Lisp a function can have a parameter that will contain the "rest" of the arguments after any required or optional parameters are processed. This parameter is designated by the `&rest` lambda list keyword. If all arguments to a function are used by by other types of parameters then the rest parameter will be bound to an empty list. If there are unused arguments then the rest parameter will be bound to a list of those arguments.

```lisp
(defun rest-of-it (req &optional opt &rest rest) (list req opt rest))
(rest-of-it 1)         ;; => (1 NIL NIL)
(rest-of-it 1 2)       ;; => (1 2 NIL)
(rest-of-it 1 2 3)     ;; => (1 2 (3))
(rest-of-it 1 2 3 4 5) ;; => (1 2 (3 4 5))
```

--
[lambda-list]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list
[lambda-list-keyword]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list_keyword