In Common Lisp a functions argument list (also known as a ('lambda list')[lambda-list]) can have arguments of different types. These different types are designated with the use of ("lambda list keywords")[lambda-list-keyword] which all begin with `&`. The most commonly used types are optional, keyword and rest arguments types. Every parameter in the lambda list after a particular lambda list keyword is will be of that type. A lambda list keyword can only be used once in a lambda list.

## optional-parameters

In Common Lisp a function can have some arguments are are optional. These are designated in the lambda list by `&optional` lambda list keyword. A parameter will be bound to the value `nil` if it is not specified. If there are several optional parameters they are bound in order. Default values can be specified for optional parameters. Finally a symbol an be specified for each optional parameter which will be bound to true or false depending on whether that parameter was supplied by the caller of the function (this is referred to as the "supplied-p parameter").

```lisp
(defun optional-parameters (&optional x (y 'default) (z nil z-supplied-p))
  (list x y (if z-supplied-p (list :z-was-supplied z)
                             (list :z-was-not-supplied z))))

(optional-parameters) ; => '(nil 'default (:z-was-not-supplied nil))
(optional-parameters 5 nil 10) => '(5 nil (:z-was-supplied 10))
```

## named-parameters

In Common Lisp a function can have named parameters (referred to as "keyword parameters" or "keyword arguments"). These are designated in the lambda list by the `&key` lambda list keyword. Keyword parameters are not required parameters. Like optional parameters they can be given default values and symbols to bind to their 'supplied-or-not' state.

When calling a function with keyword parameters the name of the parameter as a keyword is used in front of the parameter value. Keyword parameters can be specified by the caller of the function in any order.

```lisp
(defun keyword-parameters (&key x (y 'default) (z nil z-supplied-p))
  (list x y (if z-supplied-p (list :z-was-supplied z)
                             (list :z-was-not-supplied z))))

(keyword-parameters) ; => '(nil 'default (:z-was-not-supplied nil))
(keyword-parameters :y 5) ; => '(nil 5 (:z-was-not-supplied nil))
(keyword-parameters :z 10 :x 5) => '(5 nil (:z-was-supplied 10))
```

Care should be taken when combining optional and keyword arguments as the keyword name and argument could be consumed by optional parameters:

```lisp
(defun could-be-confusing (&optional x y &key z) (list x y z))
(could-be-confusing :z 'huh?) ; => '(:z huh? nil)
```

## rest-parameters

In Common Lisp a function can have a parameter that will contain the "rest" of the arguments after any required or optional parameters are processed. This parameter is designated by the `&rest` lambda list keyword. If all arguments to a function are used by by other types of parameters then the rest parameter will be bound to an empty list. If there are unused arguments then the rest parameter will be bound to a list of those arguments.

```lisp
(defun rest-of-it (req &optional opt &rest rest) (list req opt rest))
(rest-of-it 1) ; => '(1 nil nil)
(rest-of-it 1 2) ; => '(1 2 nil)
(rest-of-it 1 2 3) ; => '(1 2 (3))
(rest-of-it 1 2 3 4 5) ; => '(1 2 (3 4 5))
```

--
[lambda-list]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list
[lambda-list-keyword]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list_keyword
