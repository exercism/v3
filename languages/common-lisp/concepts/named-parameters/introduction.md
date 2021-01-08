In Common Lisp a function can have named parameters (referred to as "keyword parameters" or "keyword arguments"). These are designated in the lambda list by the `&key` lambda list keyword. Keyword parameters are not required parameters. Like optional parameters they can be given default values and symbols to bind to their 'supplied-or-not' state.

When calling a function with keyword parameters the name of the parameter as a keyword is used in front of the parameter value. Keyword parameters can be specified by the caller of the function in any order.

```lisp
(defun keyword-parameters (&key x (y 'default) (z nil z-supplied-p))
  (list x y (if z-supplied-p (list :z-was-supplied z)
                             (list :z-was-not-supplied z))))

(keyword-parameters)            ;; => (NIL DEFAULT (:Z-WAS-NOT-SUPPLIED NIL))
(keyword-parameters :y 5)       ;; => (NIL 5 (:Z-WAS-NOT-SUPPLIED NIL))
(keyword-parameters :z 10 :x 5) ;; => (5 NIL (:Z-WAS-SUPPLIED 10))
```

Care should be taken when combining optional and keyword arguments as the keyword name and argument could be consumed by optional parameters:

```lisp
(defun could-be-confusing (&optional x y &key z) (list x y z))
(could-be-confusing :z 'huh?) ;; => (:Z HUH? NIL)
```