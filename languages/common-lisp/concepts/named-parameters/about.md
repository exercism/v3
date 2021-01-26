In Common Lisp named parameters are called keyword parameters.

Keyword parameters are designated by the `&key` lambda list keyword in a lambda list. Keyword parameters are not required, can have a default and also can specify a "supplied-p parameter" which will be "true" or "false" depending on whether an argument was provided for the parameter.

```lisp
(defun keyword-parameter (&key (arg -1) arg)
(keyword-parameter)         ;; => -1
(keyword-parameter :arg 13) ;; => 13
```

In the arguments to a function the keyword parameters are specified by their "keyword name" which is, by default, a keyword symbol version of the parameter name (_i.e._ keyword parameter `name` has a keyword name of `:name`). It is possible to specify another name for the keyword parameter by using a list of keyword name and parameter name instead of just the parameter name:

```lisp
(defun other-keyword-name (&key ((other-name arg))) (list arg))
(other-keyword-name 'other-name 5) ;; => (5)
```

While multiple types of parameters can be combined with other types of parameters (optional and keyword arguments) this can be be problematic and should be done carefully. See the section on ("Mixing Different Parameter Types")(pcl-function) in Practical Common Lisp.

--

[pcl-function]: http://www.gigamonkeys.com/book/functions.html
