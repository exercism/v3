An optional parameter is designated by the `&optional` lambda list keyword in a lambda list. Optional parameters are not required, can have a default and also can specify a "supplied-p parameter" which will be "true" or "false" depending on whether an argument was provided for the parameter.

```lisp
(defun optional-parameter (&optional (arg -1)) arg)
(optional-parameter)    ;; => -1
(optional-parameter 13) ;; => 13
```

Optional parameters must be put after required parameters but before named or rest parameters. Arguments are first bound to required parameters, then optional parameters and finally to rest and named parameters.

```lisp
(defun req-and-opt (req &optional (opt -1)) (list req opt)
(req-and-opt 1)    ;; => (1 -1)
(req-and-opt 1 13) ;; => (1 13)
```

While multiple types of parameters can be combined with other types of parameters (optional and keyword arguments) this can be be problematic and should be done carefully. See the section on ("Mixing Different Parameter Types")(pcl-function) in Practical Common Lisp.

--

[pcl-function]: http://www.gigamonkeys.com/book/functions.html
