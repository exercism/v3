A rest parameter is designated by the `&rest` lambda list keyword in a lambda list. This parameter will be bound to a value which is a list of all the arguments provided by the caller which is not consumed by other parameters.

While multiple types of parameters can be combined with other types of parameters (optional and keyword arguments) this can be be problematic and should be done carefully. See the section on ("Mixing Different Parameter Types")(pcl-function) in Practical Common Lisp.

--

[pcl-function]: http://www.gigamonkeys.com/book/functions.html
