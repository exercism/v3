## 1. Check if a fast attack can be made

We need to check if the knight is awake and *invert* its truth value. This can be done with the [`not`][not] function.

## 2. Check if the group can be spied upon

We want the function to return `true` if *any* of the supplied predicates are true. This can be done using the [`or`][or] function.



[not]: https://clojuredocs.org/clojure.core/not
[or]: https://clojuredocs.org/clojure.core/or
