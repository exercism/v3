# Tests

A Clojurescript REPL allows you to easily run code and get immediate feedback and can also be used to run tests.

## Leiningen
To open a REPL using Clojure CLI change to the directory containing the exercise and run:
``` bash
$ clojure -m cljs.main -re node
```

Note that: the Node should be installed on your system.

Once you are ready to work on an exercise and have created a file to hold your solution (such as `bob.cljs`) you can run the tests using in your terminal `$ clojure -A:test`.

First, `require` the test namespace:
``` clojure
=> (require 'bob-test)
nil
;; and the test namespace
=> (require 'cljs.test)
```

Change the ns to the test namespace:
```clojure
=> (ns bob-test)
nil
```

Then call `run-tests` on `bob-test`:
``` clojure
=> (cljs.test/run-tests)

Testing bob-test

Ran 14 tests containing 14 assertions.
0 failures, 0 errors.
{:test 14, :pass 14, :fail 0, :error 0, :type :summary}
```

To run an exercise's tests with Clojure CLI, simply call:
``` bash
$ clojure -A:test

Testing bob-tests

Ran 14 tests containing 14 assertions.
0 failures, 0 errors.
```
