For each example, the following general steps are required.

First, change directory to the exercise you want to practice, in
this case `bob`.

```bash
$ cd </path/to/exercism>/lfe/bob
```

Next run the tests.  The `test` make target depends on `compile`,
so running `make test` will ensure your source is compiled,
 and then proceed to run the tests.


```bash
$ make test
```

Congratulations! You have failing tests!!!

Now read `README.md` to find out what you need to do to get all
your tests passing.


Fix the first error reported by the test, and run the tests again.

```bash
$ make test
```

Continue by fixing the next failing test in your source code, and
running your test until you get all the test passing.

Once all your test pass, take a final look over your code in the `src`
directory to ensure you are happy with your code and submit for review.

Get feedback, revise solution, submit, try next exercise, and recurse
until LFE enlightenment is achieved.

