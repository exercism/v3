## Running Tests

SWI Prolog can either be run interactively or by executing it directly at the
command line.

To run prolog interactively first run:

`$ swipl`

After the prolog console starts, load your implementation and run the tests
with:

```
?- ["hello_world.pl"].
?- ["hello_world_tests.plt"].
?- run_tests.
```

To run your implementation directly run:

`swipl -f hello_world.pl -s hello_world_tests.plt -g run_tests,halt -t
'halt(1)'`

In both cases, replace `hello_world.pl` and `hello_world_tests.plt` with the
name of the exercise you are implementing.

When you first begin an exercise, only the first test will run. The rest have
been skipped by adding `condition(pending)` to the `test` goal. Once the first
test passes, un-skip the next test by changing `pending` in `condition(pending)`
to `true`. Repeat for each test until they are all running and passing.
