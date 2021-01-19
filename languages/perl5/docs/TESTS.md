### Run All Tests

There is a Perl 5 script with the extension `.t`, which will be used to test
your solution. You can run through the tests by using the command:

`prove .`

Before you start the exercise, the output will likely look something like:

```
./hello-world.t .. 1/2
# Failed test 'Say Hi!'
# at exercises/hello-world/hello-world.t line 15.
# +---------+---------------+
# | GOT     | CHECK         |
# +---------+---------------+
# | <UNDEF> | Hello, World! |
# +---------+---------------+
exercises/hello-world/hello-world.t .. Dubious, test returned 1 (wstat 256, 0x100)
Failed 1/2 subtests

Test Summary Report
-------------------
./hello-world.t (Wstat: 256 Tests: 2 Failed: 1)
  Failed test:  2
  Non-zero exit status: 1
Files=1, Tests=2,  0 wallclock secs ( 0.03 usr  0.00 sys +  0.18 cusr  0.02 csys =  0.23 CPU)
Result: FAIL
```

You will either need to modify or create a module with the extension `.pm`, and
write a solution to pass the tests. Once the tests are passing, the output from
the command above will likely look something like:

```
./hello-world.t .. ok
All tests successful.
Files=1, Tests=2,  0 wallclock secs ( 0.02 usr  0.00 sys +  0.07 cusr  0.01 csys =  0.10 CPU)
Result: PASS
```

Some exercises may have optional tests. You can test for these by adding the
flag `-v` (for 'verbose') to the above command, like so:

`prove . -v`

As well as showing optional tests, it will include all of the tests that
your solution currently passes.

You can find a more in-depth explanation of `Test2` on
[perldoc](https://perldoc.pl/Test2).
