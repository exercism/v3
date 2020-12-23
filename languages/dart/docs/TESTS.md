# Running the Tests

Choose your operating system:

* [Windows](#windows)
* [Mac OS X](#mac-os-x)
* [Linux](#linux)

## Understanding Test Result

The test run summary will list the number of passed, skipped and failed tests.
For example :

```
❯ pub run test
00:01 +4 ~3 -1: Some tests failed.
```

`+` describes the number of tests passed. In this example, 4 tests are passed.

`~` describes the number of tests skipped. In this example, 3 tests are skipped.

`-` describes the number of tests failed. In this case, there is only one test that failed.

The `skip` parameter instructs the test suite to not run a test. 
This is commonly used to avoid running tests of unimplemented functionality, so you can focus on the part you are currently working on.

It is advised to enable tests one by one as you implement the functionality being tested or as you move towards edge cases. 
You can do so by flipping the `skip: false` to `skip: true` flag for each test. 
You can also submit your exercise without passing all tests to get feedback.

----

# Windows

1. Open a Command Prompt.

1. Get the first exercise:

     ```batchfile
     C:\Users\JaneDoe>exercism fetch dart

     Not Submitted:     1 problem
     dart (Hello World) C:\Users\JaneDoe\exercism\dart\hello-world

     New:               1 problem
     dart (Hello World) C:\Users\JaneDoe\exercism\dart\hello-world

     unchanged: 0, updated: 0, new: 1
     ```

1. Change directory into the exercism:

     ```batchfile
     C:\Users\JaneDoe>cd C:\Users\JaneDoe\exercism\dart\hello-world
     ```

1. Download the dependent packages:

     ```batchfile
     C:\Users\JaneDoe>pub get
     ```

1. Run the tests:

     ```batchfile
     C:\Users\JaneDoe>pub run test
     ```
   *(Don't worry about the tests failing, at first, this is how you begin each exercise.)*

1. Solve the exercise.  Find and work through the `README.md` guide ([view on GitHub](https://github.com/exercism/dart/blob/master/exercises/hello-world/README.md)).


Good luck!  Have fun!

If you get stuck, at any point, don't forget to reach out for [help](http://exercism.io/languages/dart/help).

----

# Mac OS X

1. In the terminal window, get the first exercise:

     ```shell
     $ exercism fetch dart

     New:                 1 problem
     Dart (Etl) /Users/janedoe/exercism/dart/hello-world

     unchanged: 0, updated: 0, new: 1
     ```

1. Change directory into the exercise:

     ```shell
     $ cd /Users/janedoe/exercism/dart/hello-world
     ```

1. Download the dependent packages:

     ```shell
     $ pub get
     ```

1. Run the tests:

     ```shell
     $ pub run test
     ```
   *(Don't worry about the tests failing, at first, this is how you begin each exercise.)*

1. Solve the exercise.  Find and work through the `README.md` guide ([view on GitHub](https://github.com/exercism/dart/blob/master/exercises/hello-world/README.md)).

Good luck!  Have fun!

If you get stuck, at any point, don't forget to reach out for [help](http://exercism.io/languages/dart/help).

----

# Linux

1. In the terminal window, get the first exercise:

     ```shell
     $ exercism fetch dart

     New:                 1 problem
     Dart (Etl) /home/janedoe/exercism/dart/hello-world

     unchanged: 0, updated: 0, new: 1
     ```

1. Change directory into the exercise:

     ```shell
     $ cd /home/janedoe/exercism/dart/hello-world
     ```

1. Download the dependent packages:

     ```shell
     $ pub get
     ```

1. Run the tests:

     ```shell
     $ pub run test
     ```
   *(Don't worry about the tests failing, at first, this is how you begin each exercise.)*

1. Solve the exercise.  Find and work through the `README.md` guide ([view on GitHub](https://github.com/exercism/dart/blob/master/exercises/hello-world/README.md)).

Good luck!  Have fun!

If you get stuck, at any point, don't forget to reach out for [help](http://exercism.io/languages/dart/help).
