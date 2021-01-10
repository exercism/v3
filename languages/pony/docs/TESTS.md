## Running tests

To compile and run the tests, just run the following in your exercise directory:
```bash
$ ponyc
$ ./exercise-name
```

(Replace `exercise-name` with the name of the exercise directory)

The compiled binary runs the tests in its Main actor, so just run the binary and see if your tests passed. Pony checks all code at compile time, so you may find that your tests won't compile until you write the required code. You can comment out tests that won't compile by starting each line with a `//`. Then, when you're ready to work on that test, you can un-comment it.
