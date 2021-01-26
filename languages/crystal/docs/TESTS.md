## Running Tests

When you are in the directory for an exercise, you should see two subdirectories:

* `src` contains your solution to the exercise
* `spec` contains the tests to run for the exercise

If you're in the right directory (i.e. the one containing `src` and `spec`), you can run the tests for that exercise by running `crystal spec`:

```bash
$ pwd
/Users/johndoe/Code/exercism/crystal/hello-world

$ ls
GETTING_STARTED.md README.md          spec               src

$ crystal spec
```

This will run all of the test files in the `spec` directory.

In each test file, all but the first test have been skipped.

Once you get a test passing, you can unskip the next one by changing `pending` to `it`.

## Crystal Format

Before submitting, it's recommended that you run `crystal tool format` on your solution.

You can format everything in your current directory with:

```bash
$ crystal tool format ./
```

Or you can selectively format files with:

```bash
$ crystal tool format ./<path>/<to>/<file>
```

## Submitting Your Solution

Be sure to submit the source file in the `src` directory when submitting your solution:

```bash
$ exercism submit src/<exercise>.cr
```
