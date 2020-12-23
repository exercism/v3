# Running tests

Elisp tests are run using [ERT](http://www.emacswiki.org/emacs/ErtTestLibrary), an Emacs Lisp library for regression/unit
testing. Tests can be run several ways:

1. Interactively and individually, with `M-x ert RET test-name RET`
2. Interactively and all at once, with `M-x ert RET t RET`
3. From the terminal, in batch mode, with `emacs -batch -l ert -l my-test.el -f
   ert-run-tests-batch-and-exit`
4. Other options can be found in the docs, `C-h i m ert RET`

Interactive testing is great while you're working on an exercise, but batch mode
testing is preferable for when you want to check that an exercise is ready for
submission. The above command is a bit unwieldy, so if you like:

1. Create a file on your `$PATH` (probably in `~/bin`) called `ert-run`
2. The contents of the file should be as follows:
   ```sh
   #!/usr/bin/sh
   emacs -batch -l ert -l $1 -f ert-run-tests-batch-and-exit
   ```
3. Make the file executable with `chmod +x ert-run`

You should be able to simply call `ert-run exercise-test.el` and run the tests
in batch mode.

### Working on exercises
Since Emacs is, itself, an elisp interpreter, your working code is always in its
native execution environment. You can evaluate any form by pressing `C-x e` at
the end of the form, or a selection with `M-x eval-region` or the whole buffer
with `M-x eval-buffer`. This can be extremely useful for quickly debugging your
code.

### Suggestions
Since both your code and tests are valid elisp, it is suggested to work with
your exercise code in a buffer pane side-by side with its test, like so:

![](/docs/img/dual-pane.png)

Split the frame vertically with `C-x 3`, switch to the new window with `C-x o`,
and open the file with `C-x C-f /path/to/file`.
