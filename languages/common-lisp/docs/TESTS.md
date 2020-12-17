Start up your Lisp implementation in the directory of the exercise you
are working on (or change the current directory for an already running
Lisp implementation to that directory).

Load the test file into your running Lisp implementation, for example,
`(load "point-mutations-test")`. This will run the tests the first
time automatically. After that you can run the test suite in the REPL
with `(lisp-unit:run-tests :all :point-mutations-test)`.

## Making your first Common Lisp solution

To create lisp code that can be loaded with `(load "hamming")`
for the first exercise, put this code in `hamming.lisp`:

```lisp
(defpackage #:hamming
  (:use #:cl)
  (:export #:distance))

(in-package #:hamming)

(defun distance (dna1 dna2)
  "Number of positional differences in two equal length dna strands."
  ;;; your solution here
  )
```

## How to run the tests

If you have a running Lisp implementation you can load the test file 
with the command: `(load "hamming-tests")`.

If instead you'd like to do it from the command line the command you
need to run depends upon the implementation. Please refer to the
documentation for your implementation. Here we'll give examples of how
to do it in two common implementations: 
[Clisp](https://clisp.sourceforge.io) & [SBCL](http://www.sbcl.org).

It is important to make sure that the command you run will load the
implementation's init file, which is needed to ensure QuickLisp is
loaded, then loads the test file, then exits back the command line.

### Clisp

Either of the following commands will work:

```
clisp -i ~/.clisprc.lisp hamming-test.lisp
```

```
clisp -i hamming-test.lisp -x '(ext:exit)'
```

### SBCL

```
sbcl --load hamming-test.lisp --quit
```

