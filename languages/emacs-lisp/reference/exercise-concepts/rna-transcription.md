# Basic

- require: in the template
- provide: in the template
- defun: represents the code
- docstring: not required, but part of the discussion with students
- naming standards in elisp

# Common

- string: represents input
- passing parameters to defuns
- calling other functions: usually one defines new functions
- mapconcat/mapcar/concat/dolist/: to process letters in a string
- lambdas: people usually do the transcoding in a lamba on a seq-map

# Control structures

- if: to detect if the code is invalid
- error: raising errors when invalid
- condition-case
- cond
- let(*): defining local variables

# Data structures

- char: to be able to process letters individually
- hash-table: to represent the char mapping
- assq/cdr
- alist
- add-to-list/split-string/concat
- =/char-equal/string-equal: comparing chars/strings
- upcase/downcase
