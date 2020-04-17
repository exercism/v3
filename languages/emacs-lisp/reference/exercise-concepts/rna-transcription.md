# Required

- require: in the template
- provide: in the template
- defun: represents the code
- returning values from functions
- passing parameters to defuns
- string: represents input

# Common

- lists: a string is a list
- calling other functions: usually one defines new functions
- mapconcat/mapcar/concat/dolist/: to process letters in a string
- lambdas: people usually do the transcoding in a lamba on a seq-map

# Style

- cl-labels: defining functions inline
- naming conventions: how to name your functions properly (commonly used pefixes and suffixes)
- docstring: not required, but part of the discussion with students
- comments: not required, but part of the discussion when they are off
- naming standards in elisp

# Control structures

- if: to detect if the code is invalid
- error/condition-case: raising and catching errors when invalid
- cond: to handle each case
- let(\*): defining local variables

# Data structures: String

- char: to be able to process letters individually
- alist
- add-to-list/split-string/concat
- =/char-equal/string-equal: comparing chars/strings
- upcase/downcase

# Data structures: Hash

- hash-table: to represent the char mapping
- assq/cdr
