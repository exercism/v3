# How to implement an Emacs Lisp concept exercise

To implement an Emacs Lisp concept exercise, the following files must be added:

```
languages
└── emacs-lisp
    ├── concepts
    |   └── &lt;CONCEPT_SLUG&gt;
    |       ├── about.md
    |       └── links.json
    └── exercises
        └── concept
            └── <SLUG>
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   └── hints.md
                ├── .meta
                |   |── design.md
                |   └── Example.el
                ├── <NAME>.el
                └── <NAME>_test.el
```
