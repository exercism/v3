# Emacs Lisp Concept Exercise Progression

This is a working document to keep track of ideas and thoughts on how the progression through the Concept Exercises on the Emacs Lisp track could work.

```mermaid
graph LR
   S-Expressions --> Lists & Atoms & g2[Special Forms] & Variables & Macros
   Lists --> Functions & Quote
   Functions --> Evaluation & Arguments & g3[Side Effects]
   Atoms --> Numbers & Strings
   Macros --> Functions
   Side-effects --> Variables
   Arguments --> Functions
   Docstrings --> Functions
   Defcustom --> Functions & S-Expressions & Variables
   Scoping --> Variables
   Autoloading --> Functions
   Byte compilation --> Functions
   Hooks -> Functions
   Minor modes -> Functions & Hooks & g2[Key bindings]
   Major modes -> Minor modes
   Advices -> Functions
```
