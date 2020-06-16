# Emacs Lisp Concept Exercise Progression

This is a working document to keep track of ideas and thoughts on how the progression through the Concept Exercises on the Emacs Lisp track could work.

First of all, a student should understand the basis of a Lisp language, illustrated on the concepts progression bellow:

```mermaid
graph LR
   S-Expressions --> Lists & Atoms & g2[Special Forms] & Variables & Macros
   Functions --> Evaluation & Arguments & g3[Side Effects]
   Error --> Symbol --> Atoms --> Numbers & Strings
   Atoms --> Booleans --> nil
   ConsCell --> Lists --> Quote --> Symbol
   Macros --> Functions
   Loops --> Conditionals --> Boolean --> Symbol
   Comment --> Functions
```

A student would then need to understand emacs specific concepts, illustrated bellow:

```mermaid
graph LR
   Error
   EIEIO
   Strings --> Chars
   Maps --> AList --> ConsCell --> Symbol
   Buffer --> Text --> Strings
   Rings
   SyntaxTables --> Buffer
   KeyBinding --> Chars
   Face --> Text
   Process
   DefCustom --> Constants
   MajorMode --> MinorMode --> KeyBinding & Hooks
   Lambda
   Advices
```
