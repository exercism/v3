Common Lisp allows the programmer to write "comments" that are ignored by the computer. Single-line comments begin with one or more semi-colons (`;`) and, occasionally, you may see the following:

```lisp
(code...) ; => value
```

Where the comment is being used to indicate what value is returned by Common Lisp after running the code on that line.

It is idiomatic to use a single semi-colon for a short comment at the end of a line, two for a longer comment above a section of code, three for long comment describing something such as a function and four for a comment such as a header at the top of a source file.
