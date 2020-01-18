# Code style

While the Factor parser doesn't enforce a particular code style, there are general [conventions](article-conventions) for terminology and word names.

Words, classes, constants, and so on are usually in `kebab-case`, except names from the C and C++ [`alien`][vocab-alien] interface, which obviously cannot be `kebab-case`.

Rarely, words are defined with `PascalCase`, `camelCase`, or `UPPERCASE` names, usually to mirror domain-specific naming conventions. For example, the [`cocoa.types`][vocab-cocoa.types] vocabulary defines words such as `NSNotFound` and `NSFastEnumerationState`. The [`->`][hyphen-gt] word is implemented by the [`cocoa`][vocab-cocoa] vocabulary to look like Objective C, and by the [`graphviz`][vocab-graphviz] vocabulary to implement `add-edge` while looking like the Graphviz DOT language.

## Formatting conventions

Factor formatting conventions are mostly derived from the Factor implementation's code formatting. Specifically,

- Lines following the first line of a definition are indented by either 2 or 4 spaces (4 for the Factor codebase), except the bodies of `HELP:`, `ARTICLE:` and `ABOUT:` help definitions and `USING:` syntax, which are not indented:
  ```factor
  USING: accessors arrays combinators kernel locals math
  math.order sequences sequences.private vectors ;
  : x ( y z -- a )
      [ / ] [ + ] bi * ;

  CONSTANT: some-data {
      1
      2
  }

  HELP: x
  { $values }
  { $description "Multiply the sum of the inputs by their ratio" } ;

  ARTICLE: "example-article" "An example article."
  "An article." ;

  ABOUT: "code-style"
  ```

- The indentation level grows each time the contents of a quotation or other literal appear on a line other than the line on which it started:
  ```factor
  [ a quotation ] [
      another quotation
      {
          1
          2
          3
      } some words
  ]
  ```

- A Factor program or script file should typically have the following overall layout (`USING` and imports, `IN`, `CONSTANT` and static definitions, private word definitions, public interface words, `MAIN`):
  ```factor
  USING: accessors arrays combinators kernel locals math
  math.order sequences sequences.private vectors ;
  FROM: math => + ;

  IN: code-style
  CONSTANT: x "x"

  ! the main private implementation part of the file
  <PRIVATE
  : (private-word) ( -- )
      ;
  PRIVATE>

  : uses-private-word ( -- )
      (private-word) ;

  ! an "inner" private section, for more complex file layouts
  <PRIVATE
  : another-word ( -- ) ;
  PRIVATE>

  : vocab-main ( -- )
      ! C-like main
      ;

  MAIN: vocab-main
  ```

There are no auto-formatting tools for Factor yet, as it's pretty manageable, but that does not preclude it as a future possibility.

[article-conventions]: https://docs.factorcode.org/content/article-conventions.html
[hyphen-gt]: https://docs.factorcode.org/content/word--__gt__,graphviz.notation.html
[vocab-cocoa]: https://docs.factorcode.org/content/vocab-cocoa.html
[vocab-cocoa.types]: https://docs.factorcode.org/content/vocab-cocoa.types.html
[vocab-graphviz]: https://docs.factorcode.org/content/vocab-graphviz.html
[vocab-alien]: https://docs.factorcode.org/content/vocab-graphviz.html
