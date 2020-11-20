# RNA & DNA Processing

## Story

In biology libraries, Nucleic Acids may be encoded as 4-bit types (see for example [BioSymbols.jl](https://github.com/BioJulia/BioSymbols.jl/blob/master/src/nucleicacid.jl)).
This can be used as a context for an exercise introducing primitive types, or other concepts related to bit manipulation.

The v2 exercises `hamming`, `rna-transcription` and `nucleotide-count` could be reused but with primitive types instead of [`strings`][types-string]/[`chars`][types-char].
You could consider adding those exercises twice (perhaps as practice exercise) in the progression path, once with [`strings`][types-string]/[`chars`][types-char] and then later on with primitive types.

## Implementations

- [Elixir: bitstrings][implementation-elixir]

## Reference

- [`types/char`][types-char]
- [`types/string`][types-string]

[implementation-elixir]: ../../languages/elixir/exercises/concept/dna-encoding/.docs/instructions.md
[types-string]: ../types/string.md
[types-char]: ../types/char.md
