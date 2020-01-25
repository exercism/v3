# Concepts of nucleotide-count

## General

- class methods: used as the starting point of the exercise. They can be named with `self.[method_name]` notation or with a `self` block.
- module methods: depending on the approach, they can be used instead of the class methods.
- factory pattern and object creation: the method `from_dna` can be used to create instances of the main class.
- instance variables: @variables are needed to store the common value/state for all object methods.
- accessors: `att_reader` can be used to access instance variables.
- access modifiers: `private` can be used to restrict access to the readers.
- method definition: the `histogram` method needs to be defined.
- method arguments: the method `from_dna` needs an argument.
- string: one string is passed as the input.
- chars: it's necessary to convert the string into an array of chars.
- strings mutation: `freeze` can be used to prevent string constants from mutating.
- Enumerable module: including `Enumerable` and implement `each` is one of the possible approaches to this exercise.
- Enumerable#count: one of the possible ways of counting nucleotides.
- Enumerable#inject vs Enumerable#each_with_object.
- exceptions: necessary to raise an exception if the input is not valid.
- return values: methods need to return values either implicitly or explicitly.
- guard clauses: a guard clause can be used to raise an exception.
- constants: constants can be used to store static information like DNA values.
- namespaces: namespaces can be used along with constants.
- if conditions: an if condition can be used to check if the input is valid.
- yield: in case `Enumerable` is included the `each` method has to use `yield`.
- blocks: they are used in any of the Enumerable methods.
- Ruby magic comments: `frozen_string_literal` can be used.
- require: use to import test library and `nucleotide_count` file

## Approach: use Enumerable mixing

- Make the class that represents the nucleotides strand to include Enumerable so that all `Enumerable` methods are available.
- Requires implementation of `each` method. See [Enumerable mixing documentation](https://ruby-doc.org/core-2.7.0/Enumerable.html)

## Approach: Use collections + Enumerable methods.

- Use `Enumerable#count` to count nucleotide occurrences.
- Use `String#chars` and `Enumerable#each_with_object` to create the histogram


## Approach: Use indexes to loop over the chars.

- Similar to the previous approach but using indexes and loops to iterate over the chars array

