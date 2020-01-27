# Concepts of nucleotide-count

## General

- class level methods: used as the starting point of the exercise. They may be named with `self.[method_name]` notation or with a `self` block. Another way of creating class methods is extending a module.
- module methods: depending on the approach, they may be used instead of the class methods.
- factory pattern and object creation: the method `from_dna` may be used to create instances of the main class.
- instance variables: store the common value/state for the instance.
- accessors: `att_reader` may be used to access instance variables.
- access modifiers: `private` may be used to restrict access to the readers.
- method definition: the `histogram` method needs to be defined.
- method arguments: the method `from_dna` needs an argument.
- string: one string is passed as the input.
- `chars`: may be used to convert the string into an array of characters.
- strings mutation: `freeze` may be used to prevent string constants from mutating. The directive `frozen_string_literal: true` may also be used for the same purpose.
- Enumerable module: including `Enumerable` and implementing `each` is one of the possible approaches to this exercise.
- `Enumerable#count`: one of the possible ways of counting nucleotides.
- `Enumerable#inject` vs `Enumerable#each_with_object`.
- exceptions: necessary to raise an exception if the input is not valid.
- return values: methods need to return values either implicitly or explicitly.
- guard clauses: a guard clause may be used to raise an exception.
- constants: constants may be used to store static information like DNA values.
- namespaces: namespaces may be used along with constants.
- conditionals: a condition may be used to check if the input is valid.
- yield: in case `Enumerable` is included the `each` method has to use `yield`.
- blocks: they are used in any of the Enumerable methods.
- Ruby magic comments: `frozen_string_literal: true` may be used.
- array difference: may be use to validate the input

## Approach: use Enumerable mixing

- Make the class that represents the nucleotides strand to include `Enumerable` so that all `Enumerable` methods are available.
- Requires implementation of `each` method. See [Enumerable mixing documentation](https://ruby-doc.org/core-2.7.0/Enumerable.html)

## Approach: Use collections + Enumerable methods.

- Use `Enumerable#count` to count nucleotide occurrences.
- Use `String#chars` and `Enumerable#each_with_object` to create the histogram

## Approach: Use indexes to loop over the chars.

- Similar to the previous approach but using indexes and loops to iterate over an `Array` of characters.

## Approach: Use collections + String#count and Regex.

- Use `String#count` to count nucleotide occurrences.
- Initialize a hash for the histogram then use `Enumerable#each` and `String#count` to fill it.
- Use Regex to validate the input.

## Approach: Use collections + String#count.
- Similar to the previous approach but using `String#count` and `String#size` to validate the input.
- For the input validation part it's also possible to use array difference to remove nucleotides from the input resulting in an empty collection for the valid inputs.

