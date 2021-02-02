# Concepts of flatten-array

## General

- method definition: the student must implement the `flatten` method
- method arguments (positional): `flatten` takes one argument
- return values: `flatten` should return an array
- class methods: one approach is the define a class `FlattenArray` with a class method `flatten`
- module methods: one approach is to implement `FlattenArray` as a module
- arrays: the input is an array, and student's should understand the basics of arrays
- array flatten: this method is the obvious/preferred way to flatten an array
- array compact: this method is the obvious/preferred way to get rid of the `nil`'s
- nil: the input array contains `nil` values, and the student should understand what `nil` is

## Approach: `flatten` as a class method

- declare `FlattenArray` to be a class 
- add `flatten` using `def self.flatten`
- add `flatten` using the `class << self` block syntax

## Approach: `flatten` as a module method

- declare `FlattenArray` to be a module
- add `flatten` using `def self.flatten`
- add `flatten` using `def flatten`, and then either `extend self` or call `module_function :flatten`

## Approach: add `flatten` as a singleton method to an instance of Object

- assign an instance of `Object` to `FlattenArray`, add `flatten as a singleton method`
