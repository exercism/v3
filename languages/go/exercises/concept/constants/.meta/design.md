## Goal

The goal of this exercise is to teach the student about constants in Go.

## Learning objectives

- Understand the purpose of `const`.
- Know when to define `const` values.
- Know how to define `const` values.
- Know how many types of `const` exist.

## Out of scope

## Concepts

- `const`
- `iota`

## Prerequisites

- `basics`
- `comments`

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise could benefit from the following rules added to the [analyzer][analyzer]:

- Verify that the `FixedInterestRate` constant is created with the `const` keyword and without an explicit type.
- Verify that the `GetFixedInterestRate()` function returns the `FixedInterestRate` constant.
- Verify that the `DaysPerYear` constant is created with the `const` keyword and with the explicit type `int`.
- Verify that the `GetDaysPerYear()` function returns the `DaysPerYear` constant.
- Verify that the `Jan`, `Feb`, `Mar`, `Apr`, `May`, `Jun`, `Jul`, `Aug`, `Sep`, `Oct`, `Nov`, and `Dec` constants are created in a block with the `const` keyword, using the `iota` identifier, and without an explicit type.
- Verify that the `GetJanuary()` function returns the `Jan` constant.
- Verify that the `GetOctober()` function returns the `Oct` constant.
- Verify that the `AccountNo` constant is created with the `const` keyword and without an explicit type.
- Verify that the `GetAccountNumber()` function returns the `AccountNo` constant.
- Verify that the `var` keyword is not used.
- Verify that the `iota` keyword is used exactly once.

[analyzer]: https://github.com/exercism/go-analyzer
[representer]: https://github.com/exercism/go-representer
