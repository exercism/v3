# Freelancer Rates

## Story

In this exercise you'll be writing code to help a freelancer communicate with a
project manager by providing a few utilities to quickly calculate day- and
month rates, optionally with a given discount.

We first establish a few rules between the freelancer and the project manager:

- The daily rate is 8 times the hourly rate;
- A month has 22 billable days.

The freelancer is offering to apply a discount if the project manager chooses
to let the freelancer bill per month, which can come in handy if there is a
certain budget the project manager has to work with.

Discounts are modeled as fractional numbers followed by a `%` (percentage)
between `0.0%` (no discount) and `90.0%` (maximum discount).

## Tasks

These are example tasks that fit the story of the freelancer communicating with a project manager:

- Calculate the day rate given an hourly rate
- Calculate the month rate, given an hourly rate and a discount
- Calculate the number of workdays given a budget, rate and discount

## Terminology

These are recommendations, not rules, for recurring terminology in the instructions (including stub commentary)

- Daily rate is 8 times the hourly rate; it should be mentioned
- Monthly rate is 22 times the daily rate; it should be mentioned
- If discount is supplied as a _different_ type than the numeric types, use the terminology **discounts are modeled as**

## Implementations

- [Elixir: numbers][implementation-elixir]
- [JavaScript: numbers][implementation-javascript] (reference implementation)

## Reference

- [`types/number`][types-number]
- [`types/decimal_number`][types-decimal-number]
- [`types/floating_point_number`][types-floating-point-number]
- [`types/string`][types-string]

[types-number]: ../types/number.md
[types-decimal-number]: ../types/decimal_number.md
[types-floating-point-number]: ../types/floating_point_number.md
[types-string]: ../types/string.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/numbers/.docs/instructions.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/numbers/.docs/instructions.md
