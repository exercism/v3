# Weighing machine

## Story

In this exercise you'll be modelling a weighing machine.

The weight can be set and retrieved in pounds or kilograms and cannot be negative.

The weight can be displayed in SI units or US units
, pounds and ounces.

A tare adjustment can be applied to the weight (for instance to deduct the
weight of a container). This can be any value (even negative or a value that makes the display weight negative)
as there are doubts about the accuracy
of the weighing machine. For security reasons this value cannot be retrieved.

Note that:

```
display-weight = input-weight - tare-adjustment
```

Conversion ratios are as follows:

- 16 ounces to a pound
- 2.20462 kg to a pound

For Example:

- 60 kilograms == 132.2772 ponds
- 132.2772 pounds == 132 pounds 4 ounces

## Tasks

These are example tasks that fit the weighing machine exercise:

- Allow the weight to be set on the weighing machine
- Ensure that a negative input weight is rejected
- Allow the US weight to be retrieved
- Allow the machine's units to be set to pounds
- Allow a tare adjustment to be applied to the weighing machine

## Implementations

- [C#: properties][implementation-csharp] (reference implementation)

[implementation-csharp]: ../../languages/csharp/exercises/concept/properties/.docs/instructions.md
