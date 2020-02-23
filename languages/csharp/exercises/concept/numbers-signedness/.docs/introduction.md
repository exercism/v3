# Introduction

Number types get a fixed amount of memory assigned, therefore they can represent a fixed range of numbers. A signed type can represent both positive and negative numbers, unsigned types can only represent positive numbers. A unsigned type doesn't need a bit to represent if a number is positive or negative so it can use that bit to represent a bigger range of positive numbers.

Unsigned types can represent double the range of positive numbers than their signed counter parts. For example the type `short` is signed and can represent -32,768 to 32,767 the unsigned type `ushort` can represent 0 to 65,535. Only integral numeric types have unsigned variants. Because of the nature of floating-point numeric types of they can already represent very large numbers.

## Arithmetic overflow

When the result of a calculation does not fit in the range a number type can represent arithmetic overflow happens. Arithmetic overflow can cause a program to crash or have unexpected results.
