# Introduction

The C# `enum` type represents a fixed set of named constants (an enumeration). Normally, one can only refer to exactly one of those named constants. However, sometimes it is useful to refer to more than one constant. To do so, one can mark the `enum` as one which constants are _flags_. By carefully assigning the values of each constant, one can use bitwise operators to add or remove references to one or more of the (flag) constants.

By default, an `enum` is stored as an `int`, but it is possible to use any C# integer type as an enum's backing type.
