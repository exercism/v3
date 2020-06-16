# Slice

## The Concept

A slice is a descriptor for a contiguous segment of an underlying [array][type-array] and provides access to a
numbered sequence of elements from that array. A slice type denotes the set of all slices of arrays of its
element type. The number of elements is called the length of the slice and is never negative. The value of
an uninitialized slice is nil.

## What to cover

TBA

## Exercises

### Log Lines

TBA

#### Implementations

| Track | Exercise                   | Changes |
| ----- | -------------------------- | ------- |
| Go    | [slice][implementation-go] | None    |

[type-array]: ./array.md
[implementation-go]: ../../languages/go/exercises/concept/slices/.docs/introduction.md
