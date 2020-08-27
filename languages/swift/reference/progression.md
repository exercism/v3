# Swift Concept Exercise Progression

This is a working document to keep track of ideas and thoughts on how the progression through the Concept Exercises on the Swift track could work.

## Core concepts that every student should definitely know after completing the track

- Value types v. Reference types
- Structs v. Classes
- Higher-order functions
- Protocol-oriented programming

## Progression Tree

```mermaid
graph TD

Start((basics)) --> numbers
Start((basics)) --> strings-and-characters
Start((basics)) --> tuples

numbers --> optionals
numbers --> bit-manipulation
strings-and-characters --> conditionals
tuples --> optionals
tuples --> functions
tuples --> loops

loops --> map-and-flatmap
loops --> reduce

map-and-flatmap --> filter-and-compactmap

conditionals --> optionals
conditionals --> arrays
conditionals --> ranges

optionals --> string-components
optionals --> dictionaries

arrays --> string-components
arrays --> functions
arrays --> sets

functions --> dictionaries
functions --> higher-order-functions
functions --> structs-and-classes

higher-order-functions --> closures

closures --> properties
closures --> generics

structs-and-classes --> enums

enums --> pattern-matching
enums --> methods
enums --> properties

generics --> map-and-flatmap
generics --> filter-and-compactmap
generics --> reduce
generics --> extensions
generics --> phantom-types

properties --> inheritance
properties --> initialization
properties --> extensions

methods --> inheritance
methods --> initialization
methods --> extensions
methods --> subscripts

initialization --> deinitialization
initialization --> protocols

extensions --> protocol-extensions

protocols --> protocol-extensions
protocols --> comparable-and-equatable
protocols --> sequences
protocols --> codeable

comparable-and-equatable --> hashable
sequences --> collections
```
