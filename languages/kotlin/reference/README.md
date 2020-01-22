# Kotlin reference

## Draft Status!

```diff
- WARNING! This document should be treated as a draft and will be actively changing. Sections that marked as WIP requires attention/rework from contributors.
```

## Concepts

Kotlin concept exercises are based on concepts. They are (mostly) connected with the corresponding parts of [official Kotlin reference](https://kotlinlang.org/docs/reference/basic-syntax.html#defining-packages).

## Misc

- [Idioms](https://kotlinlang.org/docs/reference/idioms.html)
- [Coding conventions](https://kotlinlang.org/docs/reference/coding-conventions.html)
- [Reading/Searching documentation](https://kotlinlang.org/docs/reference/basic-syntax.html)

## Program Structure

- [Program entry point](https://kotlinlang.org/docs/reference/basic-syntax.html#program-entry-point)
- Project structure
- (Tooling) Gradle
- [Packages](https://kotlinlang.org/docs/reference/packages.html)

## Classes and Objects

- [Basic Classes](https://kotlinlang.org/docs/reference/classes.html#classes)
- [Primary Contstructor](https://kotlinlang.org/docs/reference/classes.html#constructors)
- [Secondary Constructors](https://kotlinlang.org/docs/reference/classes.html#constructors)
- [Instantiating Class](https://kotlinlang.org/docs/reference/classes.html#constructors)
- [Properties and Fields](https://kotlinlang.org/docs/reference/properties.html#declaring-properties)
- [Getters and Setters](https://kotlinlang.org/docs/reference/properties.html#getters-and-setters)
- [Backing Fields](https://kotlinlang.org/docs/reference/properties.html#backing-fields) and [Backing Properties](https://kotlinlang.org/docs/reference/properties.html#backing-properties)
- [Compile-Time Constants](https://kotlinlang.org/docs/reference/properties.html#compile-time-constants)
- [Late-Initialized Properties and Variables](https://kotlinlang.org/docs/reference/properties.html#late-initialized-properties-and-variables)
- [Overriding Properties](https://kotlinlang.org/docs/reference/classes.html#overriding-properties)
- [Delegated Properties](https://kotlinlang.org/docs/reference/delegated-properties.html)
- [Inheritance](https://kotlinlang.org/docs/reference/classes.html#inheritance)
- [Interfaces](https://kotlinlang.org/docs/reference/interfaces.html#interfaces)
- [Objects](https://kotlinlang.org/docs/reference/object-declarations.html#object-expressions-and-declarations)
- [Companion object](https://kotlinlang.org/docs/reference/object-declarations.html#companion-objects)
- [Visibility modifiers](https://kotlinlang.org/docs/reference/visibility-modifiers.html#visibility-modifiers)
- [Extensions](https://kotlinlang.org/docs/reference/extensions.html#extensions)
- [Data Classes](https://kotlinlang.org/docs/reference/data-classes.html#data-classes)
- [Sealed Classes](https://kotlinlang.org/docs/reference/sealed-classes.html)
- [Genertics](https://kotlinlang.org/docs/reference/generics.html#generics)
- [Nested Classes](https://kotlinlang.org/docs/reference/nested-classes.html)
- [Enum Classes](https://kotlinlang.org/docs/reference/enum-classes.html)
- [Type aliases](https://kotlinlang.org/docs/reference/type-aliases.html)
- [Inline Classes](https://kotlinlang.org/docs/reference/inline-classes.html)
- [Delegation](https://kotlinlang.org/docs/reference/delegation.html#delegation)

## Types

- [Numbers](https://kotlinlang.org/docs/reference/basic-types.html#numbers): Integer and Decimal
- [BigInteger](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-integer/index.html) and [BigDecimal](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-integer/to-big-decimal.html)
- [Characters](https://kotlinlang.org/docs/reference/basic-types.html#characters)
- [Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [String Templates](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [Booleans](https://kotlinlang.org/docs/reference/basic-types.html#booleans)
- [Arrays](https://kotlinlang.org/docs/reference/basic-types.html#arrays)
- [Unsigned Integers](https://kotlinlang.org/docs/reference/basic-types.html#unsigned-integers)

## Collections

Also see [constructing](https://kotlinlang.org/docs/reference/constructing-collections.html#constructing-from-elements) and [empty collections](https://kotlinlang.org/docs/reference/constructing-collections.html#empty-collections).

- [Collections Overview](https://kotlinlang.org/docs/reference/collections-overview.html)
- [Lists](https://kotlinlang.org/docs/reference/collections-overview.html#list)
- [Sets](https://kotlinlang.org/docs/reference/collections-overview.html#set)
- [Maps](https://kotlinlang.org/docs/reference/collections-overview.html#map)
- [Iterators](https://kotlinlang.org/docs/reference/iterators.html#iterators)
- [Ranges](https://kotlinlang.org/docs/reference/ranges.html)
- [Sequences](https://kotlinlang.org/docs/reference/sequences.html#sequences)
- [Operations overview](https://kotlinlang.org/docs/reference/collection-operations.html)
- [Single-Element Retrieving](https://kotlinlang.org/docs/reference/collection-elements.html) and [Index Access](https://kotlinlang.org/docs/reference/list-operations.html#retrieving-elements-by-index)
- [Adding / Substracting](https://kotlinlang.org/docs/reference/collection-plus-minus.html) and [Modification](https://kotlinlang.org/docs/reference/collection-write.html) and [List-specific](https://kotlinlang.org/docs/reference/list-operations.html#list-write-operations)
- [Mapping](https://kotlinlang.org/docs/reference/collection-transformations.html#mapping)
- [Zipping](https://kotlinlang.org/docs/reference/collection-transformations.html#zipping)
- [Association](https://kotlinlang.org/docs/reference/collection-transformations.html#association)
- [Flattening](https://kotlinlang.org/docs/reference/collection-transformations.html#flattening)
- [String Representation](https://kotlinlang.org/docs/reference/collection-transformations.html#string-representation)
- [Filtering](https://kotlinlang.org/docs/reference/collection-filtering.html)
- [Partitioning](https://kotlinlang.org/docs/reference/collection-filtering.html#partitioning)
- [Grouping](https://kotlinlang.org/docs/reference/collection-grouping.html)
- [Slicing](https://kotlinlang.org/docs/reference/collection-parts.html#slice)
- [Take and Drop](https://kotlinlang.org/docs/reference/collection-parts.html#take-and-drop)
- [Chunking](https://kotlinlang.org/docs/reference/collection-parts.html#chunked)
- [Windowing](https://kotlinlang.org/docs/reference/collection-parts.html#windowed)
- [Ordering](https://kotlinlang.org/docs/reference/collection-ordering.html)
- [Aggregate Operations](https://kotlinlang.org/docs/reference/collection-aggregate.html)
- [Fold / Reduce](https://kotlinlang.org/docs/reference/collection-aggregate.html#fold-and-reduce)
- [Sub-Lists](https://kotlinlang.org/docs/reference/list-operations.html#retrieving-list-parts)
- [Search](https://kotlinlang.org/docs/reference/list-operations.html#finding-element-positions)
- [Sorting](https://kotlinlang.org/docs/reference/list-operations.html#sorting)
- [Set operations](https://kotlinlang.org/docs/reference/set-operations.html)
- [Map operations](https://kotlinlang.org/docs/reference/map-operations.html)

## Control Flow

```diff
! WIP
```

- if
- when
- for
- while
- break/continue  

## Functions and Lamdas

```diff
! WIP
```

- Functions
- Lambdas
- Inline functions
- vararg

## Other

```diff
! WIP
```

- Destructuring Declarations
- Type Checks and Casts
- This
- Equality
- Operator overloading
- Null Safety
- Exceptions
- Annotations
- Reflection
- Scope Functions
- Type-Safe Builders
- Java interop
- Testing

## Coroutines

```diff
! WIP
```

- Basics
- Cancellation and Timeout
- Composing Suspending Functions
- Coroutine Context and Dispatchers
- Async Flow
- Channels
- Exception Handling and Supervision
- Shared Mutable State and Concurrency

## Kotlin Multiplatform

```diff
! WIP
```

## Concept interpretation

```diff
! WIP
```

The concept exercises use the following concepts:

| concept | interpretation |
|
| --- | --- |

## Reference docs

```diff
! WIP
```

Reference docs are written to help explain a particular Kotlin concept to a student as an addition to other docs available onlin. They will be used when creating exercises and as references in exercise documentation.

See:
  - 
