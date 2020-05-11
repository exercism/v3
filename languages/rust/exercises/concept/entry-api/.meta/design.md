# Design

## Goal

The goal of this exercise is to introduce the student to the [Entry API](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html) on `HashMap`s/`BTreeMap`s in Rust, how to use it to fetch an `Entry` from a `HashMap`, as well as use the methods on `Entry` to manipulate it.

## Learning objectives

Students who complete this exercise should be able to...

- spot appropriate use-cases/contexts in which usage of the entry API is appropriate
- use the entry API method `and_modify` to update a fetched `Entry`
- use the entry API method `or_insert` to ensure an `Entry` is not `Vacant` when fetching it
- use multiple entry API methods in conjunction with each other to update `Entry`s, such as `entry().and_modify().or_insert()`

This exercise may review, but is not expected to include, other usages of `HashMap`s.

## Out of scope

Students who complete this exercise should not necessarily be able to...

- use the entry API `insert` method, as it's currently only in nightly
- use the entry API `or_default` method, as this is less commonly used than `or_insert`
- use the entry API `or_insert_with` method, as this is less commonly used than `or_insert`

## Concepts

- `Entry::and_modify`
- `Entry::or_insert`
- Using the above methods in conjunction with one another

## Prerequisites

Completion of the `HashMap`s concept exercise(s).

## Resources to refer to

The [HashMap::Entry](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html) docs are a great resource to refer to for more information on the entry API.

### Hints

- The [HashMap::Entry](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html) docs

### After

- The [HashMap::Entry](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html) docs detail additional `Entry` methods such as `or_default` and `or_insert_with` that students can dig into further.

### Representer

No specific changes expected.

### Analyzer

No specific changes expected.
