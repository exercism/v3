# Eccentric Party Robot

## Story

Once there was an eccentric programmer living in a strange house with barred windows.
One day he accepted a job from an online job board to build a party robot. The
robot is supposed to greet people and help them to their seats. The first addition
was very technical and showed the programmers lack of human interaction. Some of
which also made it into the final edition.

## Tasks

- Greet each person with:

```
Welcome to my party, <name>!
```

- A guest who persons birthday is today is greeted with in the following way to show off the robots knowledge of each guest:

```
Happy birthday <name>! You are now <age> years old!
Welcome to my party!
```

- Someone asking for their seat are given directions to their table with:

```
Welcome to my party, <name>!
You have been assigned to table <table-number-in-hex>. Your table is <direction>, exactly <distance-float> meters from here.
You will be sitting next to <neighbour-name>!
```

## Implementations

- [Go: strings][implementation-go] (reference implementation)

## Reference

- [`types/string`][types-string]

[types-string]: ../types/string.md
[implementation-go]: ../../languages/go/exercises/concept/strings/.docs/instructions.md
