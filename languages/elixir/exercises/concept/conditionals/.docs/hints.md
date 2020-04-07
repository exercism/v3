### General

- The [atom type is described here][atom]

### 1. Return the logging code label

- You can use a [`cond/1` function][cond] to elegantly handle the various log codes.

### 2. Support unknown logging codes

- There is a [way to specify a default branch][cond] in a cond function that can be used to catch unspecified cases.

[atom]: https://elixir-lang.org/getting-started/basic-types.html#atoms
[cond]: https://elixir-lang.org/getting-started/case-cond-and-if.html#cond
