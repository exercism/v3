# CLI

## Setup

The simplest way to install Purescript is via Node.js and NPM.

If you don't already have Node.js installed on your computer, you can download it from the [Node.js website][nodejs-website]. Once you have Node.js up and running, enter this command into the terminal to install the PureScript compiler and the Spago package manager/build tool.

```shell
npm install -g purescript spago
```

More information can be found in the [PureScript repository][purescript-repository].

## Requirements

Install assignment dependencies:

```shell
spago install
spago build
```

## Making the test suite pass

Execute the tests with:

```shell
spago test
```

In the test suites all tests but the first have been skipped. This is to encourage you to solve the exercise one step at a time.

Once you get a test passing, you can enable the next one by changing `xit` to `it`.

## Submitting the solution

Once none of the tests are skipped and they are all passing, you can submit your solution with:

```shell
exercism submit src/<NAME>.purs
```

## Writing custom tests

If you wish to write additional, custom, tests, create a new file `test/Custom.purs`, and submit it with your solution together with the new file:

```shell
exercism submit src/<NAME>.purs test/Custom.purs
```

## Links

- [Exercism CLI documentation][docs-exercism-cli]

[docs-exercism-cli]: https://exercism.io/cli
[purescript-repository]: https://github.com/purescript/purescript/blob/master/INSTALL.md
[nodejs-website]: https://nodejs.org/
