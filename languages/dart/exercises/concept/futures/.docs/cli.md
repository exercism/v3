# CLI

## Setup

Go through the setup instructions for Dart to install the necessary
dependencies:

[https://exercism.io/tracks/dart/installation][docs-exercism-dart]

## Requirements

Install assignment dependencies:

```shell
pub get
```

## Making the test suite pass

Execute the tests with:

```bash
pub run test

```

In the test suites all tests but the first have been skipped. This is to
encourage you to solve the exercise one step at a time.

Once you get a test passing, you can enable the next one by changing `skip: true` to
`skip: false`; or removing `skip: true`.

## Submitting the solution

Once none of the tests are skipped and they are all passing, you can submit
your solution using `exercism submit futures.dart`.

## Writing custom tests

If you wish to write additional, custom, tests, create a new file
`custom_test.dart`, and submit it with your solution together with the new file:

```shell
exercism submit futures.dart custom_test.dart
```

## Links

- [exercism CLI documentation][docs-exercism-cli]

[docs-exercism-cli]: https://exercism.io/cli
[docs-exercism-dart]: https://exercism.io/tracks/dart/installation
