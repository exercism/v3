# How to implement a C# concept exercise

This document describes how to implement a concept exercise for the C# track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read the following documents:

- [The features of v3][docs-features-of-v3].
- [Rationale for v3][docs-rationale-for-v3].
- [What are concept exercises and how are they structured?][docs-concept-exercises]

Please also watch the following video:

- [The Anatomy of a Concept Exercise][anatomy-of-a-concept-exercise].

As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `AnonymousMethods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue. Having done this, please read the [C# concept exercises introduction][concept-exercises].

To implement a concept exercise, the following files must be created:

<pre>
languages
└── csharp
    └── exercises
        └── concept
            └── &lt;SLUG&gt;
                ├── .docs
                |   ├── after.md
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── source.md (required if there are third-party sources)
                ├── .meta
                |   |── config.json
                |   |── design.md
                |   └── Example.cs
                ├── &lt;NAME&gt;.cs
                ├── &lt;NAME&gt;.csproj
                └── &lt;NAME&gt;Tests.cs
</pre>

## Step 1: adding track-specific files

The track-specific files should be designed to help the student learn the exercise's concepts. The following C#-specific files must be created (not necessarily in this order):

### Create `<NAME>.cs` file

**Purpose:** Provide a stub implementation.

- The stub code implementation should compile. The only exception is for syntax that we want a student to define themselves, like class, enum or property syntax. In this case, insert a descriptive TODO comment instead of providing stub code (see [this example][todo]).
- Stub methods should throw a `NotImplementedException` which message contains the method to implement (see [this example][not-implemented]).
- The code should be as simple as possible.
- Only use language features introduced by the exercise's prerequisites (and their prerequisites, and so on).

For more information, [watch this video][video-stub-file] and check [this example stub file][stub-file].

### Create `<NAME>Tests.cs` file

**Purpose:** The test suite to verify a solution's correctness.

- [xUnit][xunit] is used as the test framework.
- Only use `Fact` tests; don't use `Theory` tests.
- All but the first test should be skipped by default (check [this example][skip-fact]).
- The tests should not use the examples from the `instructions.md` file.
- The code should be as simple as possible.
- Only use language features introduced by the exercise's prerequisites (and their prerequisites, and so on).

For more information, [watch this video][video-tests-file] and check [this example tests file][tests-file].

### Create `<NAME>.csproj` file

**Purpose:** The project file required to build the project and run the tests.

For more information, check [this example project file][project-file].

### Create `.meta/Example.cs` file

**Purpose:** The idiomatic example implementation that passes all the tests.

- The implementation must be _idiomatic_.
- The code should be as simple as possible.
- Only use language features introduced by the exercise's prerequisites (and their prerequisites, and so on).

For more information, [watch this video][video-example-file] and check [this example file][example-file].

## Step 2: adding common files

How to create the files common to all tracks is described in the [how to implement a concept exercise document][how-to-implement-a-concept-exercise].

## Step 3: update list of implemented exercises

- Add the exercise to the [list of implemented exercises][implemented-exercises].

## Step 4: format code

All C# code should be formatted using the [`dotnet format` tool][dotnet-format]. There are two ways to format your C# code:

#### 1. Using a GitHub comment

If you add a comment to a GitHub PR that contains the text `/dotnet-format`, a GitHub workflow will format all C# documents in the PR using `dotnet format`. Any formatting changes made by `dotnet format` will automatically be committed to the PR's branch. This also works for forks that have [enabled maintainers to edit the fork's PR][allowing-fork-pr-changes] (which is the default).

#### 2. Using a script

Open a command prompt in the `language/csharp` directory and then run:

- `./format.ps1 <SLUG>`, where `<SLUG>` is the exercise's directory name.

## Step 5: add analyzer (optional)

Some exercises could benefit from having an exercise-specific [analyzer][analyzer]. If so, specify what analysis rules should be applied to this exercise and why.

_Skip this step if you're not sure what to do._

## Step 6: add representation (optional)

Some exercises could benefit from having an custom representation as generated by the [C# representer][representer]. If so, specify what changes to the representation should be applied and why.

_Skip this step if you're not sure what to do._

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented C# exercises like the [strings][concept-exercise-strings], [datetimes][concept-exercise-datetimes] or [floating-point numbers][concept-exercise-numbers-floating-point] exercises. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that concept.

## Help

If you have any questions regarding implementing the exercise, please post them as comments in the exercise's GitHub issue.

[analyzer]: https://github.com/exercism/csharp-analyzer
[representer]: https://github.com/exercism/csharp-representer
[concept-exercises]: ../exercises/concept/README.md
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[docs-concept-exercises]: ../../../docs/concept-exercises.md
[docs-rationale-for-v3]: ../../../docs/rationale-for-v3.md
[docs-features-of-v3]: ../../../docs/features-of-v3.md
[anatomy-of-a-concept-exercise]: https://www.youtube.com/watch?v=gkbBqd7hPrA
[concept-exercise-strings]: ../exercises/concept/strings
[concept-exercise-datetimes]: ../exercises/concept/datetimes
[concept-exercise-numbers-floating-point]: ../exercises/concept/floating-point-numbers
[reference]: ../../../reference
[dotnet-format]: https://github.com/dotnet/format
[allowing-fork-pr-changes]: https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/allowing-changes-to-a-pull-request-branch-created-from-a-fork
[implemented-exercises]: ../exercises/concept/README.md#implemented-exercises
[video-stub-file]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=1171
[video-tests-file]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=1255
[video-example-file]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=781
[stub-file]: ../exercises/concept/strings/Strings.cs
[tests-file]: ../exercises/concept/strings/StringsTests.cs
[example-file]: ../exercises/concept/strings/.meta/Example.cs
[project-file]: ../exercises/concept/strings/Strings.csproj
[skip-fact]: ../exercises/concept/strings/StringsTests.cs#L11
[xunit]: https://xunit.net/
[not-implemented]: ../exercises/concept/strings/Strings.cs#L5
[todo]: ../exercises/concept/basics/Basics.cs
