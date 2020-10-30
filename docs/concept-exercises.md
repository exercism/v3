# Concept Exercises

Concept Exercises replace V2's Core Exercises. They are exercises designed to teach specific (programming) concepts.

## What do we mean by concepts?

By concepts we mean things that a programmer would need to understand to be fluent in a language. We care specifically about how languages are **different**. What do I need to learn differently about numbers in Haskell to numbers in Ruby to be able to work with numbers in those languages. Two questions that we have felt useful to ask to establish this are:

- If someone learnt Ruby, and someone learnt Haskell, what are the things that the two people learnt that are different?
- If a Ruby programmer learnt Haskell, what new concepts would they have to learn, what knowledge would they have to disregard, and what syntax would they have to remap?

By teaching concepts we aim to teach fluency.

## What do we mean by "fluency?"

By "Fluency", we mean: Do you **get** that language? Can you reason in that language? Do you write that language like a "native" programmer writes in it? Fluency is the ability to express oneself articulately in a particular language.

"Fluency" is different to "Proficiency", where we use proficiency to mean: Can you write programs in the language (e.g. can you nav stdlib, docs, compose complex code structures).

Exercism focuses on teaching Fluency not Proficiency. We aim to teach people to understand what makes a language unique and how experienced programmers in that language would reason about - and solve - problems.

## How are Concept Exercises designed and structured?

Concept Exercises must have the following characteristics:

- Each one has a clear learning goal.
- They are language-specific, not generic.
- Stubs/boilerplate are used to avoid the student having to learn/write unnecessary code on exercises.

Exercises are unlocked based on concepts taught and learnt. Each Concept Exercise must teach one or more concepts. It may also have prerequisites on Concepts, which means it will not be unlocked until Concept Exercises teaching those prerequisite concepts have been completed.

Concept Exercises should not inherently become more difficult as the track progresses. **A seasoned developer in Language X should be able to work through all the Concept Exercises on that track spending no more than 5-10 minutes solving each one.** As each exercise should be focussed on getting someone to use a concept for the first time, and because the seasoned developer already understands that concept in Language X, the exercise should feel relatively trivial for them. Later exercises may feel more difficult to a developer unfamiliar with Language X, but only because the later exercise is teaching a concept which in itself is more complicated (for example, most people would agree Recursion is a more complex topic to learn for the first time, than a Loop is to remap from one language to another).

Concept Exercises are **not** mentored. When a student submits a submission that gets the tests passing for a Concept Exercise, we check for an Analyzer or Representer to give feedback. If none is found, then the solution is approved. This shifts the burden of teaching to the exercise, which must provide a clear pathway to learning the concept that is being taught.

Concept Exercises do not share a common base like Practice Exercises do in the `problem-specifications` repository. Instead they "share" Concepts that they are teaching with other languages. This repository aims to list all of those Concepts and provide information about the Concept that maintainers can use as the basis for their own languages. Each Concept should also link to the implementations in different languages. Maintainers are free to copy and paste from each others repositories, and then edit to make things specific to their tracks, but such copy-and-pastes should be considered hard-forks.

For example, we might define a concept of "Classes" and provide a short introduction that explains what a class is, how it fits with objects, state, etc. We might include a link to a good article introducing OOP and classes. Individual tracks implementing an exercise on Classes can then include this introductory text, making any changes or additions that explain their language-specific semantics and syntax.

## Design Guidelines

When designing Concept Exercises, please consider the following guidelines:

1. The exercise should be able to be solved in 5-10 minutes by someone proficient in the language.
1. The exercise should not involve having to learn or engineer a non-trivial algorithm.
1. The exercise should be background-knowledge-agnostic, unless the exercise calls for it (e.g. no maths unless it's a scientific/maths-based Concept).

## Style Guidelines

When writing documentation and supporting Markdown files for Concept Exercises, please consult the [style guide][style-guide] noting the [automatic formatting section][style-guide-auto-formatting]. Also check any language-specific style guides, where applicable.

## Documentation files

The purpose of the documentation files is explained both in this document and in the [Anatomy of a Concept Exercise video][anatomy-of-a-concept-exercise-video].

In the browser, these files will show at the relevant times. When used via the CLI, the `introduction.md` and `instructions.md` will be concatenated along with the track's `cli.md` document into a `README.md` file, which will sit alongside a `HINTS.md` file.

### `.docs/introduction.md`

**Purpose:** Introduce the concept(s) that the exercise teaches to the student.

- The information provided should give the student just enough context to figure out the solution themselves.
- Only information that is needed to understand the fundamentals of the concept and solve the exercise should be provided. Extra information should be left for the `after.md`.
- Links should be used sparingly, if at all. While a link explaining a complex topic like recursion might be useful, for most concepts the links will provide more information than needed so explaining things concisely inline should be the aim.
- Proper technical terms should be used so that the student can easily search for more information.
- Code examples should only be used to introduce new syntax (students should not need to search the web for examples of syntax). In other cases provide descriptions or links instead of code.

As an example, the introduction to a "strings" exercise might describe a string as just a "Sequence of Unicode characters" or a "series of bytes", tell the users how to create a string, and explain that a string has methods that can be used to manipulate it. Unless the student needs to understand more nuanced details in order to solve the exercise, this type of brief explanation (along with an example of its syntax) should be sufficient information for the student to solve the exercise.

For more information, watch [this video][video-docs-introduction.md] and check [this example introduction.md file][docs-introduction.md]. Notice how the example file's introduction is very minimal, but that the language-specific keywords are enclosed in backticks and an example of the newly introduced syntax is included.

### `.docs/instructions.md`

**Purpose:** Provide instructions for the exercise.

This file is split into two parts.

1. The first part explains the "story" or "theme" of the exercise. It should generally contain no code samples.
2. The second part provides clear instructions of what a student needs to do to, in the form of one or more tasks.

Each task must conform to the following standard:

- Start with a second-level heading starting with a number (e.g. `## 1. Do X`, `## 2. Do Y`).
- The heading should describe _what_ to implement, not _how_ to implement it (e.g. `## 1. Check if an appointment has already passed`).
- Describe which function/method the student needs to define/implement (e.g. `Implement method X(...) that takes an A and returns a Z`),
- Provide an example usage of that function in code. These examples should be different to those given in the tests.

We place high value on making Exercism's content safe for everyone and so often err on the side of caution in deciding whether stories are appropriate or not. While we are careful about what we merge, we appreciate that it's hard to be aware of what may be seen as problematic, so we'll always assume you're acting in good faith and do our best to catch any issues in review in a non-confrontational way. If you'd like to check a story with us, please mention @exercism/leadership and we'll look at it together. Here are some guiding points:

- Try to make sure the story is welcoming and can be understood by everyone. If the story contains in-jokes or regional slang, try to think of alternative phrases.
- Try to write examples that are inclusive to everyone. For example, consider using names from other cultures and mixed genders.
- Ask yourself whether you know anyone personally who would take offense by the story. If that's the case, consider changing it to avoid it.

For more information, watch [this video][video-docs-instructions.md] and check [this example instructions.md file][docs-instructions.md]. Notice how the example file has a clear distinction between the story at the top and the tasks with code samples below.

### `.docs/hints.md`

**Purpose:** Provide hints to a student to help them get themselves unstuck in an exercise.

- If the student gets stuck, we will allow them to click a button requesting a hint, which will show the relevant part of file.
- Hints should be bullet-pointed underneath headings.
- The hints should be enough to unblock almost any student.
- The hints should not spell out the solution, but instead point to a resource describing the solution (e.g. linking to documentation for the function to use).
- The hints may use code samples to explain concepts, but not to outline the solution. e.g. in a lists exercise they might show a snippet of how a certain list function works, but not in a way that is directly copy/pasteable into the solution.
- General hints about the exercise can appear under the `## General` heading.
- Task-specific hints should appear underneath headings that match their task heading in the `instructions.md` (e.g. `## 2. Do Y`).
- Task headings should describe the _what_ of the task, not the _how_.
- Task headings should use regular sentence casing (e.g. `## 2. Check if a book can be borrowed`).
- Tasks should be explicit about what method/function/type to implemented and its expected value (e.g. `` Implement the 'canBorrowBook' function to check if a book can be borrowed. The function takes a book as its parameter and returns `true` if the book has not already been borrowed; otherwise, return `false` ``).

Viewing hints will not be a "recommended" path and we will (softly) discourage using it unless the student can't progress without it. As such, it's worth considering that the student reading it will be a little confused/overwhelmed and maybe frustrated.

For more information, watch [this video][video-docs-hints.md] and check [this example hints.md file][docs-hints.md]. Notice how the example file has general and task-specific hints and how the hints don't give away the answer but instead link to (external) resources.

### `.docs/after.md`

**Purpose:** Provide information about the concept(s) for a student to learn from.

Once the student completes the exercise they will be shown this file, which should provide them with a summary of what the exercise aimed to teach. If the exercise introduced new syntax, syntax samples should be included. At a minimum, this file should contain all information that is introduced in the [`.docs/introduction.md` document](#docsintroductionmd).

This document can also link to any additional resources that might be interesting to the student in the context of the exercise, such as:

- Popular usages for a feature
- Common pitfalls in a feature's use (e.g. casual use of multiple **threads**)
- Limitations on use that may catch out the unsuspecting developer
- Alternative approaches addressed in other exercises
- Compromises made for ease of learning or to accommodate the Exercism environment, e.g. multiple classes in single file
- Similar features with which the concept may be confused
- Performance characteristics and memory usage

For more information, watch [this video][video-docs-after.md] and check (example document no longer available).

### `.docs/source.md` (required if there are third-party sources)

**Purpose:** Describe the third-party source(s) of the exercise.

This file contains third-party references and sources of the exercise. Only required if there are any such sources, but not if the exercise was completely designed from scratch for Exercism.

For more information, check [this example source.md file][meta-source.md].

### `.meta/design.md`

**Purpose:** Describe the design of the exercise.

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more. This information can be extracted from the exercise's corresponding GitHub issue.

It exists in order to inform future maintainers or contributors about the scope and limitations of an exercise, to avoid the natural trend towards making exercises more complex over time.

For more information, watch [this video][video-meta-design.md] and check [this example design.md file][meta-design.md].

### `.meta/config.json`

**Purpose:** Contains meta information on the exercise.

This file contains meta information on the exercise:

- The exercise's author(s) (required)
  - Including reviewers if their reviews substantially change the exercise (to the extent where it feels like "you got there together")
- The exercise's contributor(s) (optional)
  - Including reviewers if their reviews are meaningful/actionable/actioned.
- Which exercise(s) it was forked from (required if the exercise is forked)
- Language version requirements (optional)

If someone is both an author _and_ a contributor, only list that person as an author.

For more information, watch [this video][video-meta-config.json] and check [this example config.json file][meta-config.json].

#### Example

Assume that the user FSharpForever has written an exercise called `basics` for the F# track. PythonProfessor adapts the exercise for the Python track. Later on, the user PythonPerfection improves the exercise.

##### Python `basics` exercise `.meta/config.json` file (fork)

```json
{
  "contributors": [
    {
      "github_username": "PythonPerfection",
      "exercism_username": "PythonPerfection"
    }
  ],
  "authors": [
    {
      "github_username": "PythonProfessor",
      "exercism_username": "PythonProfessor"
    }
  ],
  "forked_from": ["fsharp/basics"],
  "language_versions": ">=3.7"
}
```

##### F# `basics` exercise `.meta/config.json` file (source)

```json
{
  "authors": [
    {
      "github_username": "FSharpForever",
      "exercism_username": "FSharpForever"
    }
  ]
}
```

Note that:

- The order of authors and contributors is not significant and has no meaning.
- If you are forking an exercise, do not reference original authors or contributors. Just ensure that `forked_from` is correct.
- While not common, it _is_ possible to fork from multiple exercises.
- `language_versions` is a free-form string that tracks are free to use and interpret as they like.

## Code files

**Purpose:** Implement the exercise.

What these files look like depends on your track. At a minimum, the following track-specific files must be added:

### Stub implementation file

**Purpose:** Provide a starting point for students.

- Design the stub such that a student will know where to add code.
- Define stubs for any syntax that is not introduced in the exercise. For most exercises, this means defining stub function/methods.
- For compiled languages, consider having compilable code, as compiler messages can sometimes be hard to grasp for students new to the language.
- The code should be as simple as possible.
- Only use language features introduced by the exercise or its prerequisites (and their prerequisites, and so on).
- The stub file is shown to the student when doing in-browser coding and is downloaded to the student's file system when using the CLI.

For more information, watch [this video][video-stub-file] and check [this example stub file][stub-file].

### Tests file

**Purpose:** Verify a solution's correctness.

- The tests should not use the examples from the `instructions.md` file.
- The code should be as simple as possible.
- Only use language features introduced by the exercise's prerequisites (and their prerequisites, and so on).
- All but the first test should be skipped by default. How this is done differs between languages.
- The tests file is _not_ shown to the student when doing in-browser coding, but _is_ downloaded to the student's file system when using the CLI.

For more information, watch [this video][video-tests-file] and check [this example tests file][tests-file].

### Example implementation file

**Purpose:** Provide an idiomatic implementation that passes all the tests.

- This implementation is the target code that we want a student to aim for.
- Mentors will be shown this code as the "target" when writing feedback
- The implementation should only use language features introduced by the exercise or its prerequisites (and their prerequisites, and so on).
- The example file is _not_ shown to the student when doing in-browser coding and is _not_ downloaded to the student's file system when using the CLI.

For more information, watch [this video][video-example-file] and check [this example file][example-file].

## Shared files

### `exercises/shared/.docs/cli.md`

**Purpose:** Explain how to use the Exercism CLI to work with an exercise.

This file contains information on how to work with the exercise when using the CLI to download and submit the exercise.

See [this example cli.md file][shared-docs-cli.md].

### `exercises/shared/.docs/debug.md`

**Purpose:** Describe the track's in-browser debug options.

This file explains how a student that is coding in the browser can still do "debugging."

See [this example debug.md file][shared-docs-debug.md].

### `config.json`

**Purpose:** Contains meta information on the track.

This file contains track-specific metadata, such as its editor settings but most importantly its exercises. It is an evolution of the v2's `config.json` format, and we have a [guide on how to migrate to the v3 format][migrating-your-config-json-files].

For each exercise:

- The UUID must be unique.
- The slug is a kebab-case version of the exercise name (e.g. `anonymous-methods`).
- The concepts (and prerequisites) must adhere to [these naming rules][determining-concepts-naming].

See [this example config.json file][config.json].

[docs-hints.md]: ../languages/csharp/exercises/concept/floating-point-numbers/.docs/hints.md
[docs-introduction.md]: ../languages/csharp/exercises/concept/strings/.docs/introduction.md
[docs-instructions.md]: ../languages/csharp/exercises/concept/floating-point-numbers/.docs/instructions.md
[meta-config.json]: ../languages/fsharp/exercises/concept/booleans/.meta/config.json
[meta-source.md]: ../languages/julia/exercises/concept/encounters/.docs/source.md
[meta-design.md]: ../languages/fsharp/exercises/concept/booleans/.meta/design.md
[shared-docs-cli.md]: ../languages/csharp/exercises/shared/.docs/cli.md
[shared-docs-debug.md]: ../languages/csharp/exercises/shared/.docs/debug.md
[config.json]: ../languages/csharp/config.json
[style-guide]: ./maintainers/style-guide.md
[style-guide-auto-formatting]: ./maintainers/style-guide.md#auto-formatting
[anatomy-of-a-concept-exercise-video]: https://www.youtube.com/watch?v=gkbBqd7hPrA
[video-docs-introduction.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=77
[video-docs-instructions.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=309
[video-docs-hints.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=482
[video-docs-after.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=596
[video-meta-design.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=870
[video-meta-config.json]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=1037
[video-stub-file]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=1171
[video-tests-file]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=1255
[video-example-file]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=781
[stub-file]: ../languages/csharp/exercises/concept/strings/Strings.cs
[tests-file]: ../languages/csharp/exercises/concept/strings/StringsTests.cs
[example-file]: ../languages/csharp/exercises/concept/strings/.meta/Example.cs
[determining-concepts-naming]: ./maintainers/determining-concepts.md#naming-concepts
[migrating-your-config-json-files]: ./maintainers/migrating-your-config-json-files.md
