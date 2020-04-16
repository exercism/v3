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

When writing documentation and supporting Markdown files for Concept Exercises, please consult the [style guide][style-guide] and especially the [automatic formatting section][style-guide-auto-formatting]. Also check any language-specific style guides, when applicable.

## Exercise Structure

An exercise has the following files. In the browser they will show at the relevant times. When used via the CLI, the introduction and instructions will be concatenated along with the track's CLI instructions into a README.md, which will sit alongside a HINTS.md.

### `.docs/introduction.md`

**Purpose:** Introduce the concept(s) that the exercise teaches to the student.

- The information provided should give the student just enough context to figure out the solution themselves.
- Only information that is needed to understand the fundamentals of the concept and solve the exercise should be provided. Extra information should be left for the `after.md`.
- Links should be used sparingly, if at all. While a link explaining a complex topic like recursion might be useful, for most concepts the links will provide more information than needed so explaining things concisely inline should be the aim.
- Proper technical terms should be used so that the student can easily search for more information.
- Code examples should only be used to introduces new syntax (students should not need to search the web for examples of syntax). In other cases provide descriptions or links instead of code.

As an example, the introduction to a "strings" exercise might describe a string as just a "Sequence of Unicode characters" or a "series of bytes", tell the users how to create a string, and explain that a string has methods that can be used to manipulate it. Unless the student needs to understand more nuanced details in order to solve the exercise, this type of brief explanation (along with an example of its syntax) should be sufficient information for the student to solve the exercise.

See [this example introduction.md file][introduction.md]. Notice how the introduction is very minimal, but that the language-specific keywords are enclosed in quotes and an example of the newly introduced syntax is included.

### `.docs/instructions.md`

**Purpose:** Provide instructions for the exercise.

This file is split into two parts.

1. The first part explains the "story" or "theme" of the exercise. It should generally contain no code samples.
2. The second part provides clear instructions of what a student needs to do to, in the form of one or more tasks.

Each task must conform to the following standard:

- Start with a third-level heading starting with a number (e.g. `### 1. Do X`, `### 2. Do Y`).
- Describe which function/method the student needs to define/implement (e.g. `Implement method X(...) that takes an A and returns a Z`),
- Provide an example usage of that function in code. These examples should be different to those given in the tests.

See [this example instructions.md file][instructions.md]. Notice the clear distinction between the story at the top and the tasks with code samples below.

### `.docs/hints.md`

**Purpose:** Provide hints to a student to help them get themselves unstuck in an exercise.

- If the student gets stuck, we will allow them to click a button requesting a hint, which will show the relevant part of file.
- Hints should be bullet-pointed underneath headings.
- The hints should be enough to unblock almost any student.
- The hints should not spell out the solution, but instead point to a resource describing the solution (e.g. linking to documentation for the function to use).
- General hints about the exercise can appear under the `### General` heading.
- Task-specific hints should appear underneath headings that match their task heading in the `instructions.md` (e.g. `### 2. Do Y`).

Viewing hints will not be a "recommended" path and we will (softly) discourage using it unless the student can't progress without it. As such, it's worth considering that the student reading it will be a little confused/overwhelmed and maybe frustrated.

See [this example hints.md file][hints.md]. Notice how there are general and task-specific hints and how the hints don't give away the answer but instead link to (external) resources.

### `.docs/after.md`

**Purpose:** Provide more information about the concept(s) for a student to learn from.

Once the student completes the exercise they will be shown this file, which should provide them with a summary of what the exercise aimed to teach. If the exercise introduced new syntax, syntax samples should be included. This document can also link to any additional resources that might be interesting to the student in the context of the exercise.

See [this example after.md file][after.md].

### `.docs/source.md` (required if there are third-party sources)

**Purpose:** Describe the third-party source(s) of the exercise.

This file contains third-party references and sources of the exercise. Only required if there are any such sources, but not if the exercise was completely designed from scratch for Exercism.

See [this example source.md file][source.md].

### `.meta/design.md`

**Purpose:** Describe the design of the exercise.

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more. This information can be extracted from the exercise's corresponding GitHub issue.

It exists in order to inform future maintainers or contributors about the scope and limitations of an exercise, to avoid the natural trend towards making exercises more complex over time.

See [this example design.md file][design.md].

### `.meta/config.json`

**Purpose:** Contains meta information on the exercise.

This file contains meta information on the exercise:

- The exercise's author(s) (required)
- The exercise's contributor(s) (optional)
- Which exercise it was forked from (required if the exercise is forked)
- Language version requirements (optional)

See [this example config.json file][config.json].

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
- `language_versions` is a free-form string that tracks are free to use and interpret as they like.

## Track Structure

### `exercises/shared/.docs/cli.md`

This file contains information on how to work with the exercise when using the CLI to download and submit the exercise.

See [this example cli.md file][cli.md].

### `exercises/shared/.docs/debug.md`

This file explains how a student that is coding in the browser can still do "debugging."

See [this example debug.md file][debug.md].

[cli.md]: ../languages/csharp/exercises/shared/.docs/cli.md
[debug.md]: ../languages/csharp/exercises/shared/.docs/debug.md
[after.md]: ../languages/csharp/exercises/concept/floating-point-numbers/.docs/after.md
[hints.md]: ../languages/csharp/exercises/concept/floating-point-numbers/.docs/hints.md
[introduction.md]: ../languages/csharp/exercises/concept/strings/.docs/introduction.md
[instructions.md]: ../languages/csharp/exercises/concept/floating-point-numbers/.docs/instructions.md
[config.json]: ../languages/fsharp/exercises/concept/booleans/.meta/config.json
[source.md]: ../languages/julia/exercises/concept/multiple-dispatch/.docs/source.md
[style-guide]: ./maintainers/style-guide.md
[style-guide-auto-formatting]: maintainers/style-guide.md#auto-formatting
