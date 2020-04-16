# How to implement a Kotlin concept exercise

```diff
! WARNING! This document should be treated as a draft and will be actively changing. Sections that marked as WIP requires attention/rework from contributors.
```

This document describes the steps required to implement a concept exercise in any v3 track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read the following documents:

- [The features of v3][docs-features-of-v3].
- [Rationale for v3][docs-rationale-for-v3].
- [What are concept exercise and how they are structured?][docs-concept-exercises]

Please also watch the following video:

- [The Anatomy of a Concept Exercise][anatomy-of-a-concept-exercise].

As this document is generic, the following placeholders are used:

- `<exercise-slug>`: the name of the exercise in kebab-case (e.g. `functions-basic`).
- `<ExerciseSlug>`: the name of the exercise in PascalCase (e.g. `FunctionsBasic`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the [exercise's GitHub issue](https://github.com/exercism/v3/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc+label%3Atrack%2Fkotlin+Implement+new+concept+exercise+in%3Atitle).

Any concept exercise in any v3 track requires the following files to be created:

<pre>
languages
└── kotlin
    └── exercises
        └── concept
            └── &lt;exercise-slug>
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   ├── source.md (required if there are third-party sources)
                |   └── after.md
                ├── .meta
                |   |── design.md
                |   |── config.json
                |   └── Example.kt
                ├── src
                |   ├── main
                |   | 	└── kotlin
                |   | 		└── &lt;ExerciseSlug>.kt
                |   └── test
                |    	└── kotlin
                |    		└── &lt;ExerciseSlug>Test.kt
                └── gradle
                |	└── wrapper
                | 		├── gradle-wrapper.jar
                |		└── gradle-wrapper.properties
                ├── build.gradle.kts
                ├── gradlew
                └── gradlew.bat
</pre>

## Step 1: add .docs/introduction.md

This file contains an introduction to the concept. It should make the exercise's learning goals explicit and provide a short introduction with enough detail for the student to complete the exercise. The aim is to give the student just enough context to figure out the solution themselves, as research has shown that self-discovery is the most effective learning experience. Using the proper technical terms in the descriptions will be helpful if the student wants to search for more information. If the exercise introduces new syntax, an example of the syntax should always be included; students should not need to search the web for examples of syntax.

As an example, the introduction to a "strings" exercise might describe a string as just a "Sequence of Unicode characters" or a "series of bytes". Unless the student needs to understand more nuanced details in order to solve the exercise, this type of brief explanation (along with an example of its syntax) should be sufficient information for the student to solve the exercise.

## Step 2: add .docs/instructions.md

This file contains instructions for the exercise. It should explicitly explain what the student needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

## Step 3: add .docs/hints.md

If the student gets stuck, we will allow them to click a button requesting a hint, which shows this file. This will not be a "recommended" path and we will (softly) discourage them using it unless they can't progress without it. As such, it's worth considering that the student reading it will be a little confused/overwhelmed and maybe frustrated.

The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any student. They might link to the docs of the functions that need to be used.

The hints should not spell out the solution, but instead point to a resource describing the solution (e.g. linking to documentation for the function to use).

## Step 4: add .docs/after.md

Once the student completes the exercise they will be shown this file, which should provide them with a summary of what the exercise aimed to teach. If the exercise introduced new syntax, syntax samples should be included. This document can also link to any additional resources that might be interesting to the student in the context of the exercise.

~~These files are also all described in the [concept exercises document][docs-concept-exercises].~~

## Step 5: update languages/kotlin/config.json

An entry should be added to the track's `config.json` file for the new concept exercise:

```json
{
  ...
  "exercises": {
    "concept": [
      ...
      {
        "slug": "<exercise-slug>",
        "uuid": "<UUID>",
        "concepts": ["<CONCEPT-1>", "<CONCEPT-2>"],
        "prerequisites": ["<PREREQUISITE-1>", "<PREREQUISITE-2>"]
      }
    ]
  }
}
```

Where:

- `<UUID>` - is unique exercise uuid;
- `<CONCEPT-1`/`<CONCEPT-2>` - concepts that are explained in the exercise (at least one concept is required, more concepts are optional);
- `<PREREQUISITE-1>`/`<PREREQUISITE-2>` - this exercises will be blocked for student unless all concepts from from `prerequisites` are already familiar for student (e.g. he finished exercises covering this concepts).

## Step 6: adding track-specific files

Having added the files that are not specific to the track, now is the time to create the track-specific files. These file will include:

- A stub implementation file (`src/main/kotlin/<ExerciseSlug>.kt`).
- A file containing the test suite (`src/test/kotlin/<ExerciseSlug>.kt`).
- An example implementation file that passes all the tests (`meta/Example.kt`).

What these files look like depends on your track. Note that some tracks might require more files in addition to the three files just mentioned.

## Step 7: add analyzer (optional)

```diff
! WIP ignore this for now as we don't have proper analyzer yet
```

Some exercises could benefit from having an exercise-specific analyzer. If so, please check the track's analyzer document for details on how to do this.

Skip this step if you're not sure what to do.

## Step 8: custom representation (optional)

```diff
! WIP ignore this for now as we don't have proper analyzer yet
```

Some exercises could benefit from having an custom representation as generated by the track's representer. If so, please check the track's representer document for details on how to do this.

Skip this step if you're not sure what to do.

## Step 9: add `.meta/design.md`

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more. This information can be extracted from the exercise's corresponding GitHub issue.

## Step 10: add .meta/config.json:

This file contains meta information on the exercise, which currently only includes the exercise's contributors.

## Inspiration

When implementing an exercise, it can be very useful to look at the exercises the track has already implemented. You can also check the exercise's [general concepts documents][reference] to see if other languages that have already an exercise for that concept.

## Help

If you have any questions regarding implementing this exercise, please post them as comments in the exercise's GitHub issue.

[reference]: ../reference/README.md
[docs-concept-exercises]: ../../../docs/concept-exercises.md
[docs-rationale-for-v3]: ../../../docs/rationale-for-v3.md
[docs-features-of-v3]: ../../../docs/features-of-v3.md
[anatomy-of-a-concept-exercise]: https://www.youtube.com/watch?v=gkbBqd7hPrA
