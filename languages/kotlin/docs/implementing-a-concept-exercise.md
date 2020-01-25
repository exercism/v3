# How to implement a Kotlin concept exercise

```diff
! WARNING! This document should be treated as a draft and will be actively changing. Sections that marked as WIP requires attention/rework from contributors.
```

This document describes the steps required to implement a concept exercise in any v3 track. As this document is generic, the following placeholders are used:

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
                |   └── after.md (optional)
                ├── .meta
                |   ├── config.json
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

This file contains an introduction to the concept. It should be explicit about what the exercise teaches and maybe provide a brief introduction to the concepts, but not give away so much that the user doesn't have to do any work to solve the exercise.

## Step 2: add .docs/instructions.md

This file contains instructions for the exercise. It should explicitly explain what the user needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

## Step 3: add .docs/hints.md

If the user gets stuck, we will allow them to click a button requesting a hint, which shows this file. We will softly discourage them using it. The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any

## Step 4: add .docs/after.md (optional):

Once the user completes the exercise they will be shown this file, which gives them any bonus information or further reading about the concept taught.

~~These files are also all described in the [concept exercises document] [docs-concept-exercises].~~

## Step 5: add .meta/config.json

This file contains metadata for the exercise. The metadata includes paths to the test file and the solution files, as well as a list of all the tests and the method they are calling.

## Step 6: update languages/kotlin/config.json

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
- `<PREREQUISITE-1>`/`<PREREQUISITE-2>` - this exercises will be blocked for user unless all concepts from from `prerequisites` are already familiar for user (e.g. he finished exercises covering this concepts).

## Step 7: adding track-specific files

Having added the files that are not specific to the track, now is the time to create the track-specific files. These file will include:

- A stub implementation file (`src/main/kotlin/<ExerciseSlug>.kt`).
- A file containing the test suite (`src/test/kotlin/<ExerciseSlug>.kt`).
- An example implementation file that passes all the tests (`meta/Example.kt`).

What these files look like depends on your track. Note that some tracks might require more files in addition to the three files just mentioned.

## Step 8: add analyzer (optional)

```diff
! WIP ignore this for now as we don't have proper analyzer yet
```

Some exercises could benefit from having an exercise-specific analyzer. If so, please check the track's analyzer document for details on how to do this.

Skip this step if your track does not have an analyzer.

## Step 9: custom representation (optional)

```diff
! WIP ignore this for now as we don't have proper analyzer yet
```

Some exercises could benefit from having an custom representation as generated by the track's representer. If so, please check the track's representer document for details on how to do this.

Skip this step if your track does not have a representer.

## Inspiration

When implementing an exercise, it can be very useful to look at the exercises the track has already implemented. You can also check the exercise's [general concepts documents][reference] to see if other languages that have already an exercise for that concept.

## Help

If you have any questions regarding implementing this exercise, please post them as comments in the exercise's GitHub issue.

[reference]: ../../reference/README.md
