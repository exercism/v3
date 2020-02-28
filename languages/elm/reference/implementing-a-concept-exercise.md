# How to implement an Elm concept exercise

This document describes the steps required to implement a concept exercise for the Elm track. As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).
- `<NAME>`: the name of the exercise in PascalCase (e.g. `AnonymousMethods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue.

Any concept exercise in any v3 track requires the following files to be created:

<pre>
languages
└── elm
    └── exercises
        └── concept
            └── &lt;SLUG&gt;
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── after.md (optional)
                └── .meta
                |   └── config.json
                |   └── design.md
                |   └── Example.elm
                ├── elm.json
                ├── src
                |   └── &lt;NAME&gt;.elm
                └── tests
                    └── &lt;NAME&gt;.elm
</pre>

## Step 1: add .docs/introduction.md

This file contains an introduction to the concept. It should be explicit about what the student should learn from the exercise, and provide a short, concise introduction to the concept(s). The aim is to give the student just enough context to figure things out themselves and solve the exercise, as research has shown that self-discovery is the most effective learning experience. Mentioning technical terms that the student can Google if they so want, is preferable over including any code samples or an extensive description. For example we might describe a string as a "Sequence of Unicode characters" or a "series of bytes" or "an object". Unless the student needs to understand the details of what those mean to be able to solve the exercise we should not give more info in this introduction - instead allowing the student to Google, ignore, or map their existing knowledge.

## Step 2: add .docs/instructions.md

This file contains instructions for the exercise. It should explicitly explain what the user needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

## Step 3: add .docs/hints.md

If the student gets stuck, we will allow them to click a button requesting a hint, which shows this file. This will not be a "recommended" path and we will (softly) discourage them using it unless they can't progress without it. As such, it's worth considering that the student reading it will be a little confused/overwhelmed and maybe frustrated.

The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any student. They might link to the docs of the functions that need to be used.

The hints should not spell out the solution, but instead point to a resource describing the solution (e.g. linking to documentation for the function to use).

## Step 4: add .docs/after.md (optional)

Once the student completes the exercise they will be shown this file, which should provide them with a summary of what the exercise aimed to teach. This document can also link to any additional resources that might be interesting to the student in the context of the exercise.

## Step 5: update languages/&lt;TRACK&gt;/config.json

An entry should be added to the track's `config.json` file for the new concept exercise:

```json
{
  ...
  "exercises": {
    "concept": [
      ...
      {
        "slug": "<SLUG>",
        "uuid": "<UUID>",
        "concepts": ["<CONCEPT-1>"],
        "prerequisites": ["<PREREQUISITE-1>", "<PREREQUISITE-2>"]
      }
    ]
  }
}
```

## Step 6: adding track-specific files

Having added the files that are not specific to the track, now is the time to create the track-specific files. These file will include:

- An elm.json file.
- A stub implementation file (src/&lt;NAME&gt;.elm).
- A file containing the test suite (tests/&lt;NAME&gt;.elm).
- An example implementation file that passes all the tests (.meta/Example.elm).

What these files look like depends on your track. Note that some tracks might require more files in addition to the three files just mentioned.

## Step 7: update the general concept document

Add the exercise to the [concept's shared document's][reference] `## Implementations` section ([example](https://github.com/exercism/v3/blob/master/reference/types/string.md#implementations)).

## Step 8: add analyzer (optional)

Some exercises could benefit from having an exercise-specific analyzer. If so, please check the track's analyzer document for details on how to do this.

Skip this step if your track does not have an analyzer.

## Step 9: custom representation (optional)

Some exercises could benefit from having a custom representation as generated by the track's representer. If so, please check the track's representer document for details on how to do this.

Skip this step if your track does not have a representer.

## Step 10: add .meta/design.md:

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more ([example][meta-design]). This information can be extracted from the exercise's corresponding GitHub issue.

## Step 11: add .meta/config.json:

This file contains meta information on the exercise, which currently only includes the exercise's contributors ([example][meta-config-json]).

## Inspiration

When implementing an exercise, it can be very useful to look at the exercises the track has already implemented. You can also check the exercise's [general concepts documents][reference] to see if other languages that have already an exercise for that concept.

## Help

If you have any questions regarding implementing this exercise, please post them as comments in the exercise's GitHub issue.

### TODO: link to elm document, once it is done and available to reference.
[reference]: ../../csharp/reference/README.md
### TODO: link to an elm concept, after the first one is done and available to reference.
[meta-design]: ../../csharp/exercises/concept/enums-advanced/.meta/design.md
### TODO: link to an elm concept, after the first one is done and available to reference.
[meta-config-json]: ../../csharp/exercises/concept/enums-advanced/.meta/config.json
