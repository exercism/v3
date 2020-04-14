# How to implement a concept exercise

This document describes the steps required to implement a concept exercise in any v3 track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read the following documents:

- [The features of v3][docs-features-of-v3].
- [Rationale for v3][docs-rationale-for-v3].
- [What are concept exercise and how they are structured?][docs-concept-exercises]

As this document is generic, the following placeholders are used:

- `<LANG>`: the name of the track in kebab-case (e.g. `ruby`).
- `<SLUG>`: the name of the exercise in kebab-case (e.g. `anonymous-methods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue.

Any concept exercise in any v3 track requires the following files to be created:

<pre>
languages
└── &lt;LANG&gt;
    └── exercises
        └── concept
            └── &lt;SLUG&gt;
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   ├── source.md (required if there are third-party sources)
                |   └── after.md
                └── .meta
                    ├── config.json
                    └── design.md
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

_The aforementioned files are also described in the [concept exercises document][docs-concept-exercises]._

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

- A stub implementation file.
- A file containing the test suite.
- An example implementation file that passes all the tests.

What these files look like depends on your track. Note that some tracks might require more files in addition to the three files just mentioned.

## Step 7: update the general concept document

Add the exercise to the [concept's shared document's][reference] `## Implementations` section ([example](https://github.com/exercism/v3/blob/master/reference/types/string.md#implementations)).

## Step 8: add analyzer (optional)

Some exercises could benefit from having an exercise-specific analyzer. If so, please check the track's analyzer document for details on how to do this.

Skip this step if you're not sure what to do.

## Step 9: custom representation (optional)

Some exercises could benefit from having an custom representation as generated by the track's representer. If so, please check the track's representer document for details on how to do this.

Skip this step if you're not sure what to do.

## Step 10: add .meta/design.md:

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more ([example][meta-design]). This information can be extracted from the exercise's corresponding GitHub issue.

## Step 11: add .meta/config.json:

This file contains meta information on the exercise, which currently includes the exercise's contributors, and (optionally) language version requirements ([example][meta-config-json]).

## Inspiration

When implementing an exercise, it can be very useful to look at the exercises the track has already implemented. You can also check the exercise's [general concepts documents][reference] to see if other languages that have already an exercise for that concept.

## Help

If you have any questions regarding implementing this exercise, please post them as comments in the exercise's GitHub issue.

[docs-concept-exercises]: ../concept-exercises.md
[docs-rationale-for-v3]: ../rationale-for-v3.md
[docs-features-of-v3]: ../features-of-v3.md
[reference]: ../../reference/concepts/README.md
[meta-design]: ../../languages/csharp/exercises/concept/flag-enums/.meta/design.md
[meta-config-json]: ../../languages/csharp/exercises/concept/flag-enums/.meta/config.json
