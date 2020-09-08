# How to implement a Pharo Smalltalk concept exercise

This issue describes how to implement a concept exercise for the Pharo Smalltalk track.

## Getting started

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read up on the following documents:

- [The features of v3](https://github.com/exercism/v3/blob/master/docs/concept-exercises.md).
- [Rationale for v3](https://github.com/exercism/v3/blob/master/docs/rationale-for-v3.md).
- [What are concept exercise and how they are structured?](https://github.com/exercism/v3/blob/master/docs/features-of-v3.md)

Please also watch the following video:

- [The Anatomy of a Concept Exercise](https://www.youtube.com/watch?v=gkbBqd7hPrA).

As this document is generic, the following placeholders are used:

- `<SLUG>`: the name of the exercise in kebab-case (e.g. anonymous-methods).
- `<NAME>`: the name of the exercise in PascalCase (e.g. AnonymousMethods).
- `<UUID>`: the exercise's unique UUID.

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue. Having done this, please read the Pharo Smalltalk concept exercises introduction.

To implement a concept exercise, the following files must be added:

```
languages
└── pharo-smalltalk
    └── exercises
        └── concept
            └── <SLUG>
                ├── .docs
                |   ├── after.md
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── source.md (required if there are third-party sources)
                ├── .meta
                |   |── config.json
                |   └── design.md
                ├── <NAME>class.st
                └── <NAME>Test.st
```

All Markdown files should adhere to the [style guide][style-guide], noting the [automatic formatting section][style-guide-auto-formatting]. Also check any language-specific style guides, where applicable.

The following files need to be added and updated:

## Add `.docs/introduction.md` file

**Purpose:** Introduce the concept(s) that the exercise teaches to the student.

For more information, please read [this in-depth description][docs-introduction.md], watch [this video][video-docs-introduction.md] and check [this example file][example-docs-introduction.md].

## Add `.docs/instructions.md` file

**Purpose:** Provide instructions for the exercise.

For more information, please read [this in-depth description][docs-instructions.md], watch [this video][video-docs-instructions.md] and check [this example file][example-docs-instructions.md].

## Add `.docs/hints.md` file

**Purpose:** Provide hints to a student to help them get themselves unstuck in an exercise.

For more information, please read [this in-depth description][docs-hints.md], watch [this video][video-docs-hints.md] and check [this example file][example-docs-hints.md].

## Add `.docs/after.md` file

**Purpose:** Provide more information about the concept(s) for a student to learn from.

For more information, please read [this in-depth description][docs-after.md], watch [this video][video-docs-after.md] and check [this example file][example-docs-after.md].

## Add `.docs/source.md` file (required if there are third-party sources)

**Purpose:** Describe the third-party source(s) of the exercise.

For more information, please read [this in-depth description][docs-source.md] and check [this example file][example-docs-source.md].

_Skip this step if there aren't any third-party sources._

## Add `.meta/design.md` file

**Purpose:** Describe the design of the exercise.

For more information, please read [this in-depth description][meta-design.md], watch [this video][video-meta-design.md] and check [this example file][example-meta-design.md].

## Add `.meta/config.json` file

**Purpose:** Contains meta information on the exercise.

For more information, please read [this in-depth description][meta-config.json], watch [this video][video-meta-config.json] and check [this example file][example-meta-config.json].

## Update `languages/<TRACK>/config.json`

**Purpose:** Contains meta information on the track. Please read [this section][config.json] for more information.

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

- Concepts (and prerequisites) must adhere to [these naming rules][determining-concepts-naming].
- The UUID can be randomly generated using the [UUID Generator][uuid-gen]. (TODO: UUIDs can be generated in image for V2. We will also port this to V3)

For more information, please read [this in-depth description][config.json] and check [this example file][example-config.json].

## Update reference document(s) (if the reference document(s) exists)

**Purpose:** Allow maintainers to find out which tracks have implemented an exercise for a concept.

For many concepts, a reference document exists in the `reference/concepts` or `reference/types` directories. If such a document exists for the exercise's concept(s), add the exercise to the corresponding exercise's `Implementations` section or create a new section for your exercise. When no reference document exists, consider writing one.

See [this example file][example-reference-document-implementations].

## Add or update story document

**Purpose:** Allow maintainers to find out which tracks have implemented an exercise for a story.

Each exercise has a story or theme. The stories people have created so far are documented in the `reference/stories` directory. If such a document exists for the exercise's story, add the exercise to the corresponding exercise's `Implementations` section. When no story document exists, consider adding one.

See [this example file][example-story-document-implementations].

## Update analyzer (optional)

Some exercises could benefit from having an exercise-specific analyzer. If so, please check the track's analyzer document for details on how to do this.

_Skip this step if you're not sure what to do._

## Update representer (optional)

Some exercises could benefit from having an custom representation as generated by the track's representer. If so, please check the track's representer document for details on how to do this.

_Skip this step if you're not sure what to do._

## Inspiration

When implementing an exercise, it can be very useful to look at the exercises the track has already implemented. You can also check the exercise's [general concepts documents][reference] to see if other languages that have already an exercise for that concept.

## Help

If you have any questions regarding implementing this exercise, please post them as comments in the exercise's GitHub issue.

[concept-exercises]: ../concept-exercises.md
[rationale-for-v3]: ../rationale-for-v3.md
[features-of-v3]: ../features-of-v3.md
[anatomy-of-a-concept-exercise]: https://www.youtube.com/watch?v=gkbBqd7hPrA
[reference]: https://github.com/exercism/v3/blob/master/reference/concepts/README.md
[docs-introduction.md]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#docsintroductionmd
[docs-instructions.md]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#docsinstructionsmd
[docs-hints.md]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#docshintsmd
[docs-after.md]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#docsaftermd
[docs-source.md]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#docssourcemd-required-if-there-are-third-party-sources
[meta-design.md]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#metadesignmd
[meta-config.json]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#metaconfigjson
[config.json]: https://github.com/exercism/v3/blob/master/docs/concept-exercises.md#configjson
[example-docs-introduction.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/strings/.docs/introduction.md
[example-docs-instructions.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/instructions.md
[example-docs-hints.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/hints.md
[example-docs-after.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/floating-point-numbers/.docs/after.md
[example-docs-source.md]: https://github.com/exercism/v3/blob/master/languages/julia/exercises/concept/multiple-dispatch/.docs/source.md
[example-meta-design.md]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/numbers/.meta/design.md
[example-meta-config.json]: https://github.com/exercism/v3/blob/master/languages/csharp/exercises/concept/flag-enums/.meta/config.json
[example-config.json]: https://github.com/exercism/v3/blob/master/languages/csharp/config.json
[example-reference-document-implementations]: https://github.com/exercism/v3/blob/master/reference/types/string.md#exercises
[example-story-document-implementations]: https://github.com/exercism/v3/blob/master/reference/stories/basics.lasagna.md#implementation
[video-docs-introduction.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=77
[video-docs-instructions.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=309
[video-docs-hints.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=482
[video-docs-after.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=596
[video-meta-design.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=870
[video-meta-config.json]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=1037
[style-guide]: https://github.com/exercism/v3/blob/master/docs/maintainers/style-guide.md
[style-guide-auto-formatting]: https://github.com/exercism/v3/blob/master/docs/maintainers/style-guide.md#auto-formatting
[determining-concepts-naming]: https://github.com/exercism/v3/blob/master/docs/maintainers/determining-concepts.md#naming-concepts
[uuid-gen]: https://www.uuidgenerator.net/version4
