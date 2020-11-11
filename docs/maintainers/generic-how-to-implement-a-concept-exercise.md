# How to implement a concept exercise

This document describes the steps required to implement a concept exercise in any v3 track.

**Please please please read the docs before starting.** Posting PRs without reading these docs will be a lot more frustrating for you during the review cycle, and exhaust Exercism's maintainers' time. So, before diving into the implementation, please read the following documents:

- [The features of v3][features-of-v3].
- [Rationale for v3][rationale-for-v3].
- [What are concept exercise and how they are structured?][concept-exercises]

Please also watch the following video:

- [The Anatomy of a Concept Exercise][anatomy-of-a-concept-exercise].

As this document is generic, the following placeholders are used:

- `<LANG>`: the name of the track in kebab-case (e.g. `ruby`).
- `<SLUG>`: the slug of the exercise in kebab-case (e.g. `calculator-conundrum`).
- `<UUID>`: the exercise's unique UUID.
- `<CONCEPT_SLUG>`: the slug of one of the exercise's concepts in kebab-case (e.g. `anonymous-methods`).

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue.

Any concept exercise in any v3 track requires at least the following files to be created:

<pre>
languages
└── &lt;LANG&gt;
    ├── concepts
    |   └── &lt;CONCEPT_SLUG&gt;
    |       ├── about.md
    |       └── links.json
    └── exercises
        └── concept
            └── &lt;SLUG&gt;
                ├── .docs
                |   ├── introduction.md
                |   ├── instructions.md
                |   ├── hints.md
                |   └── source.md (required if there are third-party sources)
                └── .meta
                    ├── design.md
                    └── config.json
</pre>

Note that the concept files should be created for each concept the exercise teaches.

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

## Add `.docs/source.md` file (required if there are third-party sources)

**Purpose:** Describe the third-party source(s) of the exercise.

For more information, please read [this in-depth description][docs-source.md] and check [this example file][example-docs-source.md].

_Skip this step if there aren't any third-party sources._

## Add `.meta/design.md` file

**Purpose:** Describe the design of the exercise.

For more information, please read [this in-depth description][meta-design.md], watch [this video][video-meta-design.md] and check [this example file][example-meta-design.md].

## Add `.meta/config.json` file

**Purpose:** Contains meta information on the exercise.

For more information, please read [this in-depth description][meta-design.md], watch [this video][video-meta-config.json] and check [this example file][example-meta-design.md].

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
- The UUID can be randomly generated using the [UUID Generator][uuid-gen].

For more information, please read [this in-depth description][config.json] and check [this example file][example-config.json].

## Add `concepts/<CONCEPT>/about.md` for each taught concept

**Purpose:** Provide information about the concept(s) for a student to learn from.

For more information, check [this example file][example-concept-about.md].

## Add `concepts/<CONCEPT>/links.json` for each taught concept

**Purpose:** Provide helpful links that provide more reading or information about a concept.

For more information, check [this example file][example-concept-links.json].

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
[reference]: ../../reference/concepts/README.md
[docs-introduction.md]: ../concept-exercises.md#docsintroductionmd
[docs-instructions.md]: ../concept-exercises.md#docsinstructionsmd
[docs-hints.md]: ../concept-exercises.md#docshintsmd
[docs-source.md]: ../concept-exercises.md#docssourcemd-required-if-there-are-third-party-sources
[meta-design.md]: ../concept-exercises.md#metadesignmd
[meta-config.json]: ../concept-exercises.md#metaconfigjson
[config.json]: ../concept-exercises.md#configjson
[example-docs-introduction.md]: ../../languages/csharp/exercises/concept/log-levels/.docs/introduction.md
[example-docs-instructions.md]: ../../languages/csharp/exercises/concept/interest-is-interesting/.docs/instructions.md
[example-docs-hints.md]: ../../languages/csharp/exercises/concept/interest-is-interesting/.docs/hints.md
[example-docs-source.md]: ../../languages/julia/exercises/concept/encounters/.docs/source.md
[example-meta-design.md]: ../../languages/csharp/exercises/concept/cars-assemble/.meta/design.md
[example-meta-config.json]: ../../languages/csharp/exercises/concept/flag-enums/.meta/config.json
[example-config.json]: ../../languages/csharp/config.json
[example-reference-document-implementations]: ../../reference/types/string.md#exercises
[example-story-document-implementations]: ../../reference/stories/basics.lasagna.md#implementation
[example-concept-about.md]: ../../languages/fsharp/concepts/strings/about.md
[example-concept-links.json]: ../../languages/fsharp/concepts/strings/links.json
[video-docs-introduction.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=77
[video-docs-instructions.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=309
[video-docs-hints.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=482
[video-meta-design.md]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=870
[video-meta-config.json]: https://www.youtube.com/watch?v=gkbBqd7hPrA&t=1037
[style-guide]: ./style-guide.md
[style-guide-auto-formatting]: ./style-guide.md#auto-formatting
[determining-concepts-naming]: ./determining-concepts.md#naming-concepts
[uuid-gen]: https://www.uuidgenerator.net/version4
