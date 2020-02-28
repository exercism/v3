# How to implement a TypeScript Concept Exercise

This document describes how to implement a Concept Exercise for the TypeScript track. As this document is generic, the following placeholders are used:

- `<slug>`: the name of the exercise in snake_case (e.g. `anonymous-methods`).
- `<concepts>`: the Concepts the exercise is about (e.g. `loops`),
- `<concept-1>`: a single Concept slug,
- `<prerequisite-n>`: a single Concept slug,
- `<uuid>`: a _new_ v4 UUID (random!)
- `<first-and-last-name>`: your first and last name (e.g. `John Doe`)
- `<git-email>`: the email address you use for git (e.g. `johndoe@email.com`)

Before implementing the exercise, please make sure you have a good understanding of what the exercise should be teaching (and what not). This information can be found in the exercise's GitHub issue. Having done this, please read the [TypeScript Concept exercises introduction][concept-exercises]. If you have come up with something completely new, create a new issue _first_ so we can discuss the Concept Exercise.

To implement a Concept Exercise, the following files must be created:

<pre>
languages
└── typescript
    └── exercises
        └── concept
            └── &lt;slug&gt;
                ├── .docs
                |   ├── instructions.md
                |   ├── introduction.md
                |   ├── hints.md
                |   └── after.md (optional)
                ├── .eslintignore
                ├── .gitignore
                ├── .eslintrc
                ├── jest.config.js
                ├── example.ts
                ├── &lt;slug&gt;.ts
                ├── &lt;slug&gt;.spec.ts
                ├── package.json
                └── yarn.lock
</pre>

## Step 1: add .docs/introduction.md

This file contains an introduction to the concept. It should be explicit about what the student should learn from the exercise, and provide a short, concise introduction to the concept(s). The aim is to give the student just enough context to figure things out themselves and solve the exercise, as research has shown that self-discovery is the most effective learning experience. Mentioning technical terms that the student can Google if they so want, is preferable over including any code samples or an extensive description. For example we might describe a string as a "Sequence of Unicode characters" or a "series of bytes" or "an object". Unless the student needs to understand the details of what those mean to be able to solve the exercise we should not give more info in this introduction - instead allowing the student to Google, ignore, or map their existing knowledge.

## Step 2: add .docs/instructions.md

This file contains instructions for the exercise. It should explicitly explain what the student needs to do (define a method with the signature `X(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

## Step 3: add .docs/hints.md

If the student gets stuck, we will allow them to click a button requesting a hint, which shows this file. This will not be a "recommended" path and we will (softly) discourage them using it unless they can't progress without it. As such, it's worth considering that the student reading it will be a little confused/overwhelmed and maybe frustrated.

The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any student. They might link to the docs of the functions that need to be used.

## Step 4: add .docs/after.md (optional)

Once the student completes the exercise they will be shown this file, which should provide them with a summary of what the exercise aimed to teach. This document can also link to any additional resources that might be interesting to the student in the context of the exercise.

## Step 5: add .meta/config.json

This file contains metadata for the exercise. The metadata includes paths to the test file and the solution files, as well as a list of all the tests and the method they are calling.

## Step 6: update languages/javascript/config.json

An entry should be added to the track's `config.json` file for the new Concept Exercise:

```json
{
  ...
  "exercises": {
    "concept": [
      ...
      {
        "slug": "<slug>",
        "uuid": "<uuid>",
        "concepts": ["<concept-1>"],
        "prerequisites": ["<prerequisite-1>", "<prerequisite-2>"]
      }
    ]
  }
}
```

## Step 7: adding track-specific files

The configuration files may be copied from another exercise. We aim to keep these in sync:

- `.eslintignore`
- `.eslintrc`
- `jest.config.js`
- `package.json`
- `tsconfig.json`
- `yarn.lock`

The `package.json` file must be edited:

```diff
-  "name": "@exercism/typescript-numbers",
+  "name": "@exercism/typescript-<slug>",
-  "description": "Exercism Concept Exercise on <concepts>",
-  "author": "Jane Doe <janedoe@email.info>",
+  "author": "<first-and-last-name> <<git-email>>"
   "version": "1.0.0",
   "license": "MIT",
   "private": true,
   "repository": {
     "type": "git",
     "url": "https://github.com/exercism/language-concepts.git",
-    "directory": "languages/typescript/exercises/concept/numbers"
+    "directory": "languages/typescript/exercises/concept/<slug>"
   },
```

Now create the following three files:

- `<slug>.cs`. the stub implementation file, which is the starting point for students to work on the exercise.
- `<slug>.spec.js`: the test suite.
- `example.js`: an example implementation that passes all the tests.

## Step 8: add analyzer (optional)

Some exercises could benefit from having an exercise-specific [analyzer][analyzer]. If so, specify what analysis rules should be applied to this exercise and why.

## Step 9: custom representation (optional)

Some exercises could benefit from having an custom representation as generated by the [TypeScript representer][representer]. If so, specify what changes to the representation should be applied and why.

## Step 10: add `.meta/design.md`

This file contains information on the exercise's design, which includes things like its goal, its teaching goals, what not to teach, and more. This information can be extracted from the exercise's corresponding GitHub issue.

## Step 11: add .meta/config.json:

This file contains meta information on the exercise, which currently only includes the exercise's contributors.

## Inspiration

When implementing an exercise, it can be very useful to look at the exercises the track has already implemented. You can also check the exercise's [general concepts documents][reference] to see if other languages that have already an exercise for that Concept.

## Inspiration

When implementing an exercise, it can be very useful to look at already implemented TypeScript exercises like the [strings][concept-exercise-strings], [numbers][concept-exercise-numbers] or [promises][concept-exercise-promises] exercises. You can also check the exercise's [general concepts documents][reference] to see if other languages have already implemented an exercise for that Concept.

## Help

If you have any questions regarding implementing the exercise, please post them as comments in the exercise's GitHub issue.

[analyzer]: https://github.com/exercism/typescript-analyzer
[representer]: https://github.com/exercism/typescript-representer
[concept-exercises]: ../exercises/concept/README.md
[how-to-implement-a-concept-exercise]: ../../../docs/maintainers/generic-how-to-implement-a-concept-exercise.md
[concept-exercise-strings]: ../exercises/concept/strings.md
[concept-exercise-numbers]: ../exercises/concept/numbers.md
[concept-exercise-promises]: ../exercises/concept/promises.md
[reference]: ../../../reference/README.md
