# Concept Exercises

Concept Exercises replace V2's Core Exercises. They are exercises designed to teach specific programming concepts.

## What do we mean by programming concepts?

By concepts we mean things that a programmer would need to understand to be fluent in a language. We care specifically about how languages are **different**. What do I need to learn differently about numbers in Haskell to numbers in Ruby to be able to work with numbers in those languages. Two questions that we have felt useful to ask to establish this are:

- If someone learnt Ruby, and someone learnt Haskell, what are the things that the two people learnt that are different?
- If a Ruby programmer learnt Haskell, what new concepts would they have to learn, what knowledge would they have to disregard, and what syntax would they have to remap?

By teaching concepts we aim to teach fluency.

## What do we mean by "fluency?"

By "Fluency", we mean: Do you **get** that language? Can you reason in that language? Do you write that language like a "native" programmer writes in it? Fluency is the ability to express oneself articulately in a particular language.

"Fluency" is different to "Proficiency", where we use proficiency to mean: Can you write programs in the language (e.g. can you nav stdlib, docs, compose complex code structures).

Exercism focuses on teaching Fluency not Proficiency. We aim to teach people to understand what a makes a language unique and how experienced programmers in that language would reason about - and solve - problems.

## How are Concept Exercises designed and structured?

Concept Exercises must have the following characteristics:

- Each one has a clear learning goal.
- They are language-specific, not generic.
- Stubs/boilerplate are used to avoid the student having to learn/write unnecessary code on exercises.

Concept Exercises are **not** mentored. When a user submits a submission that gets the tests passing for a Concept Exercise, we check for an Analyzer or Representer to give feedback. If none is found, then the solution is approved. This shifts the burden of teaching to the exercise, which must provide a clear pathway to learning the concept that needs learning.

Exercises are unlocked based on concepts taught and learnt. Each Concept Exercise must teach one or more concepts. It may also have prerequisites on Concepts, which means it will not be unlocked until Concept Exercises teaching those prerequisite concepts have been completed.

Concept Exercises do not share a common base like Practice Exercises do in the `problem-specifications` repository. Instead they "share" concepts that they are teaching with other languages. This repository aims to list all of those concepts and provide information about the concept that maintainers can use as the basis for their own languages. Each concept should also link to the implementations in different languages. Maintainers are free to copy and paste from each others repositories, and then edit to make things specific to their tracks, but such copy-and-pastes should be considered hard-forks.

For example, we might define a concept of "Classes" and provide a short introduction that explains what a class is, how it fits with objects, state, etc. We might include a link to a good article introducing OOP and classes. Individual tracks implementing an exercise on Classes can then include this introductory text, making any additions that explain their language-specific semantics and syntax.

## Exercise Structure

An exercise has the following files. In the browser they will show at the relevant times. When used via the CLI, the introduction and instructions will be concatenated along with the track's CLI instructions into a README.md, which will sit alongside a HINTS.md.

### `.docs/introduction.md`

This file provides an introduction to the concept. It should be explicit about what the exercise teaches and maybe provide a brief introduction to the concepts, but not give away so much that the user doesn't have to do any work to solve the exercise.

See the C# floating-point-numbers exercise's [introduction.md file](./languages/csharp/concept-exercises/numbers-floating-point/.docs/introduction.md) for an example.

### `.docs/instructions.md`

This file provides instructions for the exercise. It should explicitly explain what the user needs to do (define a function with the signature `X.y(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

See the C# floating-point-numbers exercise's [instructions.md file](./languages/csharp/concept-exercises/numbers-floating-point/.docs/instructions.md) for an example.

### `.docs/hints.md`

If the user gets stuck, we will allow them to click a button requesting a hint, which shows this file. We will softly discourage them using it.

The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any user. They might link to the docs of the functions that need to be used.

See the C# floating-point-numbers exercise's [hints.md file](./languages/csharp/concept-exercises/numbers-floating-point/.docs/hints.md) for an example.

### `.docs/after.md`

Once the user completes the exercise they will be shown this file, which gives them any bonus information or further reading about the concept taught.

See the C# floating-point-numbers exercise's [after.md file](./languages/csharp/concept-exercises/numbers-floating-point/.docs/after.md) for an example.

### `.docs/debug.md`

This file explains how a user that is coding in the browser can still do "debugging."

See the C# floating-point-numbers exercise's [debug.md file](./languages/csharp/concept-exercises/numbers-floating-point/.docs/debug.md) for an example.

