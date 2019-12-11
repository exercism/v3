# Exercism - V3

## An Introduction

This repository is the work-in-progress space for Exercism v3, and beyond that will be a permanent home for information about languages on Exercism, and a space for maintainers to discuss changes that need to occur across tracks.

Maintainers can consider this the new "home of maintainers", replacing `problem-specifications` in that role.

In the early "work-in-progress" phase, this repository also holds new exercises that are being created across tracks, in order to elicit a wider set of voices for discussion on these new types of exercises. Once a track is no longer a "work-in-progress," its exercises will be moved to the track's repository.

## Concepts

The key part of the content in this repository is the idea of "Concepts" - things that a programmer would need to understand to be fluent in a language. We care specifically about how languages are **different**. What do I need to learn differently about numbers in Haskell to numbers in Ruby to be able to work with numbers in those languages. Two questions that we have felt useful to ask to establish this are:

- If someone learnt Ruby, and someone learnt Haskell, what are the things that the two people learnt that are different?
- If a Ruby programmer learnt Haskell, what new concepts would they have to learn, what knowledge would they have to disregard, and what syntax would they have to remap?

### What do we mean by "fluency?"

By "Fluency", we mean: Do you **get** that language? Can you reason in that language? Do you write that language like a "native" programmer writes in it? Fluency is the ability to express oneself articulately in a particular language.

"Fluency" is different to "Proficiency", where we use proficiency to mean: Can you write programs in the language (e.g. can you nav stdlib, docs, compose complex code structures).

Exercism focuses on teaching Fluency not Proficiency. We aim to teach people to understand what a makes a language unique and how experienced programmers in that language would reason about - and solve - problems.

## Concept Exercises

Concept Exercises must have the following characteristics:

- Each one has a clear learning goal.
- They are language-specific, not generic.
- Stubs/boilerplate are used to avoid the student having to learn/write unnecessary code on exercises.

Concept Exercises are **not** mentored. When a user submits a submission that gets the tests passing for a Concept Exercise, we check for an Analyzer or Representer to give feedback. If none is found, then the solution is approved. This shifts the burden of teaching to the exercise, which must provide a clear pathway to learning the concept that needs learning.

Exercises are unlocked based on concepts taught and learnt. Each Concept Exercise must teach one or more concepts. It may also have prerequisites on Concepts, which means it will not be unlocked until Concept Exercises teaching those prerequisite concepts have been completed.

Concept Exercises do not share a common base like Practice Exercises do in the `problem-specifications` repository. Instead they "share" concepts that they are teaching with other languages. This repository aims to list all of those concepts and provide information about the concept that maintainers can use as the basis for their own languages. Each concept should also link to the implementations in different languages. Maintainers are free to copy and paste from each others repositories, and then edit to make things specific to their tracks, but such copy-and-pastes should be considered hard-forks.

For example, we might define a concept of "Classes" and provide a short introduction that explains what a class is, how it fits with objects, state, etc. We might include a link to a good article introducing OOP and classes. Individual tracks implementing an exercise on Classes can then include this introductory text, making any additions that explain their language-specific semenatics and syntax.

### Exercise Structure

An exercise has the following files. In the browser they will show at the relevant times. When used via the CLI, the introduction and instructions will be concatenated along with the track's CLI instructions into a README.md, which will sit alongside a HINTS.md.

#### `.docs/introduction.md`

This file provides an introduction to the concept. It should be explicit about what the exercise teaches and maybe provide a brief introduction to the concepts, but not give away so much that the user doesn't have to do any work to solve the exercise.

See the C# floating-point-numbers exercise's [introduction.md file](./languages/csharp/concept-exercises/floating-point-numbers/.docs/introduction.md) for an example.

#### `.docs/instructions.md`

This file provides instructions for the exercise. It should explicitly explain what the user needs to do (define a function with the signature `X.y(...)` that takes an A and returns a Z), and provide at least one example usage of that function. If there are multiple tasks within the exercise, it should provide an example of each.

See the C# floating-point-numbers exercise's [instructions.md file](./languages/csharp/concept-exercises/floating-point-numbers/.docs/instructions.md) for an example.

#### `.docs/hints.md`

If the user gets stuck, we will allow them to click a button requesting a hint, which shows this file. We will softly discourage them using it.

The file should contain both general and task-specific "hints". These hints should be enough to unblock almost any user. They might link to the docs of the functions that need to be used.

See the C# floating-point-numbers exercise's [hints.md file](./languages/csharp/concept-exercises/floating-point-numbers/.docs/hints.md) for an example.

#### `.docs/after.md`

Once the user completes the exercise they will be shown this file, which gives them any bonus information or further reading about the concept taught.

See the C# floating-point-numbers exercise's [after.md file](./languages/csharp/concept-exercises/floating-point-numbers/.docs/after.md) for an example.

## Repository Structure

We will also be changing some of the rules around unlocking. Multiple Concept Exercises will be able to be unlocked at the same time, and Concept Exercises can unlock more complex, related Concept Exercises (e.g. Ruby might have a blocks Concept Exercise, which when completed unlocks exercises on Procs, Lambdas, block_given?, etc).

We have also changed the way unlocking works. Rather than being explicitly stated in track configurations, unlocking is implicitly worked out by the tagging of "concepts learnt" on Concept Exercises, and "prerequisite concepts" on Practice Exercises. Once enough Concept Exercises are completed so that all the prerequisite concepts for a Practice Exercise are fulfilled, that Practice Exercise is unlocked.

Concept Exercises don't share a generic base such as `problem-specifications`. However, we write language-agnostic introductions to programming concepts, with links to language-generic resources on that topic, which we ask tracks to use. For example, we have defined a concept of "Enumeration" and provided a short introduction that explains how enumeration differs from loops with a link out to a good article (either internal to Exercism or elsewhere on the web). Tracks implementing an exercise on enumeration can then include this common paragraph, before explaining their own language-specific semantics and syntax.

## Repository structure

Things are grouped into several subdirectories:

- **concepts:** This directory contains files discussing **programming** concepts. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.
- **languages:** A directory for each language containing a README.md that outlines the concepts that are necessary to learn to become fluent in those languages. Each language has a config.json and the following subdirectories:

  - **transitions:** Contains a file for each pair of languages, where someone with a background in X learns new language Y. These should explain the concepts that needed to be remapped/learnt. The files in this directory intially aim to help inform us about both the langauge-agnostic and language-specific files, but will hopefully also in the long-run provide custom pathways for people learning languages.
  - **info:** Files containing information that the implementer might find useful. These may be files on language-specific elements of programming concepts or notes on the idiomatic way to use tooling. For now, maintainers are encouraged to organise this directory however they feel best, and we may formalise a structure later.
  - **concept-exercises:** The WIP concept exercises for that language.
  - **config.json** The WIP config.json for a track, with the practice exercises removed to avoid noise.

- **tooling** This directory contains documents on tooling that is common across languages (either due to a shared ecosystem - such as JS/TS/Clojurescript - or a shared platform - such as .net or JVM).
- **types:** This directory contains files discussing programming types. These documents should provide guidance, limitations, and links for maintainers who need to expound those ideas on their tracks.

## How to get started

### Filling out concepts for your language

To know which Concept Exercises should exist, a list of the language's concepts should first be compiled. There are various resources to use when compiling this list: books, (official) documentation, your own experiences and the concepts listed in this repository.

As many languages have the same concepts (but possibly implemented differently), it can greatly help to look at what other languages have already done in this regard. See the [C# README.md](./languages/csharp/README.md) for an example.

It can also be helpful to group related concepts. For example, the Classes, Polymorphism and Inheritance concepts are all object-oriented concepts, which can then be grouped as "Object-oriented concepts".

Filling out the concepts will be iterative, it is hard to get this right immediately. While creating concepts exercises, you'll probably find missing concepts or perhaps want to split out concepts.

### Writing your first concept exercise

Once a list of concepts has been compiled, these then need to be translated into Concept Exercises. Writing your first concept exercise begins with choosing a concept to write an exercise for. Good first concept candidates are concepts that involve working with basic types, like strings and numbers.

Having chosen a concept, look for the concept's description in this repository. This file should have references to all tracks that have implemented the exercise. You can use these existing implementations as a starting point for your first concept exercise, which you can then tailor to your specific language.
