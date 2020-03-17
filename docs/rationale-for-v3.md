# Rationale for v3

## Background

Exercism v1 (the original site) had no concept of progression or structure within a language. A track was a bucket of exercises, which students were free to choose from.

In designing v2, we added a structured pathway (core exercises) which aimed to take a student through the basis of what they needed to know for that language, then additional (side) exercises that allowed them to explore other related concepts. This worked great in theory, but a year later, despite a lot of work by maintainers, no tracks have core pathways that do a great job of comprehensively teaching the key concepts.

Why is this?

Firstly, we believe the key reason is that we have encouraged maintainers to reorder their existing tracks, rather than design their tracks from scratch. The original exercises were never designed to teach language concepts - they were just chosen as fun things to experiment with - so shoehorning these into a "curriculum" is a painful, and probably impossible, task for maintainers. Secondly, in our strategies in trying to achieve this, we have focused too much on what was similar about languages, and not looked enough at the key question of "what makes languages unique?"

## Problems with v2

We've been taking stock of this situation recently and spending time trying to work out what the content of an Exercism track should look like in order to fulfil the design goals of v2.

At the moment the journey for a student is:

- Be given an exercise.
- Try something - normally via an incorrect approach grounded from previous language experience.
- Be told what you should have been doing.
- Work out how to redo it that way.

That sometimes works really well. The mentor gives feedback that neither gives away the answer, nor is frustratingly vague, the student realises a new approach, gets a dopamine hit, and memorizes the new technique. This is similar to how many learning natural-language courses (such as Rosetta Stone) work, where you are given a set of images, and some text, and you have to work out which is which. At the start you have no idea, but as you are corrected you start to work out what the words mean, and the resulting dopamine hit locks the knowledge into your mind.

However, in reality on Exercism, this is often a frustrating process for students. For more complex exercises (I'm looking at you, Bob), the student will often have no idea of the idiomatic way to approach it, and so writes a very different and unidiomatic implementation, giving a mentor a huge amount of work to take them on a journey to an idiomatic solution. This can be hugely frustrating for the student, who can't see the end goal enough to be able to understand why they're being taken on the journey, and is a huge piece of work for the mentors (indeed, tracks pretty reliably fail as soon as a complex exercise like Bob is introduced). There are also huge waits between submitting an exercise and receiving feedback, and that break of flow reduces a lot of the positive excitement from learning something. Everything also relies on the mentor knowing exactly what the exercise is aiming for and not taking the student down random roads that are not appropriate to the point in the track. We have tried to solve some of these issues with the automatic analyzers, but the large potential amount of solutions submitted to an exercise, makes this a slow and unwieldy development endeavour (analyzers are hard to write!).

## Moving towards v3

In learning programming languages, it is essential to understand a concept, work out how to apply that concept, and then practice using that concept in different examples.

At the moment I don't feel like we get this right. We let a student experiment and get it wrong, then give some hints, then ask them to work it out from that and maybe receive more hints. I'm not convinced that at the end of an exercise a student really understands what they've learnt, or how to apply it in the future. This is validated by the fact that on later exercises students often make the same "mistakes" they made early on, or don't apply the techniques they've been introduced to.

I believe the key area where we are failing our students is in not having a clearer separation between explaining a concept and giving the student an opportunity to play. At no point does Exercism actually go about teaching the concepts that someone needs to learn a language - we rely on mentors giving the correct hints when a student makes a mistake. I believe we need to rework core and side exercises to achieve this. I propose renaming "core exercises" to Concept Exercises and structuring them so that they introduce a specific concept in the instructions and then challenge the student to use that concept in producing a solution; and renaming "side exercises" to Practice Exercises, which are designed to allow students to solve an arbitary problem, with the aim of them making use of the concepts they have learnt so far.

### Concept Exercises

Concept Exercises should have the following characteristics:

- Each one has a clear learning goal.
- They are language-specific, not generic.
- We use stubs/boilerplate to avoid the student having to learn/handle unnecessary code on exercises.

As Concept Exercises will be smaller and contain boilerplate, they will be much easier to generate auto-analyzers for. By also adding [Representers](https://github.com/exercism/exercism/issues/5079) we believe we can achieve 95% auto-analysis of iterations. We will automatically post feedback both giving students tips when they submit an approvable solution, but also providing guidance where the iteration is not approvable. Furthermore, our analyzers will automatically approve any approvable iteration, removing the tension/frustration that is often caused by that power lying with mentors. For the small percentage of solutions that cannot be automatically analyzed, we will automatically approve the exercise letting the student move on, and then provide feedback later once their solution has had a mentor look at it - feedback that will then be stored to be automatically applied to future similar solutions.

We will also be changing some of the rules around unlocking. Multiple Concept Exercises will be able to be unlocked at the same time, and Concept Exercises can unlock more complex, related Concept Exercises (e.g. Ruby might have a blocks Concept Exercise, which when completed unlocks exercises on Procs, Lambdas, block_given?, etc).

We will also change the way unlocking works, so rather than being explicitly stated in track configurations, unlocking will be implicitly worked out by the tagging of "concepts learnt" on Concept Exercises, and "prerequisite concepts" on Practice Exercises. Once enough Concept Exercises have been completed that all the prerequisite concepts for a Practice Exercise has been been fulfilled, that Practice Exercise will be unlocked.

Concept Exercises will not share a generic base such as problem-specifications. However, we will write language-agnostic introductions to programming concepts, with links to language-generic resources on that topic, which we will ask tracks to use. For example, we might define a concept of "Enumeration" and provide a short introduction that explains how enumeration differs from loops with a link out to a good article (either internal to Exercism or elsewhere on the web). Tracks implementing an exercise on enumeration can then include this common paragraph, before explaining their own language-specific semantics and syntax.

### Practice Exercises

Practice Exercises should be story-based rather than concept-based. They should provide an interesting challenge, which a student can try and solve using the concepts they have been taught. The existing set of Exercism exercises are generally great Practice Exercise candidates.

There will be no "approval" for Practice Exercises. A student solves the exercise against the tests and submits it. They can then choose to publish, or submit further iterations.

We will provide automated analysis of Practice Exercises, but only as tips for how things could be made more idiomatic, referring back to the Concepts previously taught. We expect this analysis to take lower priority than on Concept Exercises (as students are not blocked) so it should be seen as a nice bonus feature, not something that the exercise or student should rely on.

Once submitted, the student will have the option to browse different "Approaches". These will be articles written by our community explaining the various common (or possible) approaches to this exercises, outlining the pros/cons and tradeoffs of the approach. We will link to curated examples of student-submitted solutions that adhere to the various approaches for students to explore further. We will crowd-source these approaches, giving the community the option to become Exercism Authors, but will have a review process to ensure a solid quality-barrier. For exercises that do not yet have "Approaches" written, we will automatically group solutions by AST analysis, for students to browse.

Finally, students can ask the community a question about their exercise. e.g. "I've read this approach but I don't understand how XYZ works and how I can apply it to my code". Community members can then reply to the student with their thoughts - a little like the community interactions on v1. We will make it explicit to students that when they ask a question they are not getting "mentor-quality" feedback, but tapping into the hive-mind of the community. This will also help us add a fair gatekeeping level for mentors. Community-mentors who answer questions helpfully, and have therefore proven their ability to communicate empathetically and helpfully, will have the option to become mentors.

We intend for Practice Exercises to be shared across tracks, as they currently are, but we need to put together solutions for some of the challenges the Problem Specifications repository has encountered recently. We will be writing up a separate issue for discussion about that in the next few days.

### Practice Mode

We also intend to maintain Practice Mode ([formerly Independent Mode](https://exercism.io/blog/independent-mode-becomes-practice-mode)), but encourage it as a space for practicing a language that a student knows, rather than learning a new language. It will contain only Practice Exercises and we will make the prerequisite topics on each exercise clearer for those who want to practice certain concepts. Students will be able to ask questions to the community about their exercises, but not directly to mentors.
