# Frequently Asked Questions

## What is v3?

v3's main goal is to improve the learning experience for students, so that when they complete a track they will be able to program in that language in an idiomatic way.

Exercism v2 currently focuses heavily on discovering lots of various information from a relatively complex exercise - something which can be challenging and confusing both for students and those trying to mentor them. In contrast, in v3, we are explicitly designing exercises to teach these Concepts. These new exercises are called Concept Exercises. Their goal is to teach one just Concept, providing enough challenge to familiarse the student with the idea of the Concept and enough mental scaffolding for them to know where to discover more information. Concept Exercises are therefore designed with a specific purpose in mind, whereas in v2 the learning experience was more incidental.

v3 also adds new features and changes to the user experience, such as In-browser coding, Approaches, better automated-feedback, and an improved mentoring system.

For a more in-depth explanation on v3, please check the following links:

- [Introduction to v3][readme]: An introduction to v3.
- [The features of v3][features]: A deeper dive into the changes we're making to v3.
- [Rationale for v3][rationale]. An explanation about the rationale behind some of our our design decisions for v3.

## What is the role of maintainers in the v3 transition?

The key role for maintainers is in designing and managing their tracks. We don't expect maintainers to do all the work of creating everything. We want to crowd-source work as much as possible. Maintainers' key role is in working out how their track can best teach a student, and designing and reviewing exercises. We hope that maintainers will also enjoy creating some of the actual exercise themselves, but the load of this should be spread amongst the wider community.

For more information, see the [maintainers README][maintainers].

## What are Concept Exercises?

A Concept Exercise is an exercise that is designed to teach one Concept, and nothing more. Concept Exercises are thus designed with a specific purpose in mind, whereas in v2 the learning experience was more incidental.

As opposed to the v2 exercises, Concept Exercises are _not_ based on shared data anymore. The reason for that is that we don't want any cross-track dependency issues, where changing the shared data makes sense for some tracks, but not for other tracks. The Concept Exercises can thus be tailored completely to best fit their track.

For more information, see the [concept exercises document][concept-exercises].

## What is a Concept?

A Concept describes a feature of a programming language. Examples are "Encapsulation", "Conditionals", and "Functions." In other words, Concepts are what one would need to learn if one was to learn a new (programming) language.

Although there will obviously be cross-over between languages, Concepts are indented to be language-specific. What is it about **your** language that a junior programmer would need to learn, unlearn and remap to be able to successfully transition to it.

Each track will compile a list of Concepts for their track's language, for which Concept Exercises will be created.

## How to determine a language's Concepts?

Determining a language's Concepts will be a collective effort. Each track will start with maintainers selecting a couple of existing v2 exercises and analyze them to determine which Concepts one needs to know to get to an optimal solution. The concepts gathered thus are compared against each other and used to compile a list of Concepts. The end result is a list of the Concepts for that track's language.

For more information, see the [determining concepts documentation][determining-concepts].

## How should Concepts be ordered?

The only Concept Exercises a student can start are ones that have no prerequisite Concepts (very, very few will be like this) or where the prerequisites are fulfilled already.

Therefore, the order/hierarchy of the Concepts themselves doesn't matter, other than as an organisation tool for you. What matters is just designing lots of exercises that teach lots of Concepts, and then thinking through what each of those exercises has as it's own prerequisites. Out of that a tree (or many parallel trees) will form, which is how things will be presented to the student.

## How to design a Concept Exercise?

Each Concept Exercise will in principle be designed from scratch. The basis will be formed by a single Concept. When designing the Concept Exercise, there are lots of things to consider:

- What should be taught?
- What shouldn't be taught?
- What are the prerequisite Concepts?
- ...

It can be very helpful to look at existing Concept Exercises, and see how they were designed.

Once you have a firm idea on what precisely the Concept Exercise will be teaching, the next step is to [write a GitHub issue for the Concept Exercise][writing-a-concept-exercise-github-issue]. This issue should contain very detailed, actionable steps an outside contributor could work with. This will allow us to crowd-source the actual implementation of the Concept Exercises.

Not all Concept Exercises have to designed from scratch, it is perfectly reasonable for tracks to adapt Concept Exercises that were created for other tracks. The important part here is that when adapting existing Concept Exercises, they should be modified to best fit the track that is adapting them.

For some basic Concept Exercises, it might make sense to design them to teach two Concepts. The additional Concept must be one that is very simple and can be taught well in the context of the primary Concept. As an example, a basic "Integers" exercise could decide to also teach the basic "Conditionals" Concept. Note though that have more than one Concept should be the exception, not the rule.

## What files make up a Concept Exercise?

A Concept Exercise has two types of files. The first are the general, language-agnostic files, which are required for all tracks. You can find a description of these files in the generic [how to implement a concept exercise][how-to-implement-a-concept-exercise] section. The second type of files are language-specific, and as such are different for each track. A description of these language-specific files will be added to each track. Here are the [C# track's language-specific instructions][how-to-implement-a-concept-exercise-csharp].

## How to implement a Concept Exercises?

Implementing a Concept Exercise should in principle be a case of following the detailed instructions in the Concept Exercise's corresponding GitHub issue.

If your track has already implemented one or more Concept Exercises, we highly recommend you taking a look at them too, to see how they were implemented.

## How many Concept Exercises should be created?

We're aiming for about 50 Concept Exercises, although that does depend on the language and how many concepts it has (some languages are more complex than others). There is no fixed number here, it all depends on the language and how many Concepts it has. For the more complex languages with lots of concepts, the number could easily go to 100 Concept Exercises.

As each Concept Exercise will have detailed instructions in the form of a GitHub issue, we're hoping to crowd-source the actual implementation of the Concept Exercises. Designing the Concept Exercises and creating the corresponding GitHub issues is a responsibility of the maintainers though.

## Is there a 1-1 mapping from Concept to Concept Exercise?

In general: yes. However, for very complex Concepts, it can make sense to have multiple smaller Concept Exercises instead of one huge Concept Exercise.

We highly recommend reading [this thread][concept-to-exercise-mapping] for a nice discussion on this topic.

## How will mentoring work for Concept Exercises?

There will be no mentoring on Concept Exercises in v3. The process will be fully automated, and will work as follows:

1. The student submits a solution for a Concept Exercise which prequisites have been met (i.e. the student has completed Concept Exercises teaching the prerequisite Concepts).
1. The solution is tested automatically to verify it passes all tests. This is done by the [test runner][test-runner]. If a test fails, details of the failing test are shown to the student. The student must submit again (step 1) in order to continue.
1. The [representer][representer] creates a representation of the solution. If there are any comments linked to that representation, those comments are presented to the student. The student must submit submit again (step 1) in order to continue.
1. The [analyzer][analyzer] analyzes the solution. If there are comments, those comments are presented to the student. If one of the comments indicates that the student should not be allowed to proceed, the student must submit submit again (step 1) in order to continue.
1. The student's solution is approved and the student can work on another Concept Exercise which prerequisites have been met.

## What happens to the existing v2 exercises?

At first: nothing. We're currently focusing on creating the Concept Exercises. At a later stage, we'll turn out attention to Practice Exercises, which is what the existing v2 exercises will be converted to.

Practice Exercises will be far less structured than Concept Exercises and not have a particular goal assigned to them. This makes them perfect for exploring different options. We'll add features to highlight different approaches.

Students will have the option to request mentoring for Practice Exercises.

For more information, see the [practice exercises section][practice-exercises].

## Besides creating Concept Exercises, what more should we work on?

Concept Exercises are obviously our main focus. However, v3 also requires the following, track-specific tooling:

- [Test runner][test-runner]: verify a solution passes all the tests.
- [Representer][representer]: takes a solution and returns a normalized representation of it.
- [Analyzer][analyzer]: provide automated feedback on solutions.

The first two tools are must haves for v3, whilst the last one is in essence optional (although extremely useful).

Some tracks already have a working implementation of one or more of these tools. You can check this in your track's README file.

## What are the plans for the problem-specifications repository?

The problem-specifications repository will remain a very valuable resource to create/generate Practice Exercises from. That said, there are some issues we had with the problem-specifications repository, in particular the fact that we had some cross-track conflicts of interest. We have several ideas on how to fix these, which we'll be discussing and implementing at a later time. For now, the problem-specifications repository will remain locked and will only accept bugfixes.

## Will the v3 repository replace the existing track repositories?

Short answer: no. For the moment, we have all the v3 tracks in this single monorepo for one reason: to make it easier for tracks to borrow/be inspired by what other tracks are doing. Once a track has transitioned to v3, we'll be moving the track's data back over to the track's repository.

## How should a track organize the v3 transition?

The basic communication channel for maintainers is Slack, and in particular the maintainer-only `#maintaining-<track>` channel. For communication with everyone else, GitHub issues should be used, for obvious reasons (discoverability, familiarity, etc.). We've also enabled the GitHub Projects feature on the v3 repo, so you can organize your track's v3 transition there too.

## Should I use citations

Yes! Please use citations in any reference documentation (files under `**/reference/**`). We are using the [Science of Computer Programming citation style][socp-citation-style]. This means that when citing website, please use this format:

> $TITLE, $SOURCE. ($PUBLISHED_DATE). <$URL> (accessed $ACCESSED_DATE).

For example:
> Actor model, Wikipedia. (2020). <https://en.wikipedia.org/w/index.php?title=Actor_model&oldid=939106706> (accessed February 29, 2020).


## More questions?

Do you have more questions or feel that something is missing? Please let us know by [opening a GitHub issue][open-github-issue].

[readme]: ../../README.md
[features]: ../features-of-v3.md
[rationale]: ../rationale-for-v3.md
[maintainers]: ./README.md
[concept-exercises]: ../concept-exercises.md
[determining-concepts]: ./determining-concepts.md
[how-to-implement-a-concept-exercise]: ./generic-how-to-implement-a-concept-exercise.md
[how-to-implement-a-concept-exercise-csharp]: ../../languages/csharp/reference/implementing-a-concept-exercise.md
[writing-a-concept-exercise-github-issue]: ./writing-a-concept-exercise-github-issue.md
[concept-to-exercise-mapping]: https://exercism-team.slack.com/archives/CC5DGSAG6/p1579722161067300
[practice-exercises]: ../rationale-for-v3.md#practice-exercises
[test-runner]: https://github.com/exercism/automated-tests/blob/master/docs/interface.md
[representer]: https://github.com/exercism/automated-analysis/blob/master/docs/representers/introduction.md
[analyzer]: https://github.com/exercism/automated-analysis/blob/master/docs/analyzers/interface.md
[concepts]: ../../reference/README.md
[concept-exercise-issue-example]: https://github.com/exercism/v3/issues/33
[open-github-issue]: https://github.com/exercism/v3/issues/new/choose
[socp-citation-style]: https://paperpile.com/s/science-of-computer-programming-citation-style/#examples-website-citations
