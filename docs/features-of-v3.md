# V3 Features

## Restructured tracks

_Talk about changes to Concept/Practice exercises, pathways, and unlocking_
We are restructuring tracks to build new pathways containing "Concept Exercises".

## Concept Exercises

_Talk about what makes a good concept exercise and why_

These exercises teach the key concepts that someone needs to know when learning a langauge, unlocking other Concept and Practice Exercises as students progress. Concept Exercises will have automated feedback and approval, with no delays for mentors.

## Approaches

_Talk about what approaches look like and how this will work_

We will be automatically grouping similar solutions to Practice Exercises into Approaches. These will be supported with community-sourced articles expounding each approach, discussing its pros, cons, and potential usages.

## Representers

One of the problems faced by the Exercism community is how to provide meaningful feedback to many people providing many solutions to many different exercises. To help reduce the scope of this potentially enormous task, solutions are normalized into "representations". For example, out of the most recent 500 submissions to the TwoFer exercise on the Ruby track, about 380 of them would be considered unique (if you normalize for trivial things like code formatting and comments). If you normalize them even further (by normalizing things like function or variable names), that number gets even smaller, so there might be only 250 unique approaches. If the Exercism community can provide some feedback on those 250 approaches, then hope is that we will have valid feedback prepared for ~99% of all future submissions for TwoFer. With the Concept Exercises the solution space will be even smaller because Concepts Exercises will be deliberately designed to be solved in a specific way.

A _representer_ is a program that has the responsibility of taking a solution and returning a normalized representation of it.

A _representation_ is an extraction of a solution to its essence with normalized names, comments, spacing, etc. but still uniquely identifying the approach taken. Two different ways of solving the same exercise must not have the same representation.

The simplest representer is one that merely returns the solution's source code. However, as our goal is to have the same representation for solutions only differing in non-essential details, the representer should apply one or more normalizations.

Once we have a normalized representation for a solution, a team of vetted mentors will look at the solution and comment on it (if needed). These comments will then automatically be submitted to each new solution with the same representation. A notification will be sent for old solutions with a matching representation.

## In-browser coding

_Talk about what test runners_

We will be adding the ability to solve exercises within the browser. The CLI will remain as a first-class citizen, but v3 will allow people to work their way through a track without having to install anything.

## Improved profiles

Profiles will be getting an upgrade - with contributions throughout the site reflected in them. Mentoring, writing exercises, contributing to tracks, and maintaining parts of Exercism will all feature on profiles. Over time, contributing more will also have tangible improvements to your experience, with bumps up mentor queues and access to early testing features.
