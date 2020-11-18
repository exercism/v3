In this exercise you'll be implementing a simulation of encounters.
This will familiarise you with the basics of multiple dispatch, Julia's main paradigm.
Everything you might need to know can be found in the [Methods](https://docs.julialang.org/en/v1/manual/methods/) chapter of the Julia Manual.

In general, encounters involve one party `a` meeting another party `b` and reacting to it.

```julia
encounter(a, b) = "$(name(a)) meets $(name(b)) and $(meets(a, b))."
```

At first, we will simulate what happens when cats and dogs meet.

## 1. Define structs `Cat` and `Dog`, and corresponding methods of `name`

## 2. Define what happens when cats and dogs meet

Implement `meets` methods for the following encounters:

- When two dogs meet, they **sniff** each other.
- When a cat meets a dog, it **hisses** at the dog.
- When a dog meets a cat, it **chases** the cat.
- When two cats meet, they **slink**.

But what happens if they encounter a different pet on their walk, like a [horse](https://www.dw.com/en/horse-takes-daily-stroll-through-frankfurt-without-owner/a-47833431)?

## 3. Define a fallback reaction for encounters between pets

First, add an abstract type `Pet` to your implementation, of which `Cat` and `Dog` are subtypes.

- If a pet meets another pet that it doesn't recognize, it **is cautious**.

There are many other things that pets may encounter that aren't pets: cars, humans, plants, natural disasters, asteroids… What happens then?

## 4. Define a fallback if a pet encounters something it doesn't know

- If a pet meets something it has never seen before, it **runs away**.

## 5. Define a generic fallback

There are many other encounters that could occur in our simulation.
A car meets a dog, two electrons encounter each other and interact, ….
It is impossible to cover all encounters in advance, therefore we will implement a generic fallback.

- If two unknown things or beings encounter each other, **nothing happens**.
