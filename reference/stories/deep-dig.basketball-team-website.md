# Basketball team website

## Story

You're working on a website for basketball team. The website displays lots of data, but unfortunately what is displayed changes often. As the data is stored in a single object, this is not hard to do, but it is time-consuming and boring. You've therefore decided to automate things.

## Tasks

These are examples of tasks that fit the story of automating the website:

- Your goal is to allow the content team to select which data to display by themselves. They'll do this by specifying a path to the data to be displayed in the website's content editor. Your task is to create a function extract that takes an object and a path, separated by a dot character. Return the value at that path, or a default value.

## Additional Tasks

The story can be continued, for example:

- The content team is very excited about this new functionality, and has already requested a new feature to be added. They'd like to be able to display data of specific elements in an array.

## Implementations

- [JavaScript 1-b (research)][implementation-javascript-research-1-b]
- [JavaScript 2-b (research)][implementation-javascript-research-2-b]

## Reference

- [`types/dictionary`][types-dictionary]

[types-dictionary]: ../types/dictionary.md
[implementation-javascript-research-1-b]: https://github.com/exercism/research_experiment_1/tree/master/exercises/javascript-1-b/README.md
[implementation-javascript-research-2-b]: https://github.com/exercism/research_experiment_1/tree/master/exercises/javascript-2-b/README.md
