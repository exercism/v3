# Klingon Translation Service

## Story

In this exercise you'll be providing a `TranslationService` where paid members
have some quality assurance.

You have found a magical translation API that is able to fulfill any
translation _request_ in a reasonable amount of time, and you
want to capitalize on this.

The magical API has a very minimal interface:

### Fetching a translation

`api.fetch(text)` fetches the translation of `text`, returning two values:

- `translation`: the actual translation
- `quality`: the quality expressed as a number

If there is no translation available (because it has not been requested yet),
the API throws an error. This also happens if a piece of text is untranslatable.

### Requesting a translation

`api.request(text, callback)` requests the translation of `text`, calling the
`callback` once it's ready, without a value.

The `request` API is unstable, which means that sometimes the API will call the
`callback` with an error. If that happens, it is okay to re-request.

### ⚠ Warning! ⚠

Because of some previous users being lazy when programming, always requesting a
translation, without even checking if the text was already translated, the API
returns an error if the text has already been translated ánd blocks all access
completely, forever.

### Tasks

These These are examples of tasks that fit the story of the translation service:

- Fetch a translation, ignoring the quality
- Fetch a batch of translations, all-or-nothing
- Request a translation, retrying at most 2 times
- Fetch a translation, inspect the quality, or request it

## Terminology

These are recommendations, not rules, for recurring terminology in the instructions (including stub commentary)

- The _injected dependency_ is called the `TranslationService`, or API
- The service is async, but this is never mentioned
- A failure on a Future is **the API throws an error**
- A failure on a Callback is **the API will call `callback` with an error**

All the translation texts are Klingon to English.

## Implementations

- [JavaScript: promises][implementation-javascript] (reference implementation)

## Reference

- [`types/promise`][types-promise]
- [`types/future`][types-future]
- [`types/string`][types-string]
- [`types/number`][types-number]

[types-promise]: ../types/promise.md
[types-future]: ../types/future.md
[types-string]: ../types/string.md
[types-number]: ../types/number.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/promises/.docs/instructions.md

## N.B.

The correct translation of `'arlogh Qoylu'pu'?` is **How many times has it been heard?**.
