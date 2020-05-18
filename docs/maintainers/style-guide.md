# Formatting and Style Guide

This document records the style guide that should be followed for all documents in this repository. Maintainers should reference the Style Guide when reviewing pull requests. When a language also includes a style guide, those preferences will take precedence over the rules listed here.

## Language

Content should be written using American English, which differs from British English in a [variety of ways][comparison-of-american-british-english].

## Headers

- Use level 2 headers, e.g. `##`
- Do not use top-level headers, e.g. `#`
- Do not use leading whitespace in the file.

## Empty Documents

If a document is empty, i.e. there is no `source.md` content, then do not check it in.

## Links

Please use [reference links](https://spec.commonmark.org/0.29/#reference-link), which are defined at the bottom of the Markdown file, mapped to a reference slug, and referenced by that slug in the text.

This method makes maintenance easier, since the link must only be updated once.

Example:

```
I have a paragraph of text that links to the same page twice.

The [first link][indirect-reference] in one sentence.

Then, another sentence with the [link repeated][indirect-reference].

[indirect-reference]: https://example.com/link-to-page
```

Links should always have anchor text, instead of putting the URL itself into the text. For example, the following does not use anchor text, and is harder to read.

```
If you want some more information, please visit https://google.com.
```

Using anchor text and linking it is easier to understand and contextualize.

```
If you want some more information, [Google][google-search-link] is a useful resource.

[google-link]: https://google.com
```

Which renders as, "If you want some more information, [Google][google-search-link]".

## Code

Whenever one refers to code elements (e.g. functions or keywords), the code should be wrapped in backticks:

```
- The `printf()` function writes to the console.
```

Which renders as:

- The `printf()` function writes to the console.

More complex code (e.g. multiline code) should be wrapped in triple backticks. A [language identifier][language-identifiers] should be specified after the opening triple backticks to enable syntax highlighting:

````python
```python
# Define a variable
album = "Abbey Road"
```
````

When possible, format code in a way that is most relevant to the environment it is being presented in. If available, please reference the docs for your language.

For example, Python has the REPL (read-eval-print loop), which allows a programmer to type code directly into a terminal. If a user was debugging some code, or running a function locally to test/observe how it works, they are more likely to use a REPL to do so, since it is more convenient to type interactively there. In this situation, we prefer formatting example code as if it was being typed into the REPL, with a leading `>>>`:

```python
# This is the expected output a student would see while testing a function they wrote.
>>> print(extract_message("[INFO] Logline message"))
'Logline message'
```

In other cases, it makes more sense to leave code formatted as if it was in a `.py` file. Here, the student benefits by being presented with something that mimics how they would write the code themselves.

```python
# presenting how functions are defined in Python:
def sample_func(argument1):
    pass
```

A quick way to distinguish between the two cases when it's unclear - if this is code the student can run in a terminal, format it that way. If it's code that Exercism might run (in tests, library code the student will write, etc), default to formatting it as runnable code.

## Language Code Style

Please consult each language's docs folder for more information on the preferred style conventions for that language. All exercises of a language should use a consistent coding style.

## Auto formatting

We're using [prettier][prettier] to ensure that Markdown and JSON documents are consistently formatted. This gives us the following benefits:

- No formatting discussions.
- Great editor/IDE integration so files can be formatted on save.
- Easy to add CI checks for formatting.
- Easy to automatically format files using a script.

All the above will greatly help reduce churn in reviews, whch is frustrating for both the reviewer and the reviewee.

### Using prettier to format Markdown and JSON documents

There are a couple of ways in which you can format Markdown documents using prettier:

#### 1. Using editor integration

Prettier can be integrated with many [editors][prettier-editor]. This works especially well if the editor supports automatic formatting on save.

#### 2. Using a GitHub comment

If you add a comment to a GitHub PR that contains the text `/format`, a GitHub workflow will format all Markdown and JSON documents in the PR using prettier. Any formatting changes made by prettier will automatically be committed to the PR's branch. This also works for forks that have [enabled maintainers to edit the fork's PR][allowing-fork-pr-changes].

#### 3. Using a script

Open a command prompt at the root of the repository and then run:

- On \*NIX and MacOS: `./bin/format.sh <path-to-file-to-format>`
- On Windows: `.\bin\format.cmd <path-to-file-to-format>`

Note: this requires [Node.js][nodejs] to be installed on your machine.

## Miscellaneous

- Use the [UUID Generator][uuid-gen] to generate consistently formatted UUIDs for `config.json` files.

[uuid-gen]: https://www.uuidgenerator.net/version4
[google-search-link]: https://google.com
[comparison-of-american-british-english]: https://en.wikipedia.org/wiki/Comparison_of_American_and_British_English
[language-identifiers]: https://github.com/github/linguist/blob/master/lib/linguist/languages.yml
[prettier]: https://prettier.io/
[prettier-cli]: https://prettier.io/docs/en/cli.html
[prettier-editor]: https://prettier.io/docs/en/editors.html
[nodejs]: https://nodejs.org/en/
[allowing-fork-pr-changes]: https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/allowing-changes-to-a-pull-request-branch-created-from-a-fork
