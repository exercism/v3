## Learning objectives

- Basic string functions like `titlecase`, `replace` etc.
- Be aware that unicode strings are the default and that the above mentioned functions will work on non-ascii input.
- Know that strings can be treated as a collection of characters.
- How to concatenate strings.
  - That and why `*` (and not `+`) is used for string concat.

## Out of scope

- Graphemes ([Discussion](https://github.com/exercism/v3/pull/2944#discussion_r546797486)).
- Regex.
- Performance discussions of various ways to construct strings.

## Concepts

The Concepts this exercise unlocks are:

- `strings`:

## Prerequisites

There are no prerequisites.

## Analyzer

- Using `s[1]`/`s[length(s)]` should be discouraged. Suggest `s[begin]`/`s[end]` or `first(s)`/`last(s)` instead.
- Solutions using regex for the front door tasks should not be allowed. Using regex to filter punctuation is fine but not required.

---

## Potential extensions

This exercise could be extended to teach about graphemes.

`instructions.md`:

```markdown
### Encryption special

As a special event, the poets have invited the local steganography association to exchange ideas on hiding meaning within text.
The bar in the VIP area is running a themed special.
Everyone who finds the hidden passcode will get a beverage of their choice for free.

The bar keeper will recite a poem in its entirety.
The passcode will consist of

- the first letter of the first line,
- the second letter of the second line,
- and so on.

joined together and fully capitalised.
As before, you have to ask nicely and prepend `May I have some` and append `, please?`.

For example, consider aforementioned poem again:

> **S**tands so high
> H**u**ge hooves too
> Im**p**atiently waits for
> Rei**n**s and harness
> Eage**r** to leave

The **first** letter of the **first** line is **S**, the **second** letter of the **second** line is **u**, ..., the **fifth** letter of the **last** line is **r**.

The passcode you write down to receive your free beverage is `May I have some SUPNR, please?`.
```

For the greek poem, naively iterating over the string will not work.
