# Character

In many languages with built-in support for [Unicode][concept-unicode], the Character type represents a single extended grapheme cluster that approximates a user-perceived character.

A single character can be made up of one or more Unicode scalar values which get grouped together into a single element of the Character type by a Unicode boundary algorithm. This grouping represents what is seen as a single glyph.

For example, the Unicode country flags are built up out of the two Unicode regional indicator symbol letters that correspond to the country's ISO 3166-1 alpha-2 code. So the Unicode scalar "\u{1F1EC}" (REGIONAL INDICATOR SYMBOL LETTER G) and "\u{1F1F1}" (REGIONAL INDICATOR SYMBOL LETTER L) can be separate Characters on their own ("🇬" and "🇱", respectively), when placed together, they are combined into a single Character, which is displayed as the flag of Greenland, "🇬🇱".

[concept-unicode]: ../concepts/unicode.md
