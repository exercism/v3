A new poetry club has opened in town, and you're thinking of attending. Because there have been incidents in the past, the club has a very specific door policy which you'll need to master, before attempting entry.

There are two doors at the poetry club, both are guarded. In order to gain entry, you'll need to work out the password of that day:

## Front door

1. The guard will recite a poem
   - You will have to split the poem into individual lines and respond with the appropriate letters.
2. The guard will tell you all the letters you've responded with at once;
   - You need to format the letters as a capitalized word.

For example, one of their favorite writers is Michael Lockwood, who's written the following _acrostic_ poem, which means that the first letter of each sentence form a word:

```text
Stands so high
Huge hooves too
Impatiently waits for
Reins and harness
Eager to leave
```

When the guard recites the poem, you will split it into individual lines and respond with the first letters of each line, i.e. ["S", "H", "I", "R", "E"].

The guard will then give you the word formed by the array of letters you replied with for you to put into capitalized word form. Finally the password you return is `Shire`, and you'll get in.

## Back door

In the back of the club you'll find the most renowned poets, which is like the VIP area. Because this is not for everyone, the back door process is a bit more convoluted.

1. The guard will recite a poem;
   - Again, you will have to split the poem into lines and respond with the appropriate letter _but
     there are sometimes spaces after each sentence that will need to be removed first_.
2. The guard will tell you all the letters you've responded with at once:
   - You need to format the letters as a capitalized word
   - and ask nicely, by appending `, please`

For example, the poem mentioned before is also _telestich_, which means that
the last letter of each sentence form a word:

```text
Stands so high
Huge hooves too
Impatiently waits for
Reins and harness
Eager to leave
```

When the guard recites the poem, you will split it into individual lines, strip off any trailing spaces, and respond with the first letters of each line, i.e. ["h", "o", "r", "s", "e"].

The guard will then give you the word formed by the array of letters you replied with for you to put into capitalized word form and append `", please."`. Finally the password you return is `Horse, please.`, and you'll get in.

## Secret room

Inside the back room of the club is another door that leads to the secret room that only the very top poets may enter.

1. The guard will recite a poem;
   - Again, you will have to split the poem into lines and respond with the appropriate letter _but now the appropriate letter from line number i is the i<sup>th</sup> letter of the line_.
2. The guard will tell you all the letters you've responded with at once:
   - You need to format the letters as a capitalized word
   - You must shout the secret phrase by returning an uppercased string with an exclamation point added to the end.

For example, a modified version of the poem mentioned before fits this pattern:

```text
Stands so high
Huge hooves too
Impatiently waits for
Rider with harness
Eager to leave
```

When the guard recites the poem, you will split it into individual lines, strip off any trailing spaces, and respond with the i<sup>th</sup> letters of each line, i.e. ["S", "u", "p", "e", "r"].

The guard will then give you the word formed by the array of letters you replied with for you to put into uppercased word form and append `"!"`. Finally the password you return is `SUPER!`, and you'll get in.

## Tasks

## 1. Split a string into individual lines

Implement the function `splitLines(_:)` that takes a `String` as output and splits it into an array of `String`s using newlines as delimiters.

```swift
splitLines("Hello.\nHow are you?\n\nI'm doing fine.")
// => ["Hello." ,"How are you?", "", "I'm doing fine"].
```

## 2. Get the first letter of a sentence

Implement the function `firstLetter(_:)` that returns first letter of a sentence If there is no last letter, return an underscore (`_`):

```swift
firstLetter("Stands so high")
// => "S"
```

## 3. Capitalize a word

Implement the function `capitalize(_:)` that correctly capitalizes a word:

```swift
capitalize("SHIRE")
// => "Shire"

capitalize("horse")
// => "Horse"
```

## 4. Trim a sentence

Implement the function `trimFromEnd(_:)` that removes whitespace from the end of a sentence and returns the trimmed sentence:

```swift
trimFromEnd("Stands so high   ")
// => "Stands so high"
```

## 5. Get the last letter of a sentence

Implement the function `lastLetter(_:)` that returns the last letter of a sentence. If there is no last letter, return an underscore (`_`):

```swift
lastLetter("Stands so high")
// => "h"
```

## 6. Be polite

Implement the function `backDoorPassword(_:)` that takes a string as input and formats it in the polite manner required for the backdoor password:

```swift
backDoorPassword('horse')
// => "Horse, please"
```

## 7. Get the i<sup>th</sup> letter of a sentence

Implement the function `ithLetter(_:i:)` that returns the i<sup>th</sup> letter of a sentence. If there is no i<sup>th</sup> letter, return a space:

```swift
ithLetter("Impatiently waits for", i: 2)
// => "p"
```

## 8. Shout the answer

Implement the function `secretRoomPassword(_:)` that takes a string as input and formats it in the shouting manner required for the secret room password:

```swift
secretRoomPassword('Super')
// => "SUPER!"
```
