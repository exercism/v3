# Signedness

> Signedness is a property of data types representing [numbers][type-number]. A numeric variable is [signed][type-signed] if it can represent both positive and negative numbers, and [unsigned][type-unsigned] if it can only represent non-negative numbers (zero or positive numbers).

> As [signed][type-signed] numbers can represent negative numbers, they lose a range of positive numbers that can only be represented with [unsigned][type-unsigned] numbers of the same size (in [bits][type-bit]) because roughly half the possible values are non-positive values, whereas the respective unsigned type can dedicate all the possible values to the positive number range.

## Further reading

- [One's complement][wiki-ones-complement]
- [Two's complement][wiki-twos-complement]

[type-bit]: ../types/bit.md
[type-number]: ../types/number.md
[type-signed]: ../types/signed.md
[type-unsigned]: ../types/unsigned.md
[wiki-ones-complement]: https://en.wikipedia.org/wiki/Ones%27_complement
[wiki-twos-complement]: https://en.wikipedia.org/wiki/Two%27s_complement
