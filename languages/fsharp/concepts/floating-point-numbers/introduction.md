A floating-point number is a number with zero or more digits behind the decimal separator. Examples are `-2.4`, `0.1`, `3.14`, `16.984025` and `1024.0`.

F# has three floating-point types:

- `single`: 4 bytes (~6-9 digits precision). Written as `2.45f`.
- `float`: 8 bytes (~15-17 digits precision). This is the most common type. Written as `2.45`.
- `decimal`: 16 bytes (28-29 digits precision). Normally used when working with monetary data, as its precision leads to less rounding errors. Written as `2.45m`.

Different floating-point types can store different numbers of digits after the digit separator - this is referred to as its precision. This means that trying to store PI in a `single` will only store the first 6 to 9 digits (with the last digit being rounded).
