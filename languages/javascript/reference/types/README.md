# JavaScript Reference: Types

## Primitives

- [string][type-string]
- [number][type-number]
- [bigint][type-bigint]
- [boolean][type-boolean]
- [null][type-null]
- [undefined][type-undefined]
- [symbol][type-symbol]

All primitives are [immutable][concept-immutable], i.e., they cannot be altered. It is important not to confuse a primitive itself with a variable assigned a primitive value. The variable may be reassigned a new value, but the existing value can not be changed in the ways that [objects][type-object], [arrays][type-array], and [functions][type-function] can be altered.

## Non-primitive types

- [`object`][type-object]

Note: Things like `Array` and `Date` are not types, but rather constructors that, when used, create `object`s with a certain [_prototype_][concept-prototype-inheritance].

# See also

- [JavaScript info][see-also-info],
- [JavaScript keywords][see-also-keywords],
- [Global Objects][see-also-objects],
- [JavaScript reference][see-also-root].

[see-also-info]: ../info/README.md
[see-also-keywords]: ../keywords/README.md
[see-also-objects]: ../objects/README.md
[see-also-root]: ../README.md
[type-array]: ../../../../reference/types/array.md
[type-bigint]: ../../../../reference/types/big_integer.md
[type-boolean]: ../../../../reference/types/boolean.md
[type-function]: ../../../../reference/types/function.md
[type-null]: ../../../../reference/types/null.md
[type-number]: ../../../../reference/types/number.md
[type-object]: ../../../../reference/types/object.md
[type-string]: ../../../../reference/types/string.md
[type-symbol]: ../../../../reference/types/symbol.md
[type-undefined]: ../../../../reference/concepts/undefined.md
[concept-immutable]: ../../../../reference/concepts/immutability.md
[concept-prototype-inheritance]: ../info/prototype_inheritance.md
