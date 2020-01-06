# `Object`

See [prototype-based inheritance][info-prototype-inheritance] and the general [`object` type][type-object].

`Object` is a standard built-in global object, that acts as a [constructor][info-constructor] which creates an object _wrapper_, with specific rules. If the value given to the constructor is [`null`][type-null] or [`undefined`][type-undefined], it will create and return an empty [`object`][type-object]. If the value is a primitive type, it will return an _object_ of the _wrapper_ of that type (e.g. `new Object('foo')` gives `String { 'foo' }`, instead of the [`string`][type-string] literal `'foo'`). If the value is already an `object`, it will return the passed in value.

There are various types in other languages that don't exist in JavaScript, but instead are regular objects with a different [_prototype_][info-prototype-inheritance]. A few important examples:

## JavaScript Object

A JavaScript [object][type-object] is a mapping between keys and values. Keys are [`string`s][type-string] (or [Symbols][type-symbol]) and values can be anything. This makes objects a natural fit for [hashmaps][type-hash-map].

## JavaScript Function

[Functions][type-function] are regular objects with the additional capability of being _callable_.

## JavaScript Date

A JavaScript [`Date`][type-date] is a regular object which inherit from [`Date.prototype`][object-date]. They are created using the built-in [`Date` utility][object-date]. These objects represent a [`datetime`][type-datetime] or [`timestamp`][type-timestamp], as they have both date and time information.

## JavaScript Arrays

[Arrays][type-array] are regular objects for which there is a particular relationship between integer-key-ed properties and the 'length' property. Additionally, arrays inherit from [`Array.prototype`][object-array] which provides to them a handful of convenient methods to manipulate arrays. See the [`Array` Global Object][object-array] for more information.

[info-constructor]: ../info/constructor.md
[info-prototype-inheritance]: ../info/prototype_inheritance.md
[object-array]: ./array.md
[object-date]: ./date.md
[type-array]: ../../../types/array.md
[type-function]: ../../../types/function.md
[type-hash-map]: ../../../types/hash_map.md
[type-date]: ../../../types/date.md
[type-datetime]: ../../../types/datetime.md
[type-null]: ../../../types/null.md
[type-object]: ../../../types/object.md
[type-string]: ../../../types/string.md
[type-symbol]: ../../../types/symbol.md
[type-timestamp]: ../../../types/timestamp.md
[type-undefined]: ../../../types/undefined.md
