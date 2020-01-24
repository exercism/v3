# `export`

The `export` keyword is used to define elements of the current file, that can be imported into other files using the [`import`][concept-import] keyword.

There are two types of exports, the default export and named exports.

## Default export

The default export is marked using the `default` modifier:

```js
export default class XYZ {}
```

## Named exports

Named exports can have multiple forms, some of those are:

```js
export class XYZ {}
export const foo = "bar";
export { foo, bar }; // this exports a list of multiple features
```

## Further reading

> https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export

[concept-import]: ./import.md
