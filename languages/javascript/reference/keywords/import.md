# `import`

The `import` statement is used to load [functions][concept-function], [variables][concept-variable] and [classes][concept-class] from other [modules][concept-modules] into the current file.

It comes in multiple versions that can load the default [`export`][concept-export], some or all named exports and rename them on the fly:

```js
import defaultExport from "module-name"; // Load one default export
import * as name from "module-name"; // Load all named exports for access via `name.namedExport`
import { myExport } from "module-name"; // Only imports the myExport named export.
import { myExport as expert } from "module-name"; // Only imports the myExport but aliases it to expert
```

The `module-name` in the example can be either a [NPM][npm]-installed package – those typically reside in the `node_modules/` directory – or a relative link, such as `./myFile.js`.

## Further reading

> https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import

[concept-function]: ../../../../reference/concepts/functions.md
[concept-variable]: ../../../../reference/concepts/variables.md
[concept-class]: ../../../../reference/concepts/classes.md
[concept-module]: ../info/modules.md
[concept-export]: ./export.md
