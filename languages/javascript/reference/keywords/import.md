# `import`

The `import` statement is used to load [functions][concept-function], [variables][concept-variable] and [classes][concept-class] from other [modules][concept-modules] into the current file.

It comes in multiple versions that can load the `default` [`export`][concept-export], some or all named exports and rename them on the fly:

```js
import defaultExport from "module-name"; // Load default export
import * as name from "module-name"; // Load all named exports for access via `name.namedExport`
import { myExport } from "module-name"; // Only imports the myExport named export.
import { myExport as expert } from "module-name"; // Only imports the myExport but aliases it to expert
import { default as namedDefault, myExport } from "module-name"; // Imports the default export as namedDefault, and myExport
```

Typically, the `module-name` can be either a [NPM][npm]-installed package – which reside in the `node_modules/` directory – or a relative link, such as `./myFile.js`. But there are also JavaScript interpreter out there, where those `modules` can even be URLs, like in [deno][deno].

## Further reading

> https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import

[concept-function]: ../../../../reference/concepts/functions.md
[concept-variable]: ../../../../reference/concepts/variables.md
[concept-class]: ../../../../reference/concepts/classes.md
[concept-module]: ../info/modules.md
[concept-export]: ./export.md
[deno]: https://deno.land
