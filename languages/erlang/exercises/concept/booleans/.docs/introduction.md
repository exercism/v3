Erlang does not have any special data type representing boolean values. Instead, the atoms `true` and `false` are used. When used together with boolean operators, they work the same as boolean data types do in most other languages.

Erlang has a handful of boolean operators:
- `not`
- `andalso`
- `orelse`
- `xor`

Two other operators you may come across are:
- `or`
- `and`

These are seldom used. This is because the expressions do not short-circuit. Consider the example `is_x() orelse is_y()`, `is_y()` is evaluated even if `is_x()` returned `true`, even though logically unnecessary. The `or` and `and` operators are still around for legacy reasons - Erlang has done its best over the years to never break backwards-compatibility. The more clumsily named `orelse` and `andalso` operators are used almost exclusively.