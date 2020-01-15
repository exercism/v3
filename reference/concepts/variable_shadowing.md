# Variable Shadowing

Variable shadowing occurs when a [variable][concept-variables] declared within a certain [scope][concept-scope] (decision block, method, or inner class) has the same name as a variable declared in an outer [scope][concept-scope]. At the level of identifiers (names, rather than variables), this is known as name masking. This outer variable is said to be shadowed by the inner variable, while the inner identifier is said to mask the outer identifier. This can lead to confusion, as it may be unclear which variable subsequent uses of the shadowed variable name refer to, which depends on the name resolution rules of the language.

Some languages allow variable shadowing in more cases than others. For example [Kotlin][language-kotlin] allow an inner variable in a [function][concept-functions] to shadow a passed argument and a variable in inner block to shadow another in outer block, while Java does not allow these. Both languages allow a passed argument to a function/Method to shadow a [Class Field][concept-class-field].

Some languages, such as [CoffeeScript][language-coffeescript], disallow variable shadowing completely.

[concept-class-field]: ./classes.md
[concept-functions]: ./functions.md
[concept-scope]: ./scope.md
[concept-variables]: ./variables.md
[language-coffeescript]: ../../languages/coffeescript/README.md
[language-kotlin]: ../../languages/kotlin/README.md
