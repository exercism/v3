# Python reference

Python is an interpreted, dynamically (but strongly) typed, and garbage-collected general programming language that has become extremely popular due to its readability, low barrier for entry, and exceptionally deep ecosystem of libraries and tools. Python is object-based, but is inherently multi-paradigm and has drawn together influences from a wide range of programming languages, including ABC, Haskell, Lisp, and Modula-3.

## Language-unique concepts

- [The Zen of Python][zen-of-python]
- [Pythonic][pythonic]
- [Python Enhancement Proposals][python-enhancement-proposals]
- [PEP 8][pep-8-style-guide]
- [Dunder Methods][dunder-methods]

[zen-of-python]: ./concepts/zen_of_python.md
[pythonic]: ./concepts/pythonic.md
[python-enhancement-proposals]: ./concepts/python_enhancement_proposals.md
[pep-8-style-guide]: ./concepts/pep_8_style_guide.md
[dunder-methods]: concepts/dunder_methods.md

## Object-oriented concepts

- [Classes][classes-general]

    - [Custom classes][custom-classes]
    - [Class members][class-members]
    - [Class methods][class-methods]
    - [Static Methods][static-methods]
    - [Initialization][initialization]
    - [Instance Attributes][instance-attributes]
    - [Instance Methods][instance-methods]
    - [Instance Properties][instance-properties]
    - [Instantiation][instantiation]
    - [Implicit self][implicit-self]

- [Composition][composition-general]

    - [Class composition][class-composition]

- [Encapsulation][encapsulation-general]

    - [Namespaces][namespaces]
    - [Non-Public Methods][non-public-methods]

- [Inheritance][inheritance-general]

    - [Class inheritance][class-inheritance]

- [Interfaces][interfaces-general]

    - [Duck Typing][duck-typing]

- [Mutation][mutation-general]
- [Objects][objects-general]
- [Polymorphism][polymorphism-general]

    - [Dynamic typing][dynamic-typing]

- [State][state]

[classes-general]: ../../../reference/concepts/classes.md
[custom-classes]: concepts/custom_classes.md
[class-members]: concepts/class_members.md
[class-methods]: concepts/class_methods.md
[static-methods]: concepts/static_methods.md
[initialization]: concepts/initialization.md
[instance-attributes]: concepts/instance_attributes.md
[instance-methods]: concepts/instance_methods.md
[instance-properties]: concepts/instance_properties.md
[instantiation]: concepts/instantiation.md
[implicit-self]: concepts/implicit_self.md


[composition-general]: ../../../reference/concepts/composition.md
[class-composition]: concepts/class_composition.md

[encapsulation-general]: ../../../reference/concepts/encapsulation.md
[namespaces]: concepts/namespaces.md
[non-public-methods]: concepts/non_public_methods.md

[inheritance-general]: ../../../reference/concepts/inheritance.md
[class-inheritance]: concepts/class_inheritance.md

[interfaces-general]: ../../../reference/concepts/interfaces.md
[duck-typing]: concepts/duck_typing.md

[mutation-general]: ../../../reference/concepts/mutation.md

[objects-general]: ../../../reference/concepts/objects.md

[polymorphism-general]: ../../../reference/concepts/polymorphism.md
[dynamic-typing]: concepts/dynamic-typing.md

## Functional concepts

- [Anonymous functions][anonymous_functions-general]
    
    - TODO: Create a `lambda` concept

- [Higher-order functions][higher_order_functions]
- [Immutability][immutability]
- [Nested functions][nested_functions]
- [Partial application][partial_application]
- [Pipelines][pipelines]
- [Pure functions][pure_functions]
- [Recursion][recursion]
- [REPL][repl]
- [Type inference][type_inference]

[anonymous-functions-general]: ../../../reference/concepts/anonymous_functions.md

## General concepts

- [Arithmetic][arithmetic-general]

- [Bitwise manipulation][bitwise_manipulation-general]

    - [Binary numbers][binary-numbers]
    - [Bitflags][bitflags]
    - [Bitwise operators][bitwise-operators]
    - [Powers of Two][powers-of-two]

- [Boolean logic][boolean_logic_general]

    - [Boolean values][boolean-values]
    - [Booleans are integers][booleans-are-integers]
    - [Boolean operators][boolean-operators]

- [Comments][comments-general]

    - TODO: Write a concept explaining comments in Python

- [Comparisons][comparisons-general]

    - [Comparison operators][comparison-operators]
    - [Integer comparison][integer-comparison]
    - [Rich comparison methods][rich-comparison-methods]
    - [Equality operator][equality-operator]
    - [Equivalence][equivalence]
    - [Inequality][inequality]

- [Conditionals][conditionals-general]
- [Enumeration][enumeration]

- [Exceptions][exceptions-general]
    
    - [Exception catching][exception-catching]
    - [Exception handling][exception-handling]
    - [Exception hierarchy][exception-hierarchy]
    - [Exception message][exception-message]
    - [Raise][raise]

- [Functions][functions-general]
- [Generics][generics]
- [Loops][loops-general]
- [Methods][methods-general]
- [Scope][scope]
- [Variables][variables]

[arithmetic-general]: ../../../reference/concepts/arithmetic.md

[bitwise-manipulation-general]: ../../../reference/concepts/bitwise_manipulation.md
[binary-numbers]: concepts/binary_numbers.md
[bitflags]: concepts/bitflags.md
[bitwise-operators]: concepts/bitwise_operators.md
[powers-of-two]: concepts/powers_of_two.md

[boolean-logic-general]: ../../../reference/concepts/boolean_logic.md
[booleans-values]: concepts/boolean_values.md
[boolean-operators]: concepts/boolean_operators.md
[booleans-are-integers]: concepts/booleans_are_integers.md

[comments-general]: ../../../reference/concepts/comments.md

[conditionals-general]: ../../../reference/concepts/conditionals.md

[comparisons-general]: ../../../reference/concepts/comparisons.md
[comparison-operators]: concepts/comparison_operators.md
[integer-comparison]: concepts/integer_comparison.md
[rich-comparison-methods]: concepts/rich_comparison_methods.md
[equality-operator]: concepts/equality_operator.md
[equivalence]: concepts/equivalence.md
[inequality]: concepts/inequality.md

[exceptions-general]: ../../../references/concepts/exceptions.md
[exception-catching]: concepts/exception_catching.md
[exception-handling]: concepts/exception_handling.md
[exception-hierarchy]: concepts/exception_hierarchy.md
[exception-message]: concepts/exception_message.md
[raise]: concepts/raise.md

[loops-general]: ../../../reference/concepts/loops.md

[methods-general]: ../../../reference/concepts/methods.md
## Types

### [Builtin types][builtin-types]

- Numerical

    - [`bool`][builtin-types-bool]
    - [`int`][builtin-types-int]
    - [`float`][builtin-types-float]
    - [`complex`][builtin-types-complex]

- Byte sequences

    - [`bytes`][builtin-types-bytes]
    - [`bytearray`][builtin-types-bytearray]

- Text sequences

    - [`str`][builtin-types-str]

- Sequential containers

    - [`tuple`][builtin-types-tuple]
    - [`range`][builtin-types-range]
    - [`list`][builtin-types-list]

- Unique sets

    - [`set`][builtin-types-set]
    - [`frozenset`][builtin-types-frozenset]

- Mapping types

    - [`dict`][builtin-types-dict]

- Access to container internals

    - [`slice`][builtin-types-slice]
    - [`memoryview`][builtin-types-memoryview]

- Object-orientation related

    - [`object`][builtin-types-object]
    - [`property`][builtin-types-property]
    - [`type`][builtin-types-type]


## Resources used

- https://www.python.org

## Extracted Concepts

- [Argument Unpacking][argument-unpacking]
- [Assignment][assignment]
- [Bracket Notation][bracket-notation]
- [Call Semantics][call-semantics]
- [Comprehension Syntax][comprehension-syntax]
- [Conditionals structures][conditionals]
- [Constants][constants]
- [Constructor][constructor]
- [Data Structures][data-structures]
- [Default Arguments][default-arguments]
- [Dictionary][dictionary]
- [Docstrings][docstrings]
- [Enumerated Values][enumerated-values]
- [Enumeration][enumeration]
- [Expressions][expressions]
- [For Loop][for-loop]
- [Function Decorator][function-decorator]
- [Function signature][function-signature]
- [Functions][functions]
- [Generator comprehension][generator-comprehension]
- [Generators][generators]
- [Generics][generics]
- [Higher-Order Function][higher-order-function]
- [Identity][identity]
- [Immutability][immutability]
- [Importing][importing]
- [Indexing][indexing]
- [Int][int]
- [Iteration][iterable]
- [Iteration][iteration]
- [Iterators][iterators]
- [List Methods][list-methods]
- [Lists][lists]
- [Lookup Efficiency][lookup-efficiency]
- [Loops][loops]
- [Membership Testing][membership-testing]
- [Method Arguments][method-arguments]
- [Method Parameters][method-parameters]
- [Methods of list][methods-of-list]
- [Modular Division][modular-division]
- [Multiple Assignment][multiple-assignment]
- [Mutability][mutability]
- [None][none]
- [Objects][objects]
- [Operator overloading][operator-overloading]
- [Operator Precedence][operator-precedence]
- [Operators][operators]
- [Order of Evaluation][order-of-evaluation]
- [Property][property]
- [Property Decorator][property-decorator]
- [Recursion][recursion]
- [Refactor][refactor]
- [Regular Expressions][regular-expressions]
- [Return Values][return-value]
- [Short-Circuiting][short-circuiting]
- [Slicing][slicing]
- [Standard Library][standard-library]
- [String formatting][string-formatting]
- [String Methods][string-methods]
- [String Splitting][string-splitting]
- [String Translation][string-translation]
- [Tuple unpacking][tuple-unpacking]
- [Type Conversion][type-conversion]
- [Type hinting][type-hinting]



[argument-unpacking]: /concepts/argument_unpacking.md
[assignment]: concepts/assignment.md
[bracket-notation]: concepts/bracket_notation.md
[call-semantics]: concepts/call_semantics.md
[comprehension-syntax]: concepts/comprehension_syntax.md
[conditionals]: concepts/conditionals.md
[constants]: concepts/constants.md
[constructor]: concepts/constructor.md
[data-structures]: concepts/data_structures.md
[default-arguments]: concepts/default_arguments.md
[dictionary]: concepts/dictionary.md
[docstrings]: concepts/docstrings.md

[enumerated-values]: concepts/enumerated_values.md
[enumeration]: ../../../reference/concepts/enumeration.md
[enumeration]: concepts/enumeration.md
[expressions]: concepts/expressions.md
[for-loop]: concepts/for_loop.md
[function-decorator]: concepts/function_decorator.md
[function-signature]: concepts/function_signature.md
[functions-general]: ../../../reference/concepts/functions.md
[functions]: concepts/functions.md
[generator-comprehension]: concepts/generator_comprehension.md
[generators]: concepts/generators.md
[generics]: ../../../reference/concepts/generics.md
[generics]: concepts/generics.md
[higher-order-function]: concepts/higher_order_function.md
[higher-order-functions]: ../../../reference/concepts/higher_order_functions.md
[identity]: concepts/identity.md
[immutability]: ../../../reference/concepts/immutability.md
[immutability]: concepts/immutability.md
[importing]: concepts/importing.md
[indexing]: concepts/indexing.md
[int]: concepts/int.md
[iterable]: concepts/iterable.md
[iteration]: concepts/iteration.md
[iterators]: concepts/iterators.md
[list-methods]: concepts/list_methods.md
[lists]: concepts/lists.md
[lookup-efficiency]: concepts/lookup_efficiency.md
[loops]: concepts/loops.md
[membership-testing]: concepts/membership_testing.md
[method-arguments]: concepts/method_arguments.md
[method-overloading]: concepts/method_overloading.md
[method-parameters]: concepts/method_parameters.md
[methods-of-list]: concepts/methods_of_list.md
[modular-division]: concepts/modular_division.md
[multiple-assignment]: concepts/multiple_assignment.md
[mutability]: concepts/mutability.md
[nested-functions]: ../../../reference/concepts/nested_functions.md
[none]: concepts/none.md
[objects]: concepts/objects.md
[operator-overloading]: concepts/operator_overloading.md
[operator-precedence]: concepts/operator_precedence.md
[operators]: concepts/operators.md
[order-of-evaluation]: concepts/order_of_evaluation.md
[partial-application]: ../../../reference/concepts/partial_application.md
[pipelines]: ../../../reference/concepts/pipelines.md
[property-decorator]: concepts/property_decorator.md
[property]: concepts/property.md
[pure-functions]: ../../../reference/concepts/pure_functions.md
[recursion]: ../../../reference/concepts/recursion.md
[recursion]: concepts/recursion.md
[refactor]: concepts/refactor.md
[regular-expressions]: concepts/regular_expressions.md
[repl]: ../../../reference/concepts/repl.md
[return-value]: concepts/return_value.md

[scope]: ../../../reference/concepts/scope.md
[short-circuiting]: concepts/short_circuiting.md
[slicing]: concepts/slicing.md
[standard-library]: concepts/standard_library.md
[state]: ../../../reference/concepts/state.md

[string-formatting]: concepts/string_formatting.md
[string-methods]: concepts/string_methods.md
[string-splitting]: concepts/string_splitting.md
[string-translation]: concepts/string_translation.md
[tuple-unpacking]: concepts/tuple_unpacking.md
[type-conversion]: concepts/type_conversion.md
[type-hinting]: concepts/type_hinting.md
[type-inference]: ../../../reference/concepts/type_inference.md
[variables]: ../../../reference/concepts/variables.md

[builtin-types]: ./concepts/builtin_types/README.md
[builtin-types-bool]: ./concepts/builtin_types/bool.md
[builtin-types-bytearray]: ./concepts/builtin_types/bytearray.md
[builtin-types-bytes]: ./concepts/builtin_types/bytes.md
[builtin-types-complex]: ./concepts/builtin_types/complex.md
[builtin-types-dict]: ./concepts/builtin_types/dict.md
[builtin-types-float]: ./concepts/builtin_types/float.md
[builtin-types-frozenset]: ./concepts/builtin_types/frozenset.md
[builtin-types-int]: ./concepts/builtin_types/int.md
[builtin-types-list]: ./concepts/builtin_types/list.md
[builtin-types-memoryview]: ./concepts/builtin_types/memoryview.md
[builtin-types-object]: ./concepts/builtin_types/object.md
[builtin-types-property]: ./concepts/builtin_types/property.md
[builtin-types-range]: ./concepts/builtin_types/range.md
[builtin-types-set]: ./concepts/builtin_types/set.md
[builtin-types-slice]: ./concepts/builtin_types/slice.md
[builtin-types-str]: ./concepts/builtin_types/str.md
[builtin-types-tuple]: ./concepts/builtin_types/tuple.md
[builtin-types-type]: ./concepts/builtin_types/type.md

[builtin-functions]: ./concepts/builtin_functions/README.md
[builtin-functions-__import__]: ./concepts/builtin_functions/__import__.md
[builtin-functions-abs]: ./concepts/builtin_functions/abs.md
[builtin-functions-all]: ./concepts/builtin_functions/all.md
[builtin-functions-any]: ./concepts/builtin_functions/any.md
[builtin-functions-ascii]: ./concepts/builtin_functions/ascii.md
[builtin-functions-bin]: ./concepts/builtin_functions/bin.md
[builtin-functions-breakpoint]: ./concepts/builtin_functions/breakpoint.md
[builtin-functions-callable]: ./concepts/builtin_functions/callable.md
[builtin-functions-chr]: ./concepts/builtin_functions/chr.md
[builtin-functions-classmethod]: ./concepts/builtin_functions/classmethod.md
[builtin-functions-compile]: ./concepts/builtin_functions/compile.md
[builtin-functions-delattr]: ./concepts/builtin_functions/delattr.md
[builtin-functions-dir]: ./concepts/builtin_functions/dir.md
[builtin-functions-divmod]: ./concepts/builtin_functions/divmod.md
[builtin-functions-enumerate]: ./concepts/builtin_functions/enumerate.md
[builtin-functions-eval]: ./concepts/builtin_functions/eval.md
[builtin-functions-exec]: ./concepts/builtin_functions/exec.md
[builtin-functions-filter]: ./concepts/builtin_functions/filter.md
[builtin-functions-format]: ./concepts/builtin_functions/format.md
[builtin-functions-getattr]: ./concepts/builtin_functions/getattr.md
[builtin-functions-globals]: ./concepts/builtin_functions/globals.md
[builtin-functions-hasattr]: ./concepts/builtin_functions/hasattr.md
[builtin-functions-hash]: ./concepts/builtin_functions/hash.md
[builtin-functions-help]: ./concepts/builtin_functions/help.md
[builtin-functions-hex]: ./concepts/builtin_functions/hex.md
[builtin-functions-id]: ./concepts/builtin_functions/id.md
[builtin-functions-input]: ./concepts/builtin_functions/input.md
[builtin-functions-isinstance]: ./concepts/builtin_functions/isinstance.md
[builtin-functions-issubclass]: ./concepts/builtin_functions/issubclass.md
[builtin-functions-iter]: ./concepts/builtin_functions/iter.md
[builtin-functions-len]: ./concepts/builtin_functions/len.md
[builtin-functions-locals]: ./concepts/builtin_functions/locals.md
[builtin-functions-map]: ./concepts/builtin_functions/map.md
[builtin-functions-max]: ./concepts/builtin_functions/max.md
[builtin-functions-min]: ./concepts/builtin_functions/min.md
[builtin-functions-next]: ./concepts/builtin_functions/next.md
[builtin-functions-oct]: ./concepts/builtin_functions/oct.md
[builtin-functions-open]: ./concepts/builtin_functions/open.md
[builtin-functions-ord]: ./concepts/builtin_functions/ord.md
[builtin-functions-pow]: ./concepts/builtin_functions/pow.md
[builtin-functions-print]: ./concepts/builtin_functions/print.md
[builtin-functions-repr]: ./concepts/builtin_functions/repr.md
[builtin-functions-reversed]: ./concepts/builtin_functions/reversed.md
[builtin-functions-round]: ./concepts/builtin_functions/round.md
[builtin-functions-setattr]: ./concepts/builtin_functions/setattr.md
[builtin-functions-sorted]: ./concepts/builtin_functions/sorted.md
[builtin-functions-staticmethod]: ./concepts/builtin_functions/staticmethod.md
[builtin-functions-sum]: ./concepts/builtin_functions/sum.md
[builtin-functions-super]: ./concepts/builtin_functions/super.md
[builtin-functions-vars]: ./concepts/builtin_functions/vars.md
[builtin-functions-zip]: ./concepts/builtin_functions/zip.md

