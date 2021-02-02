An enumeration is a distinct type whose value is restricted to a range of values (see below for details), which may include several explicitly named constants ("enumerators"). The values of the constants are values of an integral type known as the underlying type of the enumeration.

An enumeration is defined using the following syntax:

1.  ``enum-key attr(optional) enum-name(optional) enum-base(optional)(C++11) { enumerator-list(optional) }``
2.  ``enum-key attr(optional) enum-name enum-base(optional)``

The Syntax variables listed above are:

- **enum-key**	-	one of enum, enum class(since C++11), or enum struct(since C++11)
- **attr(C++11)**	-	optional sequence of any number of [attributes][attributes]
- **enum-name**	-	the name of the enumeration that's being declared. If present, and if this declaration is a re-declaration, it may be preceded by nested-name-specifier(since C++11): sequence of names and scope-resolution operators ::, ending with scope-resolution operator. The name can be omitted only in unscoped enumeration declarations
- **enum-base(C++11)**	-	colon (:), followed by a type-specifier-seq that names an integral type (if it is cv-qualified, qualifications are ignored) that will serve as the fixed underlying type for this enumeration type
- **enumerator-list**	-	comma-separated list of enumerator definitions, each of which is either simply an identifier, which becomes the name of the enumerator, or an identifier with an initializer: identifier = constexpr. In either case, the identifier can be directly followed by an optional [attribute specifier sequence][Specifier]. (since C++17)

[attributes]: https://en.cppreference.com/w/cpp/language/attributes
[Specifier]: https://en.cppreference.com/w/cpp/language/attributes
