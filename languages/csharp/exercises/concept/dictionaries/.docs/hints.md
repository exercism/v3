### 1. Create a New Dictionary

A dictionary is like any other class.  You simply 'new' it to create an empty instance.

### 2. Create a Pre-populated Dictionary

Although it's possible to populate a dictionary by repeatedly adding items, dictionaries can be initialized statically.

See [this article][docs.microsoft.com_parsing-date].

### 3. Add a Country to an Empty Dictionary

See [Add][docs.microsoft.com_dictionary_add].  Pass in the dictionary returned by task 1 as a parameter.

### 4. Add a Country to an Existing Dictionary

There is no substantial difference between adding an item to an empty or initialized dictionary.  Pass in the dictionary returned by task 2 as a parameter.

### 5. Get the Country Name Matching a Country Code

See [this article][docs.microsoft.com_dictionary_item].

### 6. Attempt to Get Country Name for a Non-existent Country Code

You need to [detect][docs.microsoft.com_dictionary_contains_key] whether the country is present in the dictionary.

### 7. Update a Country Name

Again [this article][docs.microsoft.com_dictionary_item] applies.

### 8. Attempt to Update Name of Country that is not in the Dictionary

This is very similar to task 6.

### 9. Remove a Country from the Dictionary

See [this article][docs.microsoft.com_dictionary_remove].

[docs.microsoft.com_dictionary_static_initialization]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/how-to-initialize-a-dictionary-with-a-collection-initializer
[docs.microsoft.com_dictionary_add]: https://docs.microsoft.com/en-gb/dotnet/api/system.collections.generic.dictionary-2.add?view=netcore-3.1
[docs.microsoft.com_dictionary_item]: https://docs.microsoft.com/en-gb/dotnet/api/system.collections.generic.dictionary-2.item?view=netcore-3.1
[docs.microsoft.com_dictionary_contains_key]: https://docs.microsoft.com/en-gb/dotnet/api/system.collections.generic.dictionary-2.containskey?view=netcore-3.1
[docs.microsoft.com_dictionary_remove]: https://docs.microsoft.com/en-gb/dotnet/api/system.collections.generic.dictionary-2.remove?view=netcore-3.1
