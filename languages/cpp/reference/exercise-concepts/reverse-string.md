# Reverse String

## Task

Reverse a string

For example:
input: "cool"
output: "looc"


## Example

`reverse_string.h`

```cpp
#if !defined(REVERSE_STRING_H)
#define REVERSE_STRING_H

#include <string>

namespace reverse_string
{

std::string reverse_string(const std::string& text);

}

#endif
```

`reverse_string.cpp`
```cpp
#include "reverse_string.h"

namespace reverse_string
{

std::string reverse_string(const std::string& text)
{
    return std::string(text.rbegin(), text.rend());
}

}
```

## Concepts

- .h file: the tests include `"reverse_string.h"`
- .cpp file:  the most common approach is to use a `.cpp` for the definitions
- namespaces: the function is member of a namespace
- functions: used as the main entry point for the exercise
- function arguments: the function has two arguments
- strings: input is a string (or maybe string_view)
- return values: the result is returned by the function
- pass by reference to const: avoid making copy of input
- iterators: view input as a range


## Alternative approaches

- implement solution as `inline` functions in the `.h` file
- using `std::string_view` to pass input by argument


## Common mistakes

- argument is not passed by `const` reference
- explicit iteration over all characters in the string
- creating temporary variable and returning it


## Common suggestions to improve a solution

- use `auto` to deduce return type
- use `[[nodiscard]]` attribute to warn on discarding a nodiscard value
- using `std::string_view` to pass input
