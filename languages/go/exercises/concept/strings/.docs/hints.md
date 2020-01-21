# Hints

### General

- The `Go by Example` tutorial has two topics that are helpful here: [String Functions][string-functions] and [String Formatting][string-formatting]
- The `strings` package of the standard library has many useful [built-in functions][strings-package].
- The `fmt` package of the standard library has some [formatting functionality for strings][fmt-package].

### 1. Get message from a log line

- Splitting a string at a certain substring is available in the [`strings` package][split-function].
- Removing white space is [built-in][trimspace-function].

### 2. Get log level from a log line

- Changing a `string`'s casing to lower can be done with [this function][lowercase-function].

### 3. Reformat a log line

- You can just add strings to one another with the plus (`+`) operator. The preferred way is to use the [`fmt` packages][sprintf-function] functionality.

[strings-package]: https://golang.org/pkg/strings/
[string-functions]: https://gobyexample.com/string-functions
[string-formatting]: https://gobyexample.com/string-formatting
[fmt-package]: https://golang.org/pkg/fmt/
[split-function]: https://golang.org/pkg/strings/#Split
[trimspace-function]: https://golang.org/pkg/strings/#TrimSpace
[lowercase-function]: https://golang.org/pkg/strings/#ToLower
[sprintf-function]: https://golang.org/pkg/fmt/#Sprintf
