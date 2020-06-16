# File Sniffer

## Story

The story of this exercise uses the premise of comparing the file extension to the binary file format to explore the ideas of binary data, pattern matching, string encoding. The student is tasked with writing some functions to compare the expected media type to the file's encoded media type.

## Tasks

The story facilitates defining functions:

- Returning the media type from a string extension.
- Returning the media type from a binary file format.
- Comparing the expected media type from the string extension to the actual type from the binary file.

## Exercise Reference

| File type | Common extension  | Media type                   | binary 'signature' from the first bytes          |
| --------- | ----------------- | ---------------------------- | ------------------------------------------------ |
| ELF       | `none` or `"exe"` | `"application/octet-stream"` | `0x7F, 0x45, 0x4C, 0x46`                         |
| BMP       | `"bmp"`           | `"image/bmp"`                | `0x42, 0x4D`                                     |
| PNG       | `"png"`           | `"image/png"`                | `0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A` |
| JPG       | `"jpg"`           | `"image/jpg"`                | `0xFF, 0xD8, 0xFF`                               |
| GIF       | `"gif"`           | `"image/gif"`                | `0x47, 0x49, 0x46`                               |

## Implementations

- [Elixir][implementation-elixir] (reference implementation)

[implementation-elixir]: ../../languages/elixir/exercises/concept/binary-matching/.docs/instructions.md
