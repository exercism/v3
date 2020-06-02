You have been working on a project which allows users to upload files to the server to be shared with other users. You have been tasked write a function to verify that an upload matches it's [media type][mimetype]. You do some research and discover that the first few bytes of a file is generally unique to that filetype, giving it a sort of signature.

Use the following table for reference:

| File type | Common extension  | Media type                   | binary signature                                 |
| --------- | ----------------- | ---------------------------- | ------------------------------------------------ |
| ELF       | `none` or `"exe"` | `"application/octet-stream"` | `0x7F, 0x45, 0x4C, 0x46`                         |
| BMP       | `"bmp"`           | `"image/bmp"`                | `0x42, 0x4D`                                     |
| PNG       | `"png"`           | `"image/png"`                | `0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A` |
| JPG       | `"jpg"`           | `"image/jpg"`                | `0xFF, 0xD8, 0xFF`                               |
| GIF       | `"gif"`           | `"image/gif"`                | `0x47, 0x49, 0x46`                               |

## 1. Given an extension, return the expected media type

## 2. Given a binary file, return the expected media type

## 3. Given an extension and a binary file, verify that the file matches the expected type

[mimetype]: https://en.wikipedia.org/wiki/Media_type
