## 1. Making a new list

- Lists can be created with `list`.

## 2. Add things to the list.

- `cons` constructs a new list with its first argument as the `car` and its second argument as the `cdr` of the new list.

## 3. What's next thing(s) on the list?

- The helper functions `first` through `tenth` exist to access those indexes in a list.
- `nth` can access the item at any index in a list.
- List indexes are zero based.

## 4. Removing a thing from the list

- `cdr` (or `rest`) can be used to access all items but the first in a list.

## 5. Are you a member?

- `member` will return a sub-list of the given list with the searched for item as its `car` if such a sub-list exists.

## 6. Bigger lists out of smaller lists

- `append` concatenates two (or more) lists together.

## 7. How much longer?

- `length` will evaluate to the length of a list.

## 8. Divide and conquer.

- Indexes in a list are zero based
- The range of items is inclusive of the start index, but exclusive of the end index.
- `subseq` can be used to access sub-lists of a list.

## 9. Meeting in the middle.

- `reverse` will evaluate to a new list which is in the reverse order of the original list.
