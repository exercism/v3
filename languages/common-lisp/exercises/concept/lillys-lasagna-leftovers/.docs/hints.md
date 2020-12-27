## 1. Make `preparation-time-in-minutes` easier to use

    - A "rest parameter" will collect all arguments not consumed by other parameters into a list.
    - A "rest parameter" is designated in the lambda list with the `&rest` lambda list keyword.
    - The function `length` can be used to get the number of items in a list.

## 2. Allow changing the expected oven time

    - An optional parameter can be provided or not.
    - An optional parameter can have a default value.
    - Optional parameters are designated by the `&optional` lambda list keyword.

## 3. Lilly remembers another preferred cooking style

    - A "supplied-p parameter" can be specified with the optional parameter to determine if the caller has provided the parameter.

## 4. Splitting the leftovers

    - Keyword parameters are named and are optional.
    - Keyword parameters are designated with the `&key` lambda list keyword.

## 5. Making assumptions

    - Keyword parameters can have default values.

## 6. Standard amount of leftovers

    - Keyword parameters can have a "supplied-p parameter" like optional parameters.
