# Python concept exercises

| **Concept Name**   | **Exercise Name**                                                                                                                      | **Included Concepts**                   | **Prerequisites**                                                                                                              |
| ------------------ | -------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `basics`           | [Guidos Gorgeous Lasagna](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/guidos-gorgeous-lasagna)       | `basics`                                | --                                                                                                                             |
| `bools`            | [ghost-gobble-arcade-game](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/ghost-gobble-arcade-game)     | `bools`                                 | `basics`                                                                                                                       |
| `comparisons`      | TBD (PR in progress)                                                                                                                   | `comparisons`                           | `basics`                                                                                                                       |
| `rich-comparisons` | TBD (split from comparisons)                                                                                                           | `rich-comparisons`                      | `comparisons`                                                                                                                  |
| `dicts`            | [Inventory Management](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/inventory-management)             | `dicts`                                 | `loops`, `lists`, `tuples`                                                                                                     |
| `enums`            | [log-levels](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/log-levels)                                 | `enums`                                 | `classes-I`, `conditionals`, `loops/iteration`, `comprehensions`, `sequences`, `string-formatting`, `string-methods`, `tuples` |
| `iteration/loops`  | [Making the Grade](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/making-the-grade)                     | `iteration`, `loops`, `range`           | `basics`, `comparisons`, `conditionals`, `lists`, `strings`                                                                    |
| `list-methods`     | [Chaitanas Colossal Coaster](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/chaitanas-colossal-coaster) | `list-methods`                          | `lists`                                                                                                                        |
| `lists`            | [Elyses Enchantments](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/elyses-enchantments)               | `list`                                  | `comparisons`, `conditionals`, `strings`                                                                                       |
| `none`             | [Restaurant Rozalynn](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/restaurant-rozalynn)               | `None`                                  | `bools`, `conditionals`, `functions-I`, `dict-methods`, `list-methods`, `loops/iteration`                                      |
| `numbers-I`        | Currency Exchange/Chandler's Conversion Confusion (PR in process)                                                                      | `numbers`, `ints`, `floats`, Arithmetic | `basics`                                                                                                                       |
| `numbers-II`       | TBD (PR in process)                                                                                                                    | `complex`, imaginary                    | `numbers-I`                                                                                                                    |
| `str`              | [Processing Logs](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/processing-logs)                       | `str`                                   | `basics`                                                                                                                       |
| `str-formatting`   | TBD (PR in process)                                                                                                                    | `str-formatting`                        | `basics`, `strings`, `string-methods-I`                                                                                        |
| `str-methods`      | [Litte Sister's Essay](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/little-sisters-essay)             | `str-methods`                           | `basics`, `strings`                                                                                                            |
| `str-methods-II`   | TBD (PR in process)                                                                                                                    | `str-splitting`, `string processing`    | `basics`, `strings`, `string-methods-I`                                                                                        |
| `tuples`           | [Tisbury Treasure Hunt](https://github.com/exercism/v3/tree/master/languages/python/exercises/concept/tisbury-treasure-hunt)           | `tuple`                                 | `bools`, `loops`, `conditionals`, `numbers-I`                                                                                  |

## Chart

```mermaid
flowchart TD

%%concepts & exercise names (node lables)
Basics((<b>Guidos Gorgeous Lasagna</b><br>Basics))
bools((<b>Ghost Gobble<br>Arcade Game</b><br>bools))
classes-I((<b>TBD</b><br>classes-I))
classes-II((<b>TBD</b><br>classes-II))
comparisons((<b>TBD</b><br>comparisons))
comprehensions(("<b>TBD</b><br>comprehensions<br>(list comprehensions)"))
other-comprehensions((<b>TBD</b><br>other-comprehensions))
conditionals((<b>TBD</b><br>conditionals))
dicts((<b>Inventory Management</b><br>dicts))
dict-methods((<b>TBD</b><br>dict-methods))
enums((<b>Log Levels</b><br>enums))
functions-I((<b>TBD</b><br>functions-I))
functions-II((<b>TBD</b><br>functions-II))
functions-III((<b>TBD</b><br>functions-III))
iterators((<b>TBD</b><br>iterators))
lists((<b>Elyse's Enchantments</b><br>lists))
list-methods((<b>Chaitana's<br>Colossal Coaster</b><br>list-methods))
loops((<b>Making the Grade</b><br>loops))
none((<b>Restaurant Rozalynn</b><br>none))
numbers-I(("<b>Chandler's<br>Conversion Confusion</b><br>numbers-I<br>(ints & floats)"))
numbers-II(("<b>TBD (Bowling Game??)</b><br>numbers-II<br>(complex numbers)"))
rich-comparisons((<b>TBD</b><br>rich-comparisons))
sequences((<b>TBD</b><br>sequences))
sets((<b>Cater-Waiter</b><br>sets))
str((<b>Processing Logs</b> <br>str type))
str-formatting((<b>TBD</b> <br>str-formatting))
str-methods((<b>Little Sister's Essay</b><br>str-methods))
tuples((<b>Tisbury Treasure Hunt</b><br>tuples))


%%exercise prerequisites (node relations)
Basics --> functions-I & str & loops & comparisons & conditionals & bools
Basics --> numbers-I & classes-I

bools --> tuples & none
classes-I --> enums & classes-II
comparisons --> lists & loops
comparisons --> rich-comparisons & conditionals
comprehensions --> enums & other-comprehensions
conditionals --> loops & lists & none & sets & tuples & enums
dicts --> dict-methods & other-comprehensions
dict-methods --> none
functions-I --> functions-II & none
functions-II --> functions-III
lists --> list-methods & enums & sequences & comprehensions & dicts
list-methods --> none
loops --> none & enums & iterators & comprehensions & dicts & sets & tuples
numbers-I -->  sets & tuples & numbers-II
sequences --> iterators & enums
sets --> other-comprehensions
str --> str-methods & str-formatting
str --> sequences & lists
str-methods & str-formatting --> enums
tuples --> sequences & dicts & enums
```
