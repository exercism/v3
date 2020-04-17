# Flatten Array

[Example implementation](https://github.com/exercism/scala/blob/master/exercises/flatten-array/example.scala)

[Other implementation](https://scastie.scala-lang.org/OVTmCIofR5WrWsetxVpDGQ):

```
  def flatten(list: List[Any]): List[Int] = {
    list.flatMap {
      case null => Nil
      case int: Int => List(int)
      case list: List[Any] => FlattenArray.flatten(list)
    }
  }
```

## General

- object or case class: used as class wrapper for exercise
- functions: used as main entry point for the exercise
- function arguments: input strands passed as arguments
- return values: return a value from a function
- recursion: function calling the function to process the list items
- pattern matching: to deal with each item differently based on its type
- Any: to represent all types as input
- Int: is used as output of the function
- null: element of input list
- Nil: short cut for empty list
- generics: use a type parameter to specialize a type : `F[A]`
- list: contains several items of the same type (arrays)
- collection - filtering: used with condition to eliminate nulls
- collection - mapping: flatMap: to iterate through the list, map + flatten
