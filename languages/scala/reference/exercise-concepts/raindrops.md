# Rain drops

[Example implementation](https://github.com/exercism/scala/tree/master/exercises/raindrops)
[Other implementation](https://scastie.scala-lang.org/JixK2hdPQ4ier47Wj9aM9A):

```
object Raindrops {
  private val sounds = Map(
    3 -> "Pling",
    5 -> "Plang",
    7 -> "Plong"
  )

  def convert(n: Int): String =
    sounds
      .filterKeys(n % _ == 0)
      .values match {
      case s if s.isEmpty => n.toString
      case s              => s.mkString
    }
}
```

## General

- object or case class: used as class wrapper for exercise
- functions: used as main entry point for the exercise
- function arguments: input strands passed as arguments
- return values: return a value from a function
- arithmetic operators: mod `%`: to build condition based on factors
- String: chain of character, manipulate text
- Int: integer type, manipulate numbers
- Boolean: boolean type, true/false, manipulate conditions
- tuple: pair of values: `( ??? , ??? )`
- pattern matching: to treat cases based on condition
- condition in pattern matching: to execute branches of the pattern matching based on predicate

## Map/List

- Map: to store key/value pairs
- List: to store the values of the song
- collection - filtering: filterKeys: to remove entry of Map based on predicate applied to keys
- collection - mapping: values: to remove the key from the key/value pairs and only keep the values
- List.isEmpty: to test if a list/map is empty
- collection - filtering: to filter list or map based on predicate
- collection - reduce: mkString: to turn a list into a String
