Functions in Kotlin are defined with keyword **`fun`** and are [first-class citizens][wiki-fcc] (not related to OOP). It means that you can define your top-level functions just in files (e.g. in Java you can define methods only in classes, not in files):
```kotlin
// This is content of the Hello.kt file

fun hello() {}
```

Functions can receive arguments. Each argument have a name and a type. In many cases compiler know how to inference types, but you always need to write types for function arguments by yourself to define proper contract. Functions can have zero or more arguments:
```kotlin
fun hello() {}

fun hello(name: String) {}

fun hello(name: String, age: Int) {}
```

Kotlin functions always returns value. Either explicitly (with specified type):
```kotlin
fun min(a: Int, b: Int): Int

fun countBonuses(user: User): Bonuses

fun run(): Unit {}
```

Or implicitly. Return type will be `Unit` (similar to `void` from C/Java) and can be omitted:

```kotlin
fun run() {}

// is the same as
fun run(): Unit { return Unit }
```

As you can see, to return value from function you need to use keyword **`return`**:
```kotlin
fun getName(): String {
    return "Alice"
}
```

Functions can have parameters with default values. These values will be used if they are omitted where function is invoked:
```kotlin
fun ping(host: String = "localhost") {}

ping("exercism.io")  // "exercism.io" will be used as a host to ping 
ping()               // "localhost" will be used as a host to ping
```


[wiki-fcc]: https://en.wikipedia.org/wiki/First-class_citizen
