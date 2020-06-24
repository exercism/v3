This exercise looks at how you test for equality and the importance of hash codes.

### `Object.Equals()`

- Simple types (strings and primitives) are typically tested for equality with the `==` and `!=`.
- Reference types (Instances of classes) are compared using the `Equals()` method inherited from `object`. If your goal with the equality test is to ensure that two objects are the exact same instance then relying on `object`'s implementation will suffice. If not, you need to override `object.Equals()`.
- If you know that all the instances of your class are created in one place, say characters in some game or simulation then reference equality is sufficient. However, it is likely that multiple instances of the same real-world entity will be created (from a database, by user input, via a web request). In this case values that uniquely identify the entity must be tested for equality. Therefore `Equals()` must be overridden and appropriate data members of your class are tested for equality.
- An overridden `Equals()` will contain equality tests on members of simple types using `==` and reference types with recursive calls to `Equals()`.
- The static method `object.ReferenceEquals()` is used to compare two objects to detect if they are one and the same instance.

### `Object.GetHashCode()`

- `object.GetHashCode()` returns a hash code in the form of a 32 bit integer. The hash code is used by dictionary and set classes such as `Dictionary<T>` and `HashSet<T>`to store and retrieve objects in a performant manner.
- The relationship between hash code and equality is that if two objects are equal (`Equal()` returns true) then `GetHashCode()` for the two objects must return the same value. This does not apply in the reverse direction. It is not symmetrical. Picture a lookup function that first goes to a "bucket" based on the hash code and then picks out the particular item using the equality test.
- The easiest way to create a hashcode is to call `HashCode.Combine()` passing in the values used in the equality test (or a subset). Bear in mind the more information you provide to `Combine()` the more performant the hash implementation is likely to be.
- The values used in the equality test must be stable while the hashed collection is in use. If you add an object to the collection with one set of values and then change those values the hash code will no longer point to the correct "bucket". In practice this means that the object should be immutable. Other approaches run the risk of creating gotchas for maintainers.
