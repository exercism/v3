The Java lists are a collection of items, ordered and that don't have a fixed size. They can grow dynamically.

It is an interface, so we can't instantiate it directly. There are several implementation in the base library, for example :

```java
List<T> arraylist = new ArrayList<T>();
List<T> linkedlist = new LinkedList<T>();
List<T> vector = new Vector<T>();
List<T> stack = new Stack<T>();
```

With all these implementations you can use the functions of the List, that you can find [here][list-doc].
You could also create your own implementation if you have the need.

#### Generic Types

The `<T>` lets the List be parameterized over types. It can be used for other generic classes or interfaces.
By changing the T to a specific type, the list can only contain this type.

```java
List<Integer> intList = new ArrayList<Integer>();
List<String> stringList = new ArrayList<String>();
List<Object> objectList = new ArrayList<Object>();
List<ArrayList<Integer>> arraylistList = new ArrayList<ArrayList<Integer>>();
```
As you can see, it can become quite heavy in terms of verbosity. Java then has the diamond operator `<>`. It lets the Java compiler take the most suitable constructor for the invocation.
So these are equivalent type-defined Lists.
```java
List<Integer> list1 = new ArrayList<Integer>();
List<Integer> list2 = new ArrayList<>();
```

We can still use the lists without specifying the type at all, but it is more laborious. You would have to cast the elements each time you get an item from the list. So, this works but should be used with care :
```java
List list = new ArrayList();
list.add(1);
Integer i = (Integer) list.get(0);
```

You can get more informations on generic Types in the [documentation][generic-doc].

#### Reference

- [List Documentation][list-doc] : reference documentation for `List<T>`
- [List Tutorial][list-tutorial] : tutorial on lists
- [Generic Types Documentation][generic-doc] : documentation on Generic Types
- [Generic Types Tutorial][generic-tutorial] : tutorial on Generic Types

[list-doc]: https://docs.oracle.com/javase/8/docs/api/java/util/List.html
[generic-doc]: https://docs.oracle.com/javase/tutorial/java/generics/types.html
[list-tutorial]: https://www.c-sharpcorner.com/article/java-list/
[generic-tutorial]: https://www.baeldung.com/java-generics
