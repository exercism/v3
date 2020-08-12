In Java, a List is an interface for an ordered _collection_ of elements. The elements can be inserted, removed, accessed or iterated following an order that depends of the implementation of the List. These elements can also be accessed with an integer index, beginning at 0.

As it is an interface, there are several implementations. We will use here the most commonly used : the ArrayList.
```java
List list = new ArrayList();

list.add(new Integer(1));
list.add("Java")
```

As shown above, we can add elements of several types in this List. We try to avoid this as we can't know the type of the elements that we will get from the List.

It is where the _generic type_ comes. We can define the List to only contain elements of a certain type, by putting it into brackets during the declaration. It is how the lists are generally used.

```java
List<Integer> list = new ArrayList<Integer>;
list.add(new Integer(1));
list.add(5);
```

One of the main advantages to use lists are that the size is not fixed : we can remove or add elements without having to specify the size at the creation.
