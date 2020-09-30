Inheritance is a core concept in OOPS (Object Oriented Programming). It donates IS-A relationship.
It literally means in programming as it means in english, inheriting features from parent(in programming features is normally functions
and variables).

Consider a class, `Animal` as shown,

```java
//Creating an Animal class with bark() as a member function.
public class Animal {

    public void bark() {
        System.out.println("This is a animal");
    }

}
```

`Animal` is a parent class, because the properties this class has can be extended to all the animals in general.

Consider an animal named `Lion`, having a class like,

```java
//Lion class is a child class of Animal.
public class Lion extends Animal {

    public void bark() {
        System.out.println("Lion here!!");
    }

}
```
Now whenever we do,

```java
Animal animal = new Lion(); //creating instance of Animal, of type Lion
animal.bark();
```
Note: Initialising the `Animal` class with `Lion`. This talks about another concept called [Abstraction][Abstraction].
The output will look like

```java
Lion here!!
```
[Abstraction]:https://www.geeksforgeeks.org/abstraction-in-java-2/
