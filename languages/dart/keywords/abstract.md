# Abstract

The keyword `abstract` is used to define an *abstract class* - a [class][reference-class] that can not be instantiated 
directly, which means you can not create an [object][reference-object] from it. Conversely, a class that can be 
instantiated directly is called a *concrete class*.

An [interface][reference-interface] is a type that hides implementation details and show only functionality to the user. 
It lets you focus on **what** the object does instead of **how** it does it. In Dart, all classes are implicit interfaces.

Instead Dart has abstract types which can be used as interfaces, but do allow any and all methods to be implemented. So 
it's up to the user how much implementation, if any, is in the abstract class.

```dart
// This class is declared abstract and thus can't be instantiated.
abstract class CoffeeMachine {
  //Body of abstract class
}

// Abstract classes can't be instantiated.
CoffeeMachine coffeeMachine = CoffeeMachine();
```

An abstract class can have constructors (including [factories][reference-factory]), [fields][reference-member], 
[methods][reference-method] as well as [static][reference-static] [members][reference-member].
Among these only fields and methods can be abstract everything else, if defined, must have a concrete implementation. 
Abstract methods define the `class` behaviour. They can only exist within an `abstract class` and are delimiting by a 
semicolon (`;`).

```dart
abstract class CoffeeMachine {
  // Abstract method
  void makeEspresso(); 
  
  // Normal method
  void makeCoffee() { 
    print("Making coffee");
  }
}
```

Since abstract classes can have constructors and constructors in Dart can omit their body, a constructor without a body 
does not denote that it is abstract.

Constructors in Dart are always concrete.

```dart
// Concrete class
class MyClass1 {
  // Constructor without a body
  MyClass1.one();
  // Constructor with an empty body
  MyClass1.two(){}
}

// Abstract class
abstract class MyClass2 {
  // Constructor without a body
  MyClass2.one();
  // Constructor with an empty body
  MyClass2.two(){}
}
```

When concrete classes [extend][reference-extend] the abstract class, they have to [override][reference-override]
every abstract method but it is not mandatory to override normal method.

```dart
abstract class CoffeeMachine {
  // Abstract method
  void makeEspresso();
  
  // Normal method
  void makeCoffee() { 
    print("Making coffee");
  }
}

class SpecialCoffeeMachine extends CoffeeMachine {
  @override
  void makeEspresso() {
    print("Making a delicious espresso");
  }
}

SpecialCoffeeMachine specialCoffeeMachine = SpecialCoffeeMachine();

// Will print "Making coffee"
specialCoffeeMachine.makeCoffee();
```

It is also possible to create more specific interfaces, i.e. an abstract class can choose to extend another abstract 
class.

```dart
abstract class Node {}

abstract class LeftNode extends Node {}

abstract class RightNode extends Node {}
```

[reference-class]: ./class.md
[reference-extend]: ./extends.md
[reference-extend-override]: ./extends.md#override
[reference-factory]: ./factory.md
[reference-member]: ./class.md#member
[reference-interface]: ../../../reference/concepts/interfaces.md
[reference-method]: ./class.md#method
[reference-object]: ../../../reference/concepts/objects.md
[reference-static]: ./static.md