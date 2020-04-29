The primary object-oriented construct in C# is the _class_, which is a grouping of related data (_fields_) and behavior (_methods_). The fields and methods of a class are known as its _members_.

Access to members can be restricted through access modifiers, the two most common ones being:

- `public`: the member can be accessed by any code (no restrictions).
- `private` (default): the member can only be accessed by code in the same class.

The above-mentioned grouping of related data and behavior plus restricting access to members is known as _encapsulation_, which is one of the core object-oriented concepts.

You can think of a class as a template for creating instances of that class. To create an instance of a class, the `new` keyword is used:

```csharp
class Car
{
}

// Create two car instances
var myCar = new Car();
var yourCar = new Car();
```

Fields have a name and a type and can be defined anywhere in a class:

```csharp
class Car
{
    // Public field, accessible by anyone
    public int weight;

    // Private field, only accessible by code in this class
    private string color;
}
```

One can assign an initial value to a field. If a field does not have an initial value, its default value will be used (this is different per type)

```csharp
class Car
{
    public int weight = 2500;
    public int year;
}

var newCar = Car();
car.weight; // => 2500
car.year;   // => 0 (default value for int)
```

The values of the fields of a class instance are often referred to as its _state_. If a field is `public`, a new value can be assigned to it by using the dot-syntax:

```csharp
var lightCar = new Car();
lightCar.weight = 1200;
lightCar.weight; // => 1200
```

An instance's fields can be updated either by assigning a new value to them or by using a method to update a field.

```

```
