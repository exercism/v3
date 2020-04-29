The primary object-oriented construct in C# is the _class_, which is a grouping of related data ([_fields_][fields]) and behavior ([_methods_][methods]). The fields and methods of a class are known as its _members_.

Access to members can be restricted through access modifiers, the two most common ones being:

- [`public`][public]: the member can be accessed by any code (no restrictions).
- [`private`][private]: the member can only be accessed by code in the same class.

It is customary to specify an access modifier for all members. If no access modifier is specified, it will default to `private`.

The above-mentioned grouping of related data and behavior plus restricting access to members is known as _encapsulation_, which is one of the core object-oriented concepts.

You can think of a class as a template for creating instances of that class. To [create an instance of a class][creating-objects] (also known as an _object_), the [`new` keyword][new] is used:

```csharp
class Car
{
}

// Create two car instances
var myCar = new Car();
var yourCar = new Car();
```

[Fields][fields] have a type and a name (defined in camelCase) and can be defined anywhere in a class:

```csharp
class Car
{
    // Public field, accessible by anyone
    public int weight;

    // Private field, only accessible by code in this class
    private string color;
}
```

One can optionally assign an initial value to a field. If a field does _not_ have an initial value, its default value will be used:

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

Provided the field is accessible, its value can be updated:

```csharp
var lightCar = new Car();
lightCar.weight = 1200;
lightCar.weight; // => 1200
```

Private fields are usually updated as a side-effect of calling a method. Such methods usually don't return any value, in which case the return type should be [`void`][void]:

```csharp
class CarImporter
{
    private int carsImported;

    public void ImportCars(int numberOfCars)
    {
        // Update private field
        carsImported = carsImported + numberOfCars;
    }
}
```

Note that is not customary to use public fields in C# classes. Either private fields are used or other types of members that will be discussed in subsequent exercises.

Within a class, the [`this` keyword][this] will refer to the current class. This is especially useful if a parameter has the same name as a field:

```csharp
class CarImporter
{
    private int carsImported;

    public void SetImportedCars(int carsImported)
    {
        // Update private field from public method
        this.carsImported = carsImported;
    }
}
```

[fields]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/fields
[methods]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
[this]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/this
[new]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/new-operator
[void]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/void
[creating-objects]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/classes#creating-objects
[fields]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/fields
[public]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/public
[private]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/private
