In C#, a _class_ hierarchy can be defined using _inheritance_. This allows a specific, derived class (`Car`) to inherit the behavior and data of its more generic, parent class (`Vehicle`). The derived class can access its parent's `public` _and_ `protected` members. The `protected` access modifier allows a member to be accessed by code in the same class or in any derived class.

```csharp
class Vehicle
{
    private int id;       // Not accessible in derived classes
    protected int wheels; // Accessible in derived classes
    public int weight;    // Accessible in derived classes
}

class Car : Vehicle
{
    public string Print()
    {
        // Access the parent's protected and public members
        return $"Wheels: {wheels}, weight: {weight}";
    }
}
```

A base class class can also provide a _default_ implementation of a method, but allowing for derived classes to change it. To do so, a method needs to be marked as `virtual`. This allows the derived class to `override` the method with its own functionality:

```csharp
class Vehicle
{
    public virtual string Stop()
    {
        // Generic stop functionality
    }
}

class Car : Vehicle
{
    public override string Stop()
    {
        // Use custom stop functionality
    }
}

class Ship : Vehicle
{
    // Inherits `Stop` functionality from parent `Vehicle` class
}
```

If you want to prevent instances of the parent class from being created, you can add the `abstract` modifier to it:

```csharp
abstract class Vehicle
{
}

class Car : Vehicle
{
}

// Compiler error
var vehicle = new Vehicle();

// No compiler error
var car = new Car();
```

Constructors can be made `protected` to limit access. A derived class' constructor will automatically call its parent's constructor _before_ executing its own constructor's logic. If the parent class' constructor requires arguments to be passed, the derived class can pass them using the `base` keyword, which refers to the parent class:

```csharp
abstract class Vehicle
{
    protected Vehicle(int wheels)
    {
        // This gets executed first. The value for wheels
        // is passed from a derived class' constructor
    }
}

class Car : Vehicle
{
    public Car() : base(4) // Call the parent's constructor
    {
        // This gets executed after the parent constructor
    }
}
```

An abstract class can also act as a _template_, where it defines a method without any actual implementation using the `abstract` keyword. Derived classes are then forced to implement this method using the `override` keyword; not doing so results in a compiler error:

```csharp
abstract class Vehicle
{
    public abstract void Stop();
}

class Car : Vehicle
{
    public override void Stop()
    {
        // ...
    }
}

class Ship : Vehicle
{
    // Results in a compiler error
}
```

If a class doesn't specify a parent, the compiler will have it inherit from the `object` class. This holds for _any_ C# type, including built-in types likes `string` and `int`. In other words, the functionality in the `object` class is available to all types:

```csharp
class VehicleImplicitParent
{
}

// Equivalent to the above definition
class VehicleExplicitParent : object
{
}

var vehicle = new VehicleImplicitParent();
var number = 1;

// Call method inherited from the 'object' class
vehicle.ToString(); // => "VehicleImplicitParent"
number.ToString(); // => "1"
```
