A property in C# is a member of a class that provides access to data within that class.
Callers can set or retrieve (get) the data.  Properties can be either auto-implemented or
have a backing field.  

When setting a property the input value can be validated, formatted
or otherwise manipulated and in fact any programmatic operation accessible to code in the
class can be executed.  

Similarly when retrieving a property data can be calculated or formatted and again
any programmatic operation available to the class can be executed.

Properties have access modifiers (`public`, `private` etc.) in the same way as other
class members but the set accessor may have an access level independent of the retrieve (get)
accessor and vice versa or either accessor may be dispensed with completely.

The basic syntax to express properties can take two forms:

###### Field/Expression Backed Properties:
``` csharp
private int myField;
public int MyProperty
{
    get { return myfField; }
    set { myField = value; }
}
```
###### Auto-implemented Properties
```
public int MyProperty { get; private set; } = 42;
```
Initialisation is optional.
