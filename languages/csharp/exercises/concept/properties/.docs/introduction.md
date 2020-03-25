A `property` in C# is a member of a class that provides access to attributes of that class.
Callers can set or retrieve the attribute.  Properties can be either auto-implemented or
have a backing field.  

When setting a property the input value can be validated, formatted
or otherwise manipulated and in fact any programmatic operation accessible to code in the
class can be executed.  

Similarly when retrieving a property data can be calculated or formatted and again
any programmatic operation available to the class can be executed.

Properties have access modifiers (`public`, `private` etc.) in the same way as other
class members but the set accessor may have an access level independent of the retrieve (get)
accessor and vice versa or either accessor may be dispensed with completely.

The intention behind the syntax of a property `object.Myproperty = x` or `var x = object.MyProperty`
should be respected.  They are intended to be used for instantaneous operations.  Although
there is a great deal of freedom of operation (as described above) any be the shortest
operations should be avoided.
