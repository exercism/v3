Arithmetic overflow occurs when a computation such as an arithmetic operation or type conversion results in a value that is greater than the width in bits of the receiving type.

Expressions of type `int` and `long` and their unsigned counterparts will quietly wrap around under these circumstances.

The behavior of integer computations can be modified by using the `checked` keyword. When an overflow occurs within a `checked` block an insance of `OverflowException` is thrown.

```csharp
var xx = new Random().Next();
checked
{
    int expr = int.MaxValue * xx;
}

// or

int expr2 = checked(int.MaxValue * xx);
```

Expressions of type `float` and `double` will take a special value of infinity.

Expressions of type `decimal` will throw an instance of `OverflowException`.
