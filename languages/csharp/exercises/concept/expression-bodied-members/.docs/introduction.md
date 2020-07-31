Many types of struct and class members (fields being the primary exception) can use the expression-bodied member syntax. Defining a member with an expression often produces more concise and readable code than traditional blocks/statements.

Methods and read-only properties are amongst the members that can be defined with expression bodies.

```csharp
int Times3(int input) => input * 3;

int Interesting => 1729;
```

#### Ternary operators

Ternary operators allow if-conditions to be defined in expressions rather than statement blocks. This echoes functional programming approaches and can often make code more expressive and less error-prone.

```csharp
int a = 3, b = 4;
int max = a > b ? a : b;
// => 4
```

`throw` expressions are an alternative to `throw` statements and in particular can add to the power of ternary and other compound expressions.

```csharp
string trimmed = str == null ? throw new ArgumentException() : str.Trim();
```

#### Switch expressions

A switch expression can match a value to one case in a set of patterns and return the associated value or take the associated action. The association is denoted by the `=>` symbol. In addition each pattern can have an optional case guard introduced with the `when` keyword. The case guard expression must evaluate to true for that "arm" of the switch to be selected.

```case
double xx = 42d;

string interesting = xx switch
{
    0 => "I suppose zero is interesting",
    3.14 when DateTime.Now.Day == 14 && DateTime.Now.Month == 3 => "Mmm pie!",
    3.14 => "π",
    42 => "a bit of a cliche",
    1729 => "only if you are a pure mathematician"
    _ => "not interesting"
};

// => interesting == "a bit of a cliche"
```
