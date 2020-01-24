# Concepts of Grade School

### General concepts
- [Using class fields](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/fields) as opposed to local variables so they can be used throughout the entire class
- [Collections](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/collections) like `List`, `Dictionary`, or `IEnumerable` to store data and objects
- [LINQ queries](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/query-syntax-and-method-syntax-in-linq) to interact with data in collections
- [Access modifiers](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/access-modifiers) for classes, fields, and methods
- [Class properties](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/properties)

---

### Approach: Using a dictionary - Shows student how to make us of built in objects and makes use of SortedDictionary so keys are already sorted

- [SortedDictionary](https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.sorteddictionary-2?view=netframework-4.8) object to hold string (name) as key and int (grade) as value 
- [Add to](https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.sorteddictionary-2.add?view=netframework-4.8) SortedDictionary collection to store keys and values
- Using `Where`, `Select`, `OrderBy` LINQ queries to sort and filter lists using the `keys` and `values` in the dictionary

```csharp
public class GradeSchool
{
    private readonly SortedDictionary<string, int> _roster = new SortedDictionary<string, int>();

    public void Add(string student, int grade) => _roster.Add(student, grade);

    public IEnumerable<string> Roster() => _roster.OrderBy(x => x.Value)
        .Select(x => x.Key);

    public IEnumerable<string> Grade(int grade) => _roster.Where(x => x.Value == grade)
        .Select(x => x.Key)
        .OrderBy(x => x);
}
```

---

### Aproach: Using a list of objects - Shows student how to create their own objects which can also provide more readability by using variable names to get values by property names instead of `Key` and `Value`

- [Creating an object](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/objects) to store the student's name and grade
- [Initializing an object and a list of objects](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/object-and-collection-initializers) to store created object from previous step
- [Adding objects to a list](https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1.add?view=netframework-4.8)
- Using `Where`, `Select`, `OrderBy`, and `ThenBy` LINQ queries to sort and filter lists using the `keys` and `values` in the dictionary

```csharp
public class GradeSchool
{
    private readonly List<StudentInformation> _roster = new List<StudentInformation>();

    public void Add(string student, int grade) => _roster.Add(new StudentInformation(student, grade));

    public IEnumerable<string> Roster() => _roster.OrderBy(x => x.Grade)
        .ThenBy(x => x.StudentName)
        .Select(x => x.StudentName);

    public IEnumerable<string> Grade(int grade) => _roster.OrderBy(x => x.StudentName)
        .Where(x => x.Grade == grade)
        .Select(x => x.StudentName);
}

public class StudentInformation
{
    public StudentInformation(string studentName, int grade)
    {
        StudentName = studentName;
        Grade = grade;
    }

    public string StudentName { get; }

    public int Grade { get; }
}
```
