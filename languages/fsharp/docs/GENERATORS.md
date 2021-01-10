# Test Generators

Test generators allow tracks to generate tests automatically without having to write them ourselves. Each test generator reads from the exercise's `canonical data`, which defines the name of the test, its inputs, and outputs. You can read more about exercism's approach to test suites [here](https://github.com/exercism/docs/blob/master/language-tracks/exercises/anatomy/test-suites.md).

Generating tests automatically removes any sort of user error when creating tests. We want the tests to be accurate with respect to its canonical data. Test generation also makes it much easier to keep tests up to date. As the canonical data changes, the tests will be automatically updated when the generator for that test is run.

An example of a canonical data file can be found [here](https://github.com/exercism/problem-specifications/blob/master/exercises/bob/canonical-data.json)

## Common Terms

When looking through the canonical data and the generator code base, we use a lot of common terminology. This list hopefully clarifies what they represent.

* Canonical Data - Represents the entire test suite.
* Canonical Data Case - A representation of a single test case.
* Description - The name of the test.
* Property - The method to be called when running the test.
* Input - The input for the test case.
* Expected - The expected value when running the test case.

## Adding a simple generator

Adding a test generator is straightforward. Simply add a class to the `Generators.fs` file in the `generators` folder with the name of the exercise (in PascalCase), and extend the `GeneratorExercise` abstract class.

An example of a simple generator would be the Bob exercise. The source code can be found below, but you can also view it in the repository [here](https://github.com/exercism/fsharp/blob/master/generators/Generators.fs#L174).

```fsharp
type Bob() =
    inherit GeneratorExercise()
```

This is a fully working generator, no other code needs to be written. However, it's simplicity stems from the fact that the test suite and the program itself are relatively trivial.

## Adding a complex generator

For many generators, the default behavior of the `GeneratorExercise` class suffices. However, for more _complex_ generators, one can override one or more of its virtual methods:

### Method: RenderInput

Render the input of a test method.

#### Example

The `Gigasecond` generator renders the input value as a (parenthesized) `DateTime` value:

```fsharp
override __.RenderInput (_, _, value) =
    DateTime.Parse(string value, CultureInfo.InvariantCulture)
    |> DateTime.renderParenthesized
```

Note that you could use the `key` field to only customize the output for specific input fields. The `Yacht` generator does this:

```fsharp
override __.RenderInput (canonicalDataCase, key, value) =
    match key with
    | "category" -> Obj.renderEnum "Category" value
    | _ -> base.RenderInput (canonicalDataCase, key, value)
```

### Method: RenderExpected

Render the expected value of a test method.

#### Example

The `Connect` generator renders the expected value as an `Option<T>` value:

```fsharp
override __.RenderExpected (canonicalDataCase, key, value) =
    match string value with
    | "O" -> "(Some White)"
    | "X" -> "(Some Black)"
    | _   -> "None"
```

### Method: RenderSut

Render the System Under Test (SUT). This is the actual value that is verified against the expected value.

#### Example

The `RunLengthEncoding` generator has a test case where it verifies that encoding and then decoding the input results in the same value. The `RenderSut` method is overridden to convert what would be a single call to a non-existing `consistency` function, to a chain of `encode |> decode` calls:

```fsharp
override this.RenderSut canonicalDataCase =
    match canonicalDataCase.Property with
    | "consistency" ->
        let parameters = this.RenderSutParameters canonicalDataCase |> String.concat " "
        sprintf "%s |> encode |> decode" parameters
    | _ ->
        base.RenderSut canonicalDataCase
```

### Method: RenderArrange

Render the arrange part of the test method. In this part, the SUT is created.

The default behavior is usually what you want. Only in very special circumstances would you want to change this.

Note: this method returns a `string list`, where each `string` corresponds to a single, rendered line in the test file.

### Method: RenderAssert

Render the assert part of the test method. In this part, the assertion is made to verify the SUT against the expected value.

The default behavior is usually what you want. Only in very special circumstances would you want to change this.

Note: this method returns a `string list`, where each `string` corresponds to a single, rendered line in the test file.

### Method: RenderSetup

Render any additional utility/setup methods. This can be used to define helper methods that can be used in the test methods.

The setup code is added _before_ the test methods, to enable the setup methods to be used in the test methods.

Note: this method returns a `string list`, where each `string` corresponds to a single, rendered line in the test file.

#### Example

The `Zipper` generator adds two helper methods that are used in the test methods to remove some tedious, boilerplate code:

```fsharp
override __.RenderSetup _ =
    [ "let subTree value left right = Some (tree value left right)"
      "let leaf value = subTree value None None" ]
    |> String.concat "\n"
```

### Method: RenderValue

Render a single value (which is used to render input parameters, the expected value and the SUT).

### Example

The `ComplexNumbers` generator renders different values based on the value's type:

```fsharp
override __.PropertiesWithIdentifier canonicalDataCase =
    match canonicalDataCase.Expected.Type with
    | JTokenType.Array -> ["sut"]
    | _ -> base.PropertiesWithIdentifier canonicalDataCase
```

### Method: MapCanonicalDataCase

Map the canonical data case. This can be useful if you want to add or remove properties.

The default behavior is usually what you want. Only in very special circumstances would you want to change this.

#### Example

The `ScaleGenerator` generator uses the canonical data case mapping to add a missing property as a `null` value (which makes processing later easier):

```fsharp
override __.MapCanonicalDataCase canonicalDataCase =
    let input = canonicalDataCase.Input
    match Map.tryFind "intervals" input with
    | Some _ -> canonicalDataCase
    | None   -> { canonicalDataCase with Input = Map.add "intervals" null input }
```

### Method: PropertiesUsedAsSutParameter

Specifies which properties should be used as parameters to the SUT.

The default behavior is usually what you want. Only in very special circumstances would you want to change this.

#### Example

The `RailFenceCipher` explicitly defines the properties used as SUT parameters to change the order in which they are passed:

```fsharp
override __.PropertiesUsedAsSutParameter _ = ["rails"; "msg"]
```

### Method: PropertiesWithIdentifier

Specifies which properties should have an identifier assigned to them. If a property is in this list, it will be assigned its value on a separate line, to its own identifier.

#### Example

The `Transpose` generator has chosen to use identifiers for all properties:

```fsharp
override this.PropertiesWithIdentifier canonicalDataCase = this.Properties canonicalDataCase
```

### Method: IdentifierTypeAnnotation

In some cases, you want an identifier to have an explicit type. A common use case is when a value is an empty list. In those case, FsUnit needs a type annotation for it to successfully execute the assertion.

#### Example

The `Minesweeper` generator adds a `string list` type annotation for empty values:

```fsharp
override __.IdentifierTypeAnnotation (_, _, value) =
    match Seq.isEmpty value with
    | true  -> Some "string list"
    | false -> None
```

### Method: AdditionalNamespaces

Returns a list of additional namespaces to open in the test file.

#### Example

The `Gigasecond` generator works with `DateTime` instances, and thus adds its namespace to the additional namespaces list:

```fsharp
override __.AdditionalNamespaces = [typeof<DateTime>.Namespace]
```

### Method: AssertTemplate

The assert template to use. You can find the full list of assert templates in `generators/Templates/_Assert*.liquid`

#### Example

The `RationalNumbers` generator uses the `"AssertEqualWithin"` template to allow for checking doubles for equality with a specific tolerance:

```fsharp
override __.AssertTemplate canonicalDataCase =
    match canonicalDataCase.Expected.Type with
    | JTokenType.Float -> "AssertEqualWithin"
    | _ -> base.AssertTemplate(canonicalDataCase)
```

### Method: TestFileFormat

The test file's format: either a module or a class.

The default behavior is virtually always what you want. Only in extremely rare cases would you want to change this.

#### Example

The `Grep` generator defines itself as a test class to allow using setup and teardown methods:

```fsharp
override __.TestFileFormat = TestFileFormat.Class
```

### Method: TestMethodName

Specifies the test method's name.

The default behavior is usually what you want. Only in very special circumstances would you want to change this.

#### Example

The `Zipper` generator does some test method name cleanup:

```fsharp
override __.RenderTestMethodName canonicalDataCase =
    let testMethodName = base.RenderTestMethodName canonicalDataCase
    testMethodName.Replace("Set_", "Set ")
```

### Method: UseFullMethodName

Indicates if the test method name that is used should use the full path, which means that for nested test cases, it will prepend the test method name with the parent(s) description(s).

The default behavior is usually what you want. Only in cases where there would otherwise be multiple test methods with the same name would you want to change this.

#### Method: Signature

The `KindergartenGarden` generator uses full method names to prevent test method name clashes:

```fsharp
override __.UseFullMethodName _ = true
```

### Method: SkipTestMethod

Indicates if the test method should be marked with the `Skip = true` property.

The default behavior is virtually always what you want. Only in extremely rare cases would you want to change this.

#### Method: Signature

The `Markdown` exercises is a refactoring exercise, and as such all its tests should already be in a valid state. Therefore, it doesn't add the `Skip` property for any of its test methods:

```fsharp
override __.SkipTestMethod (_, _) = false
```

## Updating existing files

It is possible that an existing exercise does not match the canonical data. It is OK to update the exercise stub and/or the exercise example to follow the canonical data!

Also, if you find an issue with one of the existing generators or test suites simply open up the generator that you would like to update, make your changes, and then run the generators.

## Running the generators

This repository is coded against [.NET Core 2.x](https://www.microsoft.com/net/core). To run the generators all you need to do is run the following command in the generators directory:

`dotnet run`

This command will take all of the exercise generators that are defined, and generate all of the test cases for each exercise. We use reflection to get all of the exercises, so if you are adding a new test, the test will be automatically included when running the generator.

If you only need to run a single generator, you can do so by running the following command:

`dotnet run -e <exercise>`

Once the generator has been run, you can view the output of your generation by navigating to the test file for that exercise. As an example, the test suite for the Bob exercise can be found at:

`exercises/bob/BobTests.fs`

## Submitting a generator

If you are satisfied with the output of your generator, we would love for you to submit a pull request! Please include your generator, updated test suite, and any other corresponding files that you may have changed.
