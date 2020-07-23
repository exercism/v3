In this exercise you will be implement a software module for document management system.

You require to implement a PDF and Word export module to be plugged into a document Import/Export module.
You are provided with a two contracts/interfaces to be implemented.

```csharp
public interface IPdf
{
	string Export();
}

public interface IWord
{
	string Export();
}
```

1. You need to implement the IPdf interface in your exporter class.

```csharp
	IPdf documentExporter = new DocumentExpoter();
	var data = documentExporter.Export();
	// => "Pdf data"
```

2. Your Pdf export implementation returns only Pdf data

```csharp
	IPdf documentExporter = new DocumentExpoter();
	var data = documentExporter.Export();
	// => "Pdf data"
```

3. You need to implement the IPdf interface in your exporter class.

```csharp
	IWord documentExporter = new DocumentExpoter();
	var data = documentExporter.Export();
	// => "Word data"
```

4. Your Word export implementation returns only Word data

```csharp
	IWord documentExporter = new DocumentExpoter();
	var data = documentExporter.Export();
	// => "Word data"
```
