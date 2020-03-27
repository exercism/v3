In this exercise you'll be modelling a weighing machine.

The weight (`InputWeigh`) can be set and retrieved in pounds or kilograms.  A negative
value cannot be input.

The weight can be displayed in SI units (`DisplayWeight`) or US units (`USDisplayWeight`)
, pounds and ounces.

A tare adjustment (`TareAdjustment`) can be applied to the weight (for instance to deduct the
weight of a container).  This can be any value (even negative or a value that makes the display weight negative) 
as there are doubts about the accuracy
 of the weighing machine.  For security reasons this value cannot be retrieved.

Note that:
```
display-weight = input-weight - tare-adjustment
```

Conversion ratios are as follows:
- 16 ounces to a pound
- 2.20462 kg to a pound

For Example:
- 60 kilograms == 132.2772 ponds
- 132.2772 pounds == 132 pounds 4 ounces

To summarise the following properties must be implemented:
- `WeighingMachine.InputWeight`
- `WeighingMachine.DisplayWeight`
- `WeighingMachine.Units`
- `WeighingMachine.TareAdjustment`
- `WeighingMachine.USDisplayWeight`
- `USWeight.Pounds`
- `USWeight.Ounces`

You have 5 tasks each of which requires you to implement one or
more properties:

### 1 Allow the weight to be set on the weighing machine 

Implement the `WeigingMachine.InputWeight` property to allow the weight to be get and set
``` csharp
    var wm = new WeighingMachine();
    wm.InputWeight = 60m;

    //  => wm.InputWeight == 60m
```

### 2 Ensure that a negative input weight is rejected.

Implement validation for the `WeighingMachine.InputWeight` to reject negative numbers
``` csharp
    try {
        var wm = new WeighingMachine();
        wm.InputWeight = -10m;
        }
  
   // => an ArgumentExcption is thrown

```

### 3 Allow the US weight to be retrieved

Implement the USDisplayWeight property and the USWeight class
``` csharp
    var wm = new WeighingMachine();
    wm.InputWeight = 60m;
    bw = wm.USDisplayhWeight;

    // => bw.Pounds == 132 && bw.Ounces == 4
```

### 4 Allow the machine's units to be set to pounds

Implement the `WeighingMachine.Units` property
``` csharp
    var wm = new WeighingMachine();
    wm.InputWeight = 175.5m;
    wm.Units = Units.Pounds;
    bw = wm.USDisplayWeight;

    // => bw.Pounds == 175 && bw.Ounces == 8

```

### 5 Allow a tare adjustment to be applied to the weighing machine

Implement the `WeighingMachine.TareAdjustment` and `WeighingMachine.DisplayWeight` properties
``` csharp
    var wm = new WeighingMachine();
    wm.InputWeight = 100m;
    wm.TareAdjustment = 10m;

    // => wm.DisplayWeight == 90m
```
