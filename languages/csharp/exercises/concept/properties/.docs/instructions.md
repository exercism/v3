In this exercise you'll be modelling a weighting machine.

You can set and get the weight in kilograms or pounds depending on which units have been selected for
the weighing machine.   This is the "input weight".  The input weight must never
be entered as a negative number.

In addition to being able to retrieve the input weight, 
the machine actually displays a related value called the "display weight".
This may be different to the input weight because a "vanity factor" is
applied (see below).

If you don't like your weight you can change it it by applying a vanity factor.  
In the interests of privacy nobody should be able to retrieve that value.
A typical value would be 5 or 10 to reduce the display weight by 5% or 10%.
The factor has no restrictions on it.  If you want increase the weight or
to reduce the display weight to zero or even have a negative weight, that is OK. 

Note that:
```
display-weight = input-weight * (100 - vanity-factor) / 100
```

You can set the weighing machine to record and display the weight in either
kilograms or pounds.  When you set the units no conversion takes place
which means you can set the weight as a number and then subsequently
select the units.

Unfortunately the British author of this exercise has insisted on being able to
retrieve (although not record) the display weight using his county's idiosyncratic system of
weights and measures - stones, pounds and ounces.

Conversion ratios are as follows:
- 14 pounds to a stone
- 16 ounces to a pound
- 2.20462 kg to a pound

For Example:
- 60 kilograms == 132.2772 ponds
- 132.2772 pounds == 9 stones 6 pounds 4 ounces

To summarise the following properties must be implemented:
- `WeighingMachine.InputWeight`
- `WeighingMachine.DisplayWeight`
- `WeighingMachine.Units`
- `WeighingMachine.VanityFactor`
- `WeighingMachine.BritishWeight`
- `BritishWeight.Stones`
- `BritishWeight.Pounds`
- `BritishWeight.Ounces`

You have 5 tasks each of which requires you to implement one or
more properties:

### 1 Implement the `WeigingMachine.InputWeight` property so that it allows the caller to set and get the weight and supports the following usage:
```
    var wm = new WeighingMachine();
    wm.InputWeight = 60f;
```
Ensure that
```
    wm.InputWeight == 60f
```

### 2 Implement validation for the `WeighingMachine.InputWeight` ensuring that a negative input is rejected and the following usage is supported:
```
    try {
        var wm = new WeighingMachine();
        wm.InputWeight = -10f;
        }
```
Ensure that the following occurs
```
   catch (ArgumentException) 

```

### 3 Implement the `WeighingMachine.BritishWeight` property and all properties of the `BritishWeight` class to support the following usage:
```
    var wm = new WeighingMachine();
    wm.InputWeight = 60f;
    bw = wm.BritishWeight;
```
Ensure that;
```
    bw.Stones == 9 && bw.Pounds == 6 && bw.Ounces == 4
```

### 4 Implement the `WeighingMachine.Units` property to allow the machine's units to be set to pounds to support the following usage:
```
    var wm = new WeighingMachine();
    wm.InputWeight = 175f;
    wm.Units = Units.Pounds;
    bw = wm.BritishWeight;
```
Ensure that:
```
    bw.Stones == 12 && bw.Pounds == 7 && bw.Ounces == 0

```

### 5 Implement the `WeighingMachine.VanityFactor` and `WeighingMachine.DisplayWeight` properties to allow the display weight to be calculated as above and support the following usage:
```
    var wm = new WeighingMachine();
    wm.InputWeight = 100;
    wm.VanityFactor = 10;
```
Ensure that:
```
    wm.DisplayWeight == 90;
```
