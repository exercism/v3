In this exercise you'll be writing code to helping printing name
badges for the employees on a factory.

On our system, people may have a first name, a middle name and a
family name. They are all printed on a single line on a plastic badge.

For examle, if my name is `Ernest Johnny Payne`, it should appear as
follows in my badge:

```
ERNEST JOHNNY PAYNE
```

Your task will be writing a static method called `ComputeWidthPx`. It
will take as input parameters the first, middle and last names of the
employee and the font size that is going to be used to print the name.

For a font size of 12px, the total width of the name on the badge will
be:

```
6*12 + 1*12 + 6*12 + 1*12 + 5*12 = 228
```

PS 1: Notice that the spaces between words take space on the badge.

PS 2: Let us assume that the three parametres are optional, i.e. an
employee may have no first name, no middle name or no family name on
the system.

