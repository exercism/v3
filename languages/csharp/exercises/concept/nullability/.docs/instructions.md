In this exercise you'll be writing code to helping printing name
badges factory employees.

Employees have a first and family name, and optionally a middle name
family name. An employee's badge consists of a single line with the employee's first, middle and family name on it (in that order), with the name parts separated by a single space.

For examle, if my name is `Ernest Johnny Payne`, it should appear as
follows in my badge:

```
ERNEST JOHNNY PAYNE
```

Your task will be writing a static method called `ComputeNameText`. It
will take as input parameters the first, middle and last names of the
employee. You will then compute the string to be print on the name
line of the badge. Notice that you'll need to include spaces between
words when applicable.

Your second task will be writing a static method called
`ComputeWidthPx`. It will take a string and an int (a font size) as
input and it will compute the width in pixels of the print text.

For a font size of 12px, the total width of the name of the string
`Ernest Johnny Payne` will be:

```
6*12 + 1*12 + 6*12 + 1*12 + 5*12 = 228
```

**PS**: Let us assume that the all parametres are optional, i.e. an
employee may have no first name, no middle name or no family name on
the system. Besides, if someone forgets to configure a proper font
size, your code should react accordingly.
