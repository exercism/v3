In this exercise you'll be writing code to helping printing name
badges for factory employees.

Employees have an id, a name and a department name.  For example, if
my id is `734`, my name is `Ernest Johnny Payne`, and my department is
`Strategic Communication` my badge name is: `[734] - Ernest Johnny
Payne - STRATEGIC COMMUNICATION`.

Your task will be writing a static method called `Badge.Label`. It
will take as input parameters the id, name and department of an
employee. You will then compute the string to be printed on the
badge. 

Notice that you'll need to include dashes between words when
applicable and write the department in UPPER CASE.

You will also need to deal with people who have no department. Just
print `GUEST` as their department name.

Since badges have physical size, you need to wrap the text if it gets
too long.

Your second task will be writing a static method called
`Badge.PrintLabel`. It will take a string generated with the first
method and the width of the badge and add a new line character on the
right points on the string so that it fits on the provided width.

So for example, if the label to print is `[734] - Ernest Johnny
Payne - STRATEGIC COMMUNICATION` and the width of the badge has a
maximun width of 30 characters, your method should return the
following string:

```
[734] - Ernest Johnny Payne - 
STRATEGIC COMMUNICATION
```

The width is optional, so if it is not provided, just return the
string as is.
