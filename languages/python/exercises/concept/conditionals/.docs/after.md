## General

Uptil now most program examples seen in the concept exercises ran in a sequential manner. Where code executed on after the other. However, this doesn't allow for implementation of more complex logic. Conditional statements allows program to execute different lines of code based on some factors. Python uses `if/else` statements to achieve this.

Let's look at basic structure of python's `if/else` and lets make a real world analogy.

```python
if <expression>:
    <statement >
```

Now look at the following examples

```python
x = 5
y = 10

if x > y:                           # expression is True
    print("x is greater than y")
>>> x is greater than y

if x < y:                           # expression is False
    print("x is greater than y")
>>>                                 # Nothing is executed

```

Here in first if statement the expression evaluvates to `True` therfore the statement associated with the if statment gets executed. In the next example the expression evaluvates to `False` and therfor the statement associated with it is not executed.

## The indentation and blocks

The python examples follow shown above the statemnts to be executed are arranged into blocks using indentation. So if there are multiple expressions that needs to be executed from one if statement we can put it in a block by having a uniform indentation throughout.

```python
if x > y:                           # expression is True
    print("x is greater than y")
    print("This is part of same block")
    print("This is part of same block")

print("this is not part of the if block")
```

## The else clause

So far we made a single `if` statement. What if we wanted a program to execute one statement if the expression evaluvates to `True` or execute an another statement if the expression evaluvates to `False`
In such scenarios we can use the `else` clause

```python

x = 5

if x > 10:                          # Expression evaluvates to False
    print("x is greater than 10")
else:
    print("x is less than 10")      # Expression evaluvates to True
>>> x is less than 10

```

In this example we see that the `if` condition evaluvates to False so the statement in the `else` block is executed.

A real world analogy to `if/else` statment is like this. If its sunny outside, then go to park. Otherwise read a book.

## The elif clause

Now that we have understood branching for sing `if` statement. What if we need to make several such alternatives in such cases we need to rely on `elif` clause. Which literally means else if. Here is an example using `elif`

```python
x = 20

if x < 5:
    print("x is less than 5")
elif x < 10:
    print("x is less than 10")
elif x < 30:
    print("x is less than 30")
else:
    print("None of the above")
>>> x is less than 30
```

Here first the code checks the first if condition. It finds that the expression evaluvates to `False`. Therfore it goes into `elif` statement and sees if the expression inside `elif` evaluvates to truth if not it will continue to go through each `elif` expressions. If none of them evaluvates to `True` then the conditional goes to the `else` clause and executes the statement in the `else` clause. Note that in the case where `elif` is used. The `else` clause can come only at the end of the expression.

## One line If statments

It is possible to write `if` conditionals in a single line. Example

```python
x = 5
if x == 5: print("Came inside the if statement "); print("x equals 5 ");print("came to this part");
>>> Came inside the if statement x equals 5 came to this part
```

Here all the statements seperated by semicolon is considered as part of single block. We can have more complicated ones like the example shown below.

```python
x = 10
if x == 10: print('x '); print(' contains '); print(' 10')
    elif x == 20: print('x '); print(' contains '); print(' 20')
    else: print('x '); print(' contains '); print(' something else')

```

Now though this is a valid python syntax its highly discourage as it makes the code more difficult to read and is not the recomended as per [PEP8][pep8-link] standards.

[pep8-link]: https://www.python.org/dev/peps/pep-0008/#other-recommendations

## Additional information

In the `if` clause python strictly doesn't need a `True/False` value. This will become evidenet with the following example.

```python

str = ""
str_1 = "Hello,World"

if (str):
    print(str)
elif (str_1):
    print(str_1)
else:
    print("All are empty")

>>> Hello,World
```

In this example we did not evaluvate if condition like this `if str == "":`. Even then its understood by python that if a string is empty evaluvate it to `False` and evaluvate to `True` otherwise.

Here is another on some pythonic expressions inside `if`

```python
x = ["A", "B", "C"]

if "A" in x:
    print("A is inside x")
>>> A is inside x
```
