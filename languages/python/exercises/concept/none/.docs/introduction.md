When you are beginning to code, you always assign a variable to a value. Say,

```python
a = 3
b = True
```

This declares the variable as well as defines them. But when you want to just declare the variable and not define them at start you use a special built in called as `None`.

If you have heard about `NULL` type in other languages then this must be familiar. `None` helps you to declare variables. These variables can be used later to be defined to a value.

```python
a = None
print(a)
#=> None
type(a)
#=> <class 'NoneType'>
```

In Python, `None` is a Singleton built-in which is used to declare a variable as well as can be used as `False`. Meaning if you want to check for a variable which is declared with `None`, It will be considered as `False`

```python
a = None
if a: #=> a will be considered as False when it interprets this line.
    print("This will not be printed")
```
