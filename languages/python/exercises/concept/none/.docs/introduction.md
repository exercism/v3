In Python, `None` is frequently used to represent the absence of a value, it can also be used to define a `null` variable or an object.

If you have heard about `NULL` type in other languages then this must be familiar. `None` helps you to declare variables. These variables can be used later to be defined to a value. We can assign `None` to any variable.

```python
a = None
print(a)
#=> None
type(a)
#=> <class 'NoneType'>
```

Python returns `None` when you do not specify a return for a function, or if you just say `return`.

```python
def test_func():
    pass
test_func()
#=> returns None

def test_func2():
    return
#=> returns None
```

`None` evaluates to `False` when used in a conditional check.

```python
a = None
if a: #=> a will be evaluated to False when its used in a conditional check.
    print("This will not be printed")
```
