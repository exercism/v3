`string` is a datatype that you'll probably use often (_if not constantly_) in your Python programming life.  If you want to use those strings to display data to others in a clean, readable fashion, you'll want to build proficiency with _string formatting_. String formatting can also be very useful when storing data in a file, or preparing and parsing data as part of a data pipeline or for storage in a database. 

## Formatting a string

Today you are going to use two ways of formatting a string: `.format()` and `literal string interpolation` (_"f-strings"_)

### .format()

---

The `str.format()` method was introduced into Python in version `2.7`. This is why it offers great compatibility with other Python distributions.

```python
>> name = "Polly the Python"
>> "The kids love the snake: {}!".format(name)
"The kids love the snake: Polly the Python!"
```

You can use `.format()` after a string to replace `{}` with the given arguments.

```python
>> people_visited = 73
>> "The {} who came to visit '{}' were all excited.".format(people_visited, name)
"The 73 who came to visit 'Polly the Python' were all excited."
```

This also works with multiple arguments in strings.

```python
>> money = 1647
>> "Did you know that {name} brings in {income} dollars a day?".format(name=name, income=money)
"Did you know that Polly the Python brings in 1647 dollars a day?"
```

Adding references between the `{}` will allow you to assign objects to them.

### F-strings

---

F-strings are a newer way of string formatting as they were introduced into Python in version `3.6`.

```python
>> cost = 782
>> f"In reality {name} brings in {money-cost} dollars a day"
"In reality Polly the Python brings in 865 dollars a day"
```

You can see that you can also put in equations and such into references (this is the same with `.format()`).
