`String formatting` in Python is something that you will be using very often, to display data to users or to save data to a file. We showed you 2 of the most common ways to format strings, but these are not the only ways to do this.

## The `%` operator (A.K.A. 'The old style')

The `%` operator comes from the C language and it allows you to do positional arguments in string easily.

```python
>> name = "Anna the Annaconda"
>> print("The snake's name is %s." % name)
"The snake's name is Anna the Annaconda."
```

In this example we used `%s` in the string, then after the string we added a `%` and the variable `name` after the string. This replaces `%s` with the `name` variable. If you want to add multiple variables to a string you put a [tuple](https://www.w3schools.com/python/python_tuples.asp) with your variables after the `%`.

Python supports a whole [format specification mini language](https://docs.python.org/3/library/string.html#format-specification-mini-language) that can be used to align text, apply effects, display currency and much more when formatting.

This method is now superseded by the `.format()` method, which is why we call it _'The old style'_, But if you want to learn how to use it then check out [this tutorial](https://www.learnpython.org/en/String_Formatting) by _learnpython.org_.

## Template strings

`Template` is a module from the `string` library, which is automatically included when you download Python. It is simpler and also less powerful than any of the previously mentioned methods, but sometimes that's just what you need to make your code look better.

```python
>> from string import Template

>> template_string = Template("The snake called `$snake_name` has escaped!")
>> print(template_string.substitute(snake_name=name))
The snake called `Anna the Annaconda` has escaped!
```

The official Python documentation has more information about `Template strings` [here](https://docs.python.org/3/library/string.html#template-strings).

## So when do you use these?

Because `.format()` and `f strings` will literally execute whatever you give them as an input, they can cause big security risks - especially if they depend on user input. This is why you should never use them if you don't fully trust the input given. Instead use `Template strings` -- they're much safer.

As explained earlier, the `%` is 'old style', it will still be supported (in many Python distros, not all of them), but is a little old and less powerful than other methods.

If you need help making a decision, then here is a decision tree to help you choose:

![Decision Tree](https://github.com/exercism/v3-files/blob/master/python/string-formatting/decision_tree.png?raw=true)
