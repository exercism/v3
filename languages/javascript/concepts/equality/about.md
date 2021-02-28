Javascript uses `==`, `===` and `Object.is()` to compare the equality in different instances

In general as conclusion

- If you know the type(s) in a comparison.If both of the types are same then `==` is identical to `===`.So prefering `==` will be shorter to write in such scenario
- If you don't know the type(s) in a comparison. It is preferable to use `===`.

Sometimes implicit coercion can be helpful.In such scenarion the understanding of coercion can be very helpful

You can find all the rules of the equality in [ECMA docs][ecma-docs] and learn more about coercion and equality from [You Don't know JS][ydkjs]

[ecma-docs]: https://262.ecma-international.org/10.0/index.html#sec-abstract-equality-comparison
[ydkjs]: https://github.com/getify/You-Dont-Know-JS/blob/1st-ed/types%20%26%20grammar/ch4.md
