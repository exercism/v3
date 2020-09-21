_ternary operators_ can be thought of as being _if-else_ inline. It is a good example that most of the times you have different ways of achieving the same result.

So, how to decide between _if-else_ and _ternary_ ? Well, _Ternary_ is used in simple cenarios, where you just need to return a value based on a condition and no extra computation is needed, use _if-else_ for everthing else, like nested conditions, big expressions and when more than one line is needed to decide the return value.

And try to never nest _ternary operators_ as the expression becomes illegible, prefer nested if's to handle those cases

```java

// hard to read
int value = expr1 ? expr2 ? val1 : expr3 ? val2 : val3 : val4

// easier to read
if (expr1){
    if (expr2){
        return val1;
    } else{
        if (expr3) {
            return val2;
        } else {
            return val3;
        }
    }
} else {
    return val4;
}


```

For more examples check out [this][ternary-operator-first] and [this][ternary-operator-second] sources.

[ternary-operator-first]: https://www.programiz.com/java-programming/ternary-operator
[ternary-operator-second]: https://www.baeldung.com/java-ternary-operator
