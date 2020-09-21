Java have another operator to handle simple decision making, the _ternary operator_ looks like a _if-else_ in just one like, its sintax is very simple:

```java
int value = (expression) ? trueExpression : falseExpression;
```

A lot of simple _if-else_ expressions can be simplified using _ternary operators_

```java

//this if statement
if ( 5 > 4 ) {
    return true;
} else {
    return false;
}

//is equivalent to this ternary expression
return 5 > 4 ? true : false;

```
