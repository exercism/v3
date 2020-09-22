The _ternary operator_ is a lightweight, compat alternative for simple _if/else_ statements. Its syntax is as follows:

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
