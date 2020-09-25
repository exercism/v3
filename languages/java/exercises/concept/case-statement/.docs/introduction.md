Wikipedia describes a `switch` statement as "a type of selection control mechanism used to allow the value of a variable or expression to change the control flow of program".

The mechanism involves the following keywords: `switch`, `case`, `break` and `default`.

At their simplest they test a primitive or string expression and make a decision based on its value. For example:

```java
String direction = getDirection();
switch (direction){
    case "left":
        goLeft();
        break;
    case "right":
        //if direction equals "right"
        goRight();
        break;
    default:
        //otherwise
        markTime();
        break;
}
```
