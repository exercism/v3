Like an _if/else_ statement, a `switch` statement allow you to change the flow of the program by conditionally executing code. The difference is that a `switch` statement can only compare the value of a primitive or string expression against pre-defined constant values.

Some keywords are usefull when using a switch statement.

- `switch` : this keyword allow you to declare the structure of the switch. It his follow by the expression or the variable that will make the result change ([more information][switch-statement]).
- `case` : you will use this one to declare the differents possibilties for the result ([more information][case]).
- `break` : the `break` keyword is very usefull in order to stop the execution of the switch at the end of the wanted flow. If you forget it, the program will continue and may lead to unexpected results ([more information][break]).
- `default` : as it's name says use it as a default result when no other case matchs your expression's result ([more information][default]).

At their simplest they test a primitive or string expression and make a decision based on its value. For example:

```java
String direction = getDirection();
switch (direction) {
    case "left":
        goLeft();
        break;
    case "right":
        goRight();
        break;
    default:
        //otherwise
        markTime();
        break;
}
```

[wikipedia]: https://en.wikipedia.org/wiki/Switch_statement
[switch-statement]: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/switch.html
[case]: https://www.w3schools.com/java/ref_keyword_case.asp
[break]: https://www.w3schools.com/java/ref_keyword_break.asp
[default]: https://www.w3schools.com/java/ref_keyword_default.asp
