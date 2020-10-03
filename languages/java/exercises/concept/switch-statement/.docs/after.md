Starting with Java 14 (available as a preview before in Java 12 and 13) it is possible to use the "enhanced" switch implementation.

1. You have the possiblity to assign multiple value in a sigle case.
   Instead of :

   ```java
   switch (number) {
       case 1:
           //do stuff
           break;
       case 2:
           //do different stuff
           break;
       case 3:
           //do same stuff as case 1
           break;
       (...)
   }
   ```

   You can do :

   ```java
   switch (number) {
       case 1, 3:
           //do stuff
           break;
       case 2:
           //do other stuff
           break;
       (...)
   }
   ```

2. The `switch-statement` is now a `switch expression`. What is the difference ?
   Basicly a statement is expecting some strict logic where an expression can return a value.
   Instead of :

   ```java
   String result = "";
   switch (expression) {
       case "bob":
           result = "bob;
           break;
       (...)
   }
   ```

   You can do :

   ```java
   String result = switch(expression) {
       case "bob":
           yield "bob";
       (...)
   }
   ```

   The [`yield`][yield-keyword] works like a `return` except it's for switch expression. As `yield` terminates the expression `break` is not needed here.

3. An other main difference between the `switch statement` and the `switch expression` : in a `switch expression` you _**MUST**_ cover all the case in a `switch expression`. Either by mutliple `case` or using a `default` statement.

4. You can use `->` instead of `:`. The `->` allow you to not include the `break` keyword. Both notation can be used but in a switch you have to stick with only one.

   ```java
       switch(expression) {
           case 1 -> //do something
           case 2: //do something
           (...)
       }
   ```

   Won't work!

5. The scope. Traditionnals `switch` can lead to some unexected behavior because of it's scope as there is only one scope for the whole `switch`.

   ```java
       switch(expression) {
           case 1:
               String message = "something";
               break;
           case 2:
               String message = "anything";
               break;
           (...)
       }
   ```

   This exemple is not working because message is declared twice in the `switch`.
   It could be solved using :

   ```java
       switch(expression) {
           case 1: {
               String message = "something";
               break;
           }
           case 2: {
               String message = "anything";
               break;
           }
           (...)
       }
   ```

   As the `{}` is delimiting the scope of the `case`. However it's not intuitive because `{}` are not mandatory.
   However if you use the new `->` notation it must be followed by either : a single statement/expression, a `throw` statement or a `{}` block. No more confussion!

As a final note here is the resolution of the exericse using the enhanced `switch`.

```java
//enhanced switch implementation
public static String onField(int shirtNum){
    String playerDescription = switch(shirtNum) {
        case 1 -> "goalie";
        case 2 -> "left back";
        case 5 -> "right back";
        case 3, 4 -> "center back";
        case 6, 7, 8 -> "midfielder";
        case 9 -> "left wing";
        case 11 -> "right wing";
        case 10 -> "striker";
        default -> throw new IllegalArgumentException();
    };
    return playerDescription;
}
```

You can find more information on enhanced switch [here][switch1], [here][switch2] and on the [oracle documentation][oracle-doc].

[yield-keyword]: https://www.codejava.net/java-core/the-java-language/yield-keyword-in-java
[switch1]: https://www.vojtechruzicka.com/java-enhanced-switch/
[switch2]: https://howtodoinjava.com/java14/switch-expressions/
[oracle-doc]: https://docs.oracle.com/en/java/javase/13/language/switch-expressions.html
