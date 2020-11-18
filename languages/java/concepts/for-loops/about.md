## For loop

The for loop provides a mechanism to execute a group of statements repeatedly.

Syntax:

````java
for (initialization; test; update)
{
     // body
}
````

The loop consists of four parts.

The initialization sets an initial state for the loop.
Typically it sets a counter used in the test and update parts.
Example:

````java
int i=1;
````

The test expression tests if the loop should execute the body
and evalute the update expression.

If the test evaluates to true the body and the update expression will be executed.

If the expression evaluates to false the neither body nor update  will be executed and execution continues after the loop.
Example:

````java
i <= 10
````

After executing the loop body, the update expression increments/decrements the loop variable by some value.
Example:
````java
i++;
````
If you want more control over which values to iterate over, a `for` loop can be used:

```java
char[] vowels = {'a', 'e', 'i', 'o', 'u'}{};

for (int i = 0; i < 3; i++) {
    // Output the vowel
    System.out.print(vowels[i]);
}

// => aei
```

A `for` loop does have some advantages over a `foreach` loop:

- You can start or stop at the index you want.
- You can use any (boolean) termination condition you want.
- You can skip elements by customizing the incrementing of the loop variable.
- You can process collections from back to front by counting down.
- You can use `for` loops in scenarios that don't involve collections.

https://docs.oracle.com/javase/tutorial/java/nutsandbolts/for.html
