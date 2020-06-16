In this exercise you will be building a simple integer calculator. To make matters simple, it is assumed the class can only handle positive numbers. 

The goal is to implement 4 different methods.

## 1. Implement method Calculate

The main method for implementation in this task will be the (_static_) `SimpleCalculator.Calculate()` method. It takes three arguments. The first two arguments are integer numbers on which an operation is going to be conducted. The third argument is of type string and for this excercise it is necessary to implement the following operations:

- addition using the `+` string
- multiplication using the `-` string
- division using the `/` string

Any other operation should throw the `InvalidOperationException` exception. This functionality and handling of operations should be contained in a `try` block.

## 2. Implement method SimpleCalculator.Addition()

Implement the (_static_) `SimpleCalculator.Addition()`, which takes two integer values as arguments and returns the result of their multiplication. If the result of the operation does not fit into the `int` type (is greater than `2_147_483_647`), then the method should throw an `ArgumentException` exception with the message `"Result of operation does not fit in type of int."`. 

## 3. Implement method SimpleCalculator.Multiplication()

Implement the (_static_) `SimpleCalculator.Multiplication()`, which takes two integer values as arguments and returns the result of their multiplication. If the result of the operation does not fit into the `int` type (is greater than `2_147_483_647`), then the method should throw an `ArgumentException` exception with the message `"Result of operation does not fit in type of int."`. 

## 4. Implement method SimpleCalculator.Division()

Implement the (_static_) `SimpleCalculator.Division()`, which takes two integer values as arguments and returns the result of their divison. The divisor must be different than `0`. If it is eqaul to zero, the method should throw `DivideByZeroException` exception.

## 5. Handle exceptions in the SimpleCalculator.Calculate() method

The `Exceptions` class contains the `ErrorLog` property, which will act, as the name suggest, as the logger for error messages that occur during the execution of the program. In the  `SimpleCalculator.Calculate()` method, implement the `catch` clause that catches `ArgumentException` exceptions. Inside the `catch` clause, the `ErrorLog` property should be modified to include the string `Result invalid: ` as well the content of the exception message. Finally, it should return `-1` as the result of the operation.
